module TexParser (
     Kind
   , Eval
   , functionDispatch
   , Record
   , parseDocument  
   , toTex
   , texHeader
   , onlyExternalize
) where

import Text.ParserCombinators.Parsec

-- a data type that specifies the kind of operation on contents of a tex command
data Operation = Environment String | Command1 String | Enclosed String | Paragraph 
   | Text | Link String | Empty 
   deriving (Show, Eq)

-- specifies how to handle the parsed pieces
data Eval = Header | Convert Operation | Externalize Operation | ExternalizeInline Operation
   deriving (Show, Eq)

-- specifies the integer that is necessary to keep track of what number the external files will have
type Record = Int

-- the composed type which will be used as the results of parsing a single logical statement
type Kind = ((Eval, Record) , String)



-- recursively apply the parseRules and keep track of the record until the end of the LaTeX document is reached
parseDocument rec =
   do
     first@((eval, newRec), str) <- parseRules rec
     next  <- endOfFile <|> parseDocument newRec
     return (first:next)

-- the end of the LaTeX file
endOfFile =
   do try (string "\\end{document}")
      return []


-- which parsing rules to apply. Note that the order is important
parseRules i = 
       try (header i)
   <|> try (environment i)
   <|> try (link i)
   <|> try (command1 i)
   <|> try (paragraphStart1 i)
   <|> try (paragraphStart2 i)
   <|> try (paragraphContinue i)
   <|> try (emptyLines i)
   <|> try (lineBreak i)
   <|> try (dollarMathEqn i)
   <|> try (inlineMath i)
   <|> try (comments i)


--------------------------------------------------
-- Parsing
--------------------------------------------------

-- the LaTeX header
header i =
   do string "\\documentclass"   -- begins with the documentclass statement
      manyTill anyChar (char '}') -- and some statements and arguments ending with '}'
      skipMany (oneOf " \n")    -- there might be some spaces or newline characters
      -- everything until \begin{document} is considered the header:
      arg <- manyTill anyChar (try (string ("\\begin{document}"))) -- Fix me: spaces?
      skipMany (oneOf " \n")  -- if there are some spaces or newlines, we will skip them
      return ((Header, i), arg)  -- save the header


-- an environment begins with \begin{...} and ends with \end{...}
environment i =
   do string "\\begin"  -- it's only an environment if it begins with \begin
      spaces  -- followd by possibly some spaces
      char '{'  -- and then the argument
      description <- manyTill anyChar (char '}') -- what kind of environment?
      -- parse anything until the \end{...} statement. NO NESTING SUPPORTED:
      arg <- manyTill anyChar (try (string ("\\end{" ++ description ++ "}")))
      many (oneOf " \n")  -- skip trailing whitespace and newlines
      -- for now all environments are externalized.
      -- the Record integer is increased by one:
      return ((Externalize (Environment description), i+1), arg)

-- except environments, all commands with one argument like \command{...} are handled here
command1 i =
   do char '\\'  -- always begins with a backslash
      description <- many1 (noneOf " {")  -- followed by the name of the commands
      spaces  -- possibly some whitespace
      char '{' -- argument of the command
      arg <- manyTill anyChar (char '}') -- argument ends with '}'
      return ((Convert (Command1 description), i), arg ) -- mark for conversion to HTML

-- a hyperlink
link i =
   do char '\\'  -- always starts with a \
      try (string "href") -- followed by the command href
      spaces -- possibly some whitespace
      char '{'  -- and the first argument
      url <- manyTill anyChar (char '}') -- that contains the URL
      spaces  -- followed by zero or more whitespace
      char '{' -- and the second argument
      name <- manyTill anyChar (char '}') -- that contains the name of the link
      return ( (Convert (Link url), i), name)  -- hyperlinks are converted to HTML


-- a paragraph that needs a <p> in the beginning
paragraphStart1 i =
   do char '\n'    -- if there is a newline
      spaces -- followed by zero or more whitespace
      char '\n'  -- and another newline, we know that anything that follows is a new paragraph
      many (oneOf " \n")  -- all other whitespace and newlines don't matter.
      arg <- many1 (noneOf stopChars)  -- anything that follows that is not a key character
      return ((Convert Paragraph,i) , arg) -- is a paragraph

-- another possibility for a paragraph is that it appears after some section statement
-- note that command1 has its trailing newline not removed while math environments have them removed
paragraphStart2 i =
   do char '\n'
      many (oneOf " ")
      arg <- many1 (noneOf stopChars)
      return ((Convert Paragraph,i) , arg)

-- if there are no newline characters, we can assume that the text is not meant to be a new paragraph
paragraphContinue i =
   do arg <- many1 (noneOf stopChars)
      return ((Convert Text,i) , arg)

-- all things starting with a % character are considered a comment and removed
comments i =
   do char '%'
      skipMany (noneOf "\n")
      return ((Convert Empty, i), [])

-- if for some reason, there are blank lines, we will remove them
emptyLines i =
   do char '\n'
      spaces
      char '\n'
      return ((Convert Empty,i) , [])

-- if all else fails and we have still lineBreaks, we will remove them
lineBreak i =
   do many1 (oneOf "\n")
      return ((Convert Empty,i) , [])

-- this is the typical math equation initiated and concluded by $$
dollarMathEqn i =
   do try (string "$$")
      arg <- manyTill anyChar (try (string "$$"))
      many (oneOf " ")
      char '\n'
      return ((Externalize (Enclosed "$$"), i+1), arg)


-- inline math initiated and concluded with $. Note that this needs to be distinguished from other Math environments as it needs a different CSS style
inlineMath i =
   do char '$'
      arg <- manyTill anyChar (char '$')
      return ((ExternalizeInline (Enclosed "$"), i+1), arg)

-- characters that should trigger to stop parsing text.
stopChars = "$%\n\\"

--------------------------------------------------


--this function defines how our internal represantation as a Kind data type is translated to HTML.
functionDispatch :: Kind -> String
-- the TeX header should be completely omitted
functionDispatch ( (Header, _) , _ ) = ""
-- paragraphs should be initiated by a <p> 
functionDispatch ( (Convert Paragraph, _) , arg ) = "<p>" ++ arg ++ "\n"
-- continued text (no new paragraph) should be taken verbatim 
functionDispatch ( (Convert Text, _) , arg ) = arg ++ "\n"
-- parsed content that contains no information
functionDispatch ( (Convert Empty, _) , arg ) = "" 
-- all commands with one argument are translated to the <div> tags with the class attribute being the command name
functionDispatch ( (Convert (Command1 str), _) , arg ) = htmlEnv "div" ("class=\"" ++ str ++ "\"") arg
-- all hyperlinks are translated to <a> tags with class attribute "textlink"
functionDispatch ( (Convert (Link str), _) , arg ) = htmlEnv "a" ("class=\"textlink\" href=\"" ++ str ++ "\"") arg
-- all externalized equations are included with the <img> tag with class "eqn". Note that we need to refer to the correct file by assuming that it is named i.svg where i is the record of the externalized equation
functionDispatch ( (Externalize _ , i), arg) = (htmlImg "eqn" ((show i) ++ ".svg")) ++ "\n"
-- inlined math is handled the same as equation environments but with class attribute "eqninline" for separate CSS style modifications
functionDispatch ( (ExternalizeInline _ , i), arg) = htmlImg "eqninline" ((show i) ++ ".svg")
-- if all fails, it fails
functionDispatch _ = error "functionDispatch: failed to find suitable function"


-- This function wraps a statement that needs to be externalized into a complete tex file with standalone documentclass
toTex :: String -> String -> Kind -> String
-- environments need to be rewrapped into \begin{...} and \end{...} statements
toTex document_commands xs ((Externalize (Environment str), _), arg) = xs ++ document document_commands (env str arg)
-- enclosed math environments like $$ need to be rewrapped as well
toTex document_commands xs ((Externalize (Enclosed str), _), arg) = xs ++ document document_commands (enclose str arg)
-- inline math environments need to be wrapped into $ symbols and we need to add invisible \bigl. and \bigr. statements to make vertical alignment possible. The SVG files contain no information on the baseline of the font, thus we need to somewhat ensure that the height of all inline math is the same
toTex document_commands xs ((ExternalizeInline (Enclosed str), _), arg) = xs ++ document document_commands (enclose str ("\\bigl." ++ arg ++ "\\bigr.") )
-- if all fails, it fails
toTex _ _ _ = error "toTex: didn't define the operation yet"


-- builds a string for an HTML statement embraced into tags
htmlEnv ::  String -> String -> String -> String
htmlEnv tag options content = 
   "<" ++ tag ++ " " ++  options ++ ">" ++ content ++ "</" ++ tag ++ ">\n"

-- creates a string denoting an HTML img
htmlImg :: String -> String -> String
htmlImg c src = "<img class=\"" ++ c ++ "\" src=\"" ++ src ++ "\"/>"

-- creates a LaTeX environment 
env name content = "\\begin{" ++ name ++ "}" ++ content ++ "\\end{" ++ name ++ "}\n"
enclose name content = name ++ content ++ name ++ "\n"
document document_commands content = "\\begin{document}\n" ++ document_commands ++ content ++ "\\end{document}"


-- extracts the Header from a list of parsed content and appends it to a another documentclass statement
texHeader :: String -> [Kind] -> String
texHeader header ( ((eval, rec), arg) :xs) 
   | eval == Header = header ++ arg
   | otherwise      = error "No header found in .tex file"


-- filters all parsed content that needs to be externalized
onlyExternalize :: [Kind] -> [Kind]
onlyExternalize [] = []
onlyExternalize (first@((Externalize str, rec),arg):xs) =  first: onlyExternalize xs
onlyExternalize (first@((ExternalizeInline str, rec),arg):xs) =  first: onlyExternalize xs
onlyExternalize (_:xs) =   onlyExternalize xs
