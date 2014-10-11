module StyleSvg (
   cssStyleSVG
) where


import Text.ParserCombinators.Parsec

data Kind = Def |  Other deriving (Eq, Show)


-- takes a string with a CSS like style and the contents of an SVG file.
-- returns the contents of an SVG without any style statements within the <g> tags and with a style defined by the CSS like style input embraced in <style> tags at the beginning of the document
cssStyleSVG :: String -> String -> String
-- applies parseRules many times
cssStyleSVG style input = case parse (many parseRules) "(unknown)" input of
                             -- error
                             Left _   -> []  
                             -- if successful, the resulting parsed list is folded to the
                             -- resulting string
                             Right xs -> foldl (svgFold style) "" xs 

-- this is the fold function that takes a style string, and a list of parsed input and inserts the style at the proper location.
svgFold :: String -> String -> (Kind, String) -> String
svgFold sty str (Def, arg) = str ++ (cssStyle sty) ++ arg
svgFold sty str (Other, arg) = str ++ arg

cssStyle str = "<style>\n" ++ str ++ "</style>\n"


-- rules of how to parse the SVG document
parseRules =
       try defs
   <|> try style
   <|> try gStyles
   <|> try other

-- we need the <defs> statement of the SVG file, since we want to insert the style before that
defs =
   do definition <- try (string "<defs>")
      return (Def, definition)

-- if there's already some CSS like style included, we remove it
style = 
   do try (string "<style>")
      manyTill anyChar (try (string "</style>"))
      skipMany (oneOf " \n")
      return (Other, "")

-- if there is a style statement within the <g> tags, we remove it since it overrides the CSS like style
gStyles =
   do try (string "<g style=\"")  -- this is not generic enough!
      many (noneOf "\"")
      char '\"'
      return (Other, "<g")

-- everything else within the document, we can just take verbatim
-- but we need to ensure that we don't parse over statements that we wanted to catch with other parsing rules
other =
   do initial <- many (noneOf "<")
      comchar <- char '<' 
      following <- try (many1 (noneOf "<"))
      return (Other, initial ++ (comchar:following) )
