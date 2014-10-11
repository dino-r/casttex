import Text.ParserCombinators.Parsec
import Control.Monad
import System.Cmd
import System.Directory
import System.Environment
import TexParser
import StyleSvg
import Config

main =
   do args <- getArgs -- get the list of command line arguments
      let configFilePath = case args of      
                              [] -> "config"  -- default is ./config
                              xs -> xs !! 0   -- set the config file path
      -- read the IO part of the config:
      [inputDir, texFileName, targetDir, htmlFileName] <- readConfigIO configFilePath
      -- read tex-document contents:
      contents <- readFile (inputDir ++ texFileName)
      let parsed =  parseTex contents    -- parse contents into a list
      let ext = onlyExternalize parsed   -- get a list of the ones to externalize to SVGs
      let numberOfFiles = length ext     -- how many SVGs are there?
      -- read the SVG part of the config:
      [standalone_tex_header, document_commands] <- readConfigSVG configFilePath
      -- write the externalized tex files to the target directory:
      externalize targetDir (texHeader standalone_tex_header parsed) document_commands tex ext
      -- compile the externalized tex files to PDFs:
      mapM_ system (commandList "" ("pdflatex -output-directory " ++ targetDir) tex numberOfFiles)
      -- convert PDFs to SVGs:
      mapM_ system (commandList2 targetDir "pdf2svg" pdf svgTmp numberOfFiles)
      -- read the contents of the generated SVG files:
      svgContents <- mapM readFile (listOfFiles targetDir svgTmp numberOfFiles)
      -- translate the parsed tex file to HTML and write it to the target directory:
      writeFile (targetDir ++ htmlFileName) (toHTML functionDispatch parsed) 
      -- remove SVG styles and enable CSS like styling (still empty):
      multiWrite (listOfFiles targetDir svg numberOfFiles) (map (cssStyleSVG "") svgContents)
      -- remove all files that are not necessary:
      mapM_ removeFile (listOfFiles targetDir tex numberOfFiles)
      mapM_ removeFile (listOfFiles targetDir ".log" numberOfFiles)
      mapM_ removeFile (listOfFiles targetDir ".aux" numberOfFiles)
      mapM_ removeFile (listOfFiles targetDir ".out" numberOfFiles)
      mapM_ removeFile (listOfFiles targetDir svgTmp numberOfFiles)
      mapM_ removeFile (listOfFiles targetDir pdf numberOfFiles)
      

-- suffixes
svgTmp = ".tmp.svg"
svg = ".svg"
pdf = ".pdf"
tex = ".tex"


-- command to compile to pdf:
texCommand = "pdflatex"
-- command to translate to svg:
svgCommand = "pdf2svg"

-- uses the parseDocument function from TexParser and removes the Either 
parseTex :: String -> [Kind]
parseTex input = case parse (parseDocument (0 :: Int) )  "(unknown)" input of
                    Left str -> []
                    Right xs -> xs


-- writes multiple files
multiWrite :: [String] -> [String] -> IO ()
multiWrite [] _ = return ()
multiWrite _ [] = return ()
multiWrite (filename:rest) (x:xs) = 
   do
      writeFile filename x                     
      multiWrite rest xs

-- takes a path to a directory, a command, a suffix and the total number of files
-- repeats the command for each file, e.g.
-- $ pdflatex path/1.tex
-- $ pdflatex path/2.tex
commandList :: String -> String -> String -> Int -> [String]
commandList path command arg_suffix n = 
   zipWith ( (++) . (++ " ") ) (repeat command) (listOfFiles path arg_suffix n)

-- same as commandList but takes to lists of arguments as in
-- pdf2svg path/1.tex path/1.svg
-- pdf2svg path/2.tex path/2.svg
commandList2 :: String -> String -> String -> String -> Int -> [String]
commandList2 path command arg1_suffix arg2_suffix n = 
   zipWith ( (++) . (++ " ") ) (commandList path command arg1_suffix n) (listOfFiles path arg2_suffix n)

-- from a path, a suffix and the total number of files, a list is created as
-- [path/1.tex, path/2.tex, ..., path/n.tex]
listOfFiles :: String -> String -> Int -> [String]
listOfFiles path str n = zipWith (++) (repeat path) (zipWith ((++).show) [1..n] (repeat str))


-- translates all parsed input that needs to be externalized to standalone tex documents
externalize :: String -> String -> String -> String -> [Kind] -> IO ()
externalize path header document_commands suffix input =
   do
      let files = map (toTex document_commands header) input
      multiWrite (listOfFiles path suffix (length files)) files

-- takes a dispatch function that translates all parsed input to HTML tags
toHTML :: (Kind -> String) -> [Kind] -> String
toHTML dispatch [] = ""
toHTML dispatch (x:xs) = (dispatch x) ++ (toHTML dispatch xs)

