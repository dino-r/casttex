module Config (
   readConfigIO
,  readConfigSVG
) where

import Data.ConfigFile
import Control.Monad.Except

-- reads the IO section from a config file
readConfigIO :: String -> IO ([String])
readConfigIO configFilePath = do
   rv <- runExceptT $
              do
              cp <- join $ liftIO $ readfile emptyCP configFilePath
              -- specifies the directory where the latex file is located.
              -- needs to end with a '/', i.e. path/to/dir/ not path/to/dir
              input_directory <- get cp sectionIO "input_directory"
              -- file name e.g. texfile.tex
              input_file_name <- get cp sectionIO "input_file_name"
              -- the directory where the SVG files and the resulting html file should be located
              output_directory <- get cp sectionIO "output_directory"
              -- the output html file name. e.g. htmlfile.html
              output_file_name <- get cp sectionIO "output_file_name"
              return [input_directory, input_file_name, output_directory, output_file_name]
   case rv of
      Left  xs -> do print xs
                     return []
      Right xs -> return xs

sectionIO = "IO"

-- reads the SVG section of the config file
readConfigSVG :: String -> IO ([String])
readConfigSVG configFilePath = do
   rv <- runExceptT $
              do
              cp <- join $ liftIO $ readfile emptyCP configFilePath
              -- the documentclass and possibly some package statements that are not
              -- included in the Header of the original tex file
              standalone_tex_header <- get cp sectionSVG "standalone_tex_header"
              -- all statements that should appear after the \begin{document} statement
              document_commands <- get cp sectionSVG "document_commands"
              return [standalone_tex_header, document_commands]
   case rv of
      Left  xs -> do print xs
                     return []
      Right xs -> return xs

sectionSVG = "SVG"
