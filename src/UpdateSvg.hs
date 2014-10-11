import System.IO
import System.Directory
import System.Environment
import StyleSvg

main =
   do 
      -- extract css file path and svg files from arguments
      (svgCssFilePath: svgFiles) <- getArgs
      -- read svgFile
      svgStyle <- readFile svgCssFilePath
      putStrLn $ "CSS File: " ++ svgCssFilePath
      putStr $ "SVG Files: " 
      mapM_ putStr (zipWith (++) svgFiles (repeat "  "))
      putStrLn ""
      -- update SVG files with the svg Style from the SVG file
      mapM_ (updateSVG svgStyle) svgFiles

-- takes a string with an css style and a path to a SVG file and updates the svg file
updateSVG :: String -> String -> IO ()
updateSVG svgStyle fileName =
   do
      -- file handle for svg file
      handle <- openFile fileName ReadMode 
      -- get contents of svg file
      svgContents <- hGetContents handle
      -- open temporary file to store updated results
      (tempName, tempHandle) <- openTempFile "." "temp"
      -- put the updated SVG file into the temporary file
      hPutStr tempHandle $ cssStyleSVG svgStyle svgContents
      hClose handle
      hClose tempHandle
      -- remove the old svg file
      removeFile fileName
      -- rename the temporary svg file to have the same name as the initial svg file
      renameFile tempName fileName

