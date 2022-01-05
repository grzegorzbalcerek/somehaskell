module FileReadWrite where

import System.Directory
import Data.List (dropWhileEnd)
import System.IO
import System.Exit
import Text.Parsec

input2output :: String -> String -> (String -> Either ParseError String) -> IO ()
input2output inputPath outputPath action =
  withFileRead inputPath $ \inputContent -> do
    case (action inputContent) of
      Right outputContent -> do
        putStrLn $ "" ++ inputPath ++ " (" ++ show (length inputContent) ++ ") -> " ++ outputPath ++ " (" ++ show (length outputContent) ++ ")"
        safeWriteToFile outputPath outputContent
      Left error ->
        do putStrLn $ show error
           exitFailure

withFileRead :: String -> (String -> IO ()) -> IO ()
withFileRead path action = do
  hinput <- openFile path ReadMode
  hSetEncoding hinput utf8
  input <- hGetContents hinput
  action input
  hClose hinput

safeWriteToFile :: String -> String -> IO ()
safeWriteToFile path content = do
  let dir = dropWhileEnd (/= '/') path
  if dir /= "" then createDirectoryIfMissing True dir else return ()
  houtput <- openFile path WriteMode
  hSetEncoding houtput utf8
  hPutStr houtput content
  hClose houtput
