module MainLogic where

import System.Environment
import FileReadWrite
import Model
import RenderModel
import Data.List
import Parse

mainLogic :: (EDoc -> String) -> String -> IO ()
mainLogic renderer suffix = do
  args <- System.Environment.getArgs
  mainWithArgs renderer suffix args

mainWithArgs :: (EDoc -> String) -> String -> [String] -> IO ()
mainWithArgs renderer suffix [inputPath, outputDir] =
  let basename = takeWhile (/='.') . reverse . takeWhile (/='/') . reverse $ inputPath
      prefix = filter (/='/') . reverse . dropWhile (/='/') . reverse $ inputPath
      outputPath = outputDir ++ "/" ++ prefix ++ basename ++ suffix
  in input2output inputPath outputPath $ fmap (renderer . remodel) . (parseEnrichedInput inputPath)
mainWithArgs _ _ _                      = putStrLn "Input arguments not recognized. Nothing to do."
