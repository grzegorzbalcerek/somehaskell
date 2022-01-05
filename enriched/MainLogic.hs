module MainLogic where

import System.Environment
import FileReadWrite
import Model
import RenderModel
import Parse

mainLogic :: (EDoc -> String) -> IO ()
mainLogic renderer = do
  args <- System.Environment.getArgs
  mainWithArgs renderer args

mainWithArgs renderer [inputPath , outputPath] = input2output inputPath outputPath $ fmap renderer . (parseEnrichedInput inputPath)
mainWithArgs _ _                        = putStrLn "Input arguments not recognized. Nothing to do."
