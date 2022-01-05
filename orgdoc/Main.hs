-- -*- coding: utf-8; -*-
module Main where

{-
ghc Main.hs -o orgdoc && cp orgdoc ~/bin
-}

import System.Environment
import Model
import Parse
import Render
import Eval
import Docs
import CopyProps
import System.IO
import GHC.IO.Encoding
import Control.Monad.Trans.State
import Control.Monad.Reader
import Data.List
import qualified Data.Map as Map
import System.Directory

main = do
  args <- System.Environment.getArgs
  mainWithArgs args

mainWithArgs ["parse",path] =           parseCommand path
mainWithArgs ["props",path] =           propsCommand path
mainWithArgs ["eval",path] =            evalCommand Map.empty path
mainWithArgs [path] =                   renderCommand Map.empty path
mainWithArgs _ =                        putStrLn "Input arguments not recognized. Nothing to do."

parseCommand path = processFile path $ \input -> do
  let parsed = parseInput input
  putStrLn $ showElements 0 parsed

propsCommand path = processFile path $ \input -> do
  let parsed = parseInput input
  let copied = copyProps parsed
  putStrLn $ showElements 0 copied

evalCommand env path = processFile path $ \input -> do
  evaluated <- string2elements env Map.empty input
  let content = copyProps evaluated
  putStrLn $ showElements 0 content

renderCommand env path = processFile path $ \input -> do
  evaluated <- string2elements env Map.empty input
  let content = copyProps evaluated
  let rawDocs = makeDocs content
  let docs = Map.map (\es -> concat (map renderElement es)) rawDocs
  forM_ (Map.toList docs) $ \(file,content) ->
    if file == ""
    then putStr content
    else do
      houtput <- safeOpenFileForWriting file
      putStrLn $ "Generating " ++ file ++ ". Length: " ++ show (length content) ++ "."
      hPutStr houtput content
      hClose houtput

processFile :: String -> (String -> IO ()) -> IO ()
processFile path action = do
  hinput <- openFile path ReadMode
  hSetEncoding hinput utf8
  input <- hGetContents hinput
  action input
  hClose hinput

safeOpenFileForWriting path = do
  let dir = dropWhileEnd (/= '/') path
  if dir /= "" then createDirectoryIfMissing True dir else return ()
  houtput <- openFile path WriteMode
  hSetEncoding houtput utf8
  return houtput

showElements :: Int
             -> [Element]
             -> String

showElements n (e@(Apply name props subelements):es) =
  (take (2*n) $ repeat ' ') ++ show (Apply name props []) ++ "\n" ++
  showElements (n+1) subelements ++
  showElements n es

showElements n (e@(Def name subelements):es) =
  (take (2*n) $ repeat ' ') ++ show (Def name []) ++ "\n" ++
  showElements (n+1) subelements ++
  showElements n es

showElements n (e:es) =
  (take (2*n) $ repeat ' ') ++ show e ++ "\n" ++
  showElements n es

showElements _ []= ""
