-- -*- coding: utf-8; -*-
module Eval where

import Data.List (find,groupBy,intersect)
import Model
import Parse
import Render
import CopyProps
import Control.Monad.Reader
import qualified Data.Map as Map
import Debug.Trace
import System.IO
import GHC.IO.Encoding
import Data.Maybe

string2elements :: Map.Map String [Element] -> Map.Map String String -> String -> IO [Element]
string2elements env props inputstr = string2elements' env props inputstr []

string2elements' :: Map.Map String [Element] -> Map.Map String String -> String -> [Element] -> IO [Element]
string2elements' env props input elements = do
  let parsed = parseInput input
  let copied = copyProps parsed
  evaluate env props (copied ++ elements)

evaluate :: Map.Map String [Element]  -- środowisko (definicje: String -> [Element])
         -> Map.Map String String     -- akumulowane props z wyższych elementów, propagowane niżej, początkowo puste
         -> [Element]                 -- ewaluowane elementy
         -> IO [Element]

-- dopisz def do środowiska i ewaluuj resztę
-- def nie ma właściwości, więc props bez zmian
evaluate env props ((Def name elements):es) =
  evaluate (Map.insert name elements env) props es

-- jeśli napotkano instrukcję importu
-- wczytaj i ewaluuj zawartość pliku
evaluate env props ((Import path):es) = do
  let thepath = evalString props path
  hinput <- openFile thepath ReadMode
  hSetEncoding hinput utf8
  input <- hGetContents hinput
  putStrLn $ "Importing " ++ thepath ++ ": " ++ (show (length input))
  result <- string2elements' env props input es
  hClose hinput
  return result

-- jeśli napotkano doc ewaluuj podelementy i łącz props
evaluate env props ((Doc eprops subelements):es) = do
  evaluatedTail <- evaluate env props es
  evaluatedSubelements <- evaluate env (Map.union eprops props) subelements
  let newProps = evalProps $ Map.union eprops props
  return $ (Doc newProps evaluatedSubelements) : evaluatedTail

-- jeśli napotkano element to mamy 2 przypadki:
evaluate env props ((Apply name eprops subelements):es) =
  case (Map.lookup name env) of

    -- przypadek 1: środowisko zawiera definicję z nazwą równą nazwie elementu
    -- zastąp element definicją, podstawiając argumenty
    Just defBody -> do
      let bodyWithArgsApplied = defBody >>= applyArguments (Map.union eprops props) subelements 
      evaluatedBody <- evaluate env (Map.union eprops props) bodyWithArgsApplied
      evaluatedTail <- evaluate env props es
      return $ evaluatedBody ++ evaluatedTail

    -- przypadek 2: środowisko nie ma elementu: raportuj błąd
    Nothing -> do
      error $ "Undefined element "++name

-- jeśli napotkano eval tekst
evaluate env props ((EvalText eprops txt):es) = do
  evaluatedTail <- evaluate env props es
  let newProps = evalProps $ Map.union eprops props
  let newText = (Text newProps [] (evalString newProps txt))
  evaluatedNewText <- evaluate env newProps [newText]
  return $ evaluatedNewText ++ evaluatedTail

-- jeśli napotkano tekst
evaluate env props ((Text eprops rules txt):es) = do
  evaluatedTail <- evaluate env props es
  let newProps = evalProps $ Map.union eprops props
  let newRules = maybe rules id $ Map.lookup (stringProp "textrules" newProps) env
  evalulatedNewRules <- evaluate env props newRules
  return $ (Text newProps evalulatedNewRules txt) : evaluatedTail

-- jeśli napotkano inny element
evaluate env props (e:es) = do
  evaluatedTail <- evaluate env props es
  return $ e : evaluatedTail

-- zakończ na końcu listy
evaluate _ _ [] = return []

applyArguments :: Map.Map String String    -- akumulowane props z wyższych elementów
               -> [Element]                -- argumenty
               -> Element                  -- fragment definicji, do którego stosujemy argumenty
               -> [Element]                -- fragment definicji w którym parametry zastąpiono argumentami

-- doc: aplikuj argumenty do podelementów
applyArguments props args (Doc eprops subelements) =
  [Doc eprops (subelements >>= applyArguments props args)]

-- element: aplikuj argumenty do podelementów
applyArguments props args (Apply name eprops subelements) =
  [Apply name eprops (subelements >>= applyArguments props args)]

-- IfEq: zbadaj czy w props jest taka wartość i jeśli tak
-- zwróć przetworzone elementy
-- w przeciwnym razie zwróć listę pustą
applyArguments props args (IfEq name value elements) =
  case (Map.lookup name props) of
    Just actualValue | actualValue == value ->
      elements >>= applyArguments props args
    _ -> []

-- IfDef: zbadaj czy w props jest taka wartość zdefiniowana
-- zwróć przetworzone elementy
-- w przeciwnym razie zwróć listę pustą
applyArguments props args (IfDef name elements) =
  if hasProp name props
  then elements >>= applyArguments props args
  else []

-- IfUndef: odwrotność IfDef
applyArguments props args (IfUndef name elements) =
  if hasProp name props
  then []
  else elements >>= applyArguments props args

-- jeśli ciało zawiera Args
-- skopiuj argumenty ale dodając do nich własności
applyArguments props args (Args argsprops) = map (mergeProps (Map.union argsprops props)) args

-- każdy inny element pozostaw bez zmian
applyArguments args env e = [e]

mergeProps props (Doc eprops elements) = Doc (Map.union eprops props) elements
mergeProps props (Apply name eprops elements) = Apply name (Map.union eprops props) elements
mergeProps props (Text eprops rules elements) = Text (Map.union eprops props) rules elements
mergeProps props e = e

----------------------------------------------------

evalProps :: Map.Map String String -> Map.Map String String
evalProps props = Map.map (evalString props) props
