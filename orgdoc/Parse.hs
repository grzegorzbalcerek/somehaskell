-- -*- coding: utf-8; -*-
module Parse where

import Text.Parsec
import Data.List (dropWhileEnd)
import Control.Applicative ((<$>))
import Model
import Data.String (words)
import Data.Char
import qualified Data.Map as Map
import Debug.Trace
import Data.Maybe (maybe)

type P = Parsec String ()

parseInput :: String -> [Element]
parseInput input =
    case (parse elements "error" input) of
      Right result -> result
      Left err -> error (show err)

----------------------------------------------------

elements :: P [Element]
elements = do
  optional propLine
  results <- many (try (many1 (try $ singleElement 1)) <|> try (many1 emptyOrRegularLineWithEol >> return []))
  try eof
  return $ concat results

singleElement :: Int -> P Element
singleElement n =
  (
  try (levelTag n "IFDEF" >> IfDef <$> (takeWord <* restOfLine) <*> nextLevelElements n) <|>
  try (levelTag n "IFUNDEF" >> IfUndef <$> (takeWord <* restOfLine) <*> nextLevelElements n) <|>
  try (levelTag n "IFEQ" >> IfEq <$> takeWord <*> (takeWord <* restOfLine) <*> nextLevelElements n) <|>
  try (levelTag n "ARGS" >> Args <$> properties) <|>
  try (levelTag n "DEF" >> Def <$> takeWordIgnoreUntilEol <*> nextLevelElements n) <|>
  try (levelTag n "DOC" >> Doc <$> properties <*> nextLevelElements n) <|>
  try (levelTag n "COMMENT" >> Comment <$> properties <*> nextLevelElements n) <|>
  try (levelTag n "BUILT-IN TEXT RULE" >> BuiltInTextRule <$> takeWord <*> restOfLine) <|>
  try (charReplaceTextRule n) <|>
  try (stringReplaceTextRule n) <|>
  try (charPairTextRule n) <|>
  try (text n) <|>
  try (evaltext n) <|>
  try (table n) <|>
  try (levelTag n "IMPORT" >> Import <$> takeWordIgnoreUntilEol) <|>
  try (beginLevel n >> Apply <$> takeWord <*> properties <*> nextLevelElements n) <|>
  try implicitText
  ) <?> "singleElement"

----------------------------------------------------

charReplaceTextRule n =
  let replaceCharRule = do
        c <- ( space >> char '*' ) <|> noneOf " *\n\r"
        r <- restOfLine
        return (c,r)
  in do
      levelTag n "CHAR REPLACE TEXT RULE" >> restOfLine
      rules <- many (try replaceCharRule)
      return $ CharReplaceTextRule (Map.fromList rules)

stringReplaceTextRule n =
  let stringPairRule separator = do
        c <- noneOf ['*']
        a <- many (noneOf [separator])
        char separator
        b <- restOfLine
        return (c:a,b)
  in do
      levelTag n "STRING REPLACE TEXT RULE"
      parsedSeparator <- restOfLine
      let trimmedSeparator = trim parsedSeparator
      let separator = if trimmedSeparator == "" then ' ' else head trimmedSeparator
      rules <- many (try (stringPairRule separator))
      return $ StringReplaceTextRule (Map.fromList rules)

charPairTextRule n =
  let charPairRule separator = do
        c <- ( space >> char '*' ) <|> noneOf " *\n\r"
        b <- many (noneOf [separator])
        char separator
        e <- restOfLine
        return (c,(b,e))
  in do
      levelTag n "CHAR PAIR TEXT RULE"
      parsedSeparator <- restOfLine
      let trimmedSeparator = trim parsedSeparator
      let separator = if trimmedSeparator == "" then ' ' else head trimmedSeparator
      rules <- many (try (charPairRule separator))
      return $ CharPairTextRule (Map.fromList rules)

implicitText = do
  content <- many1 emptyOrRegularLineWithEol
  return $ Text Map.empty [] (concat content)

text n = do
  levelTag n "TEXT"
  txt <- takeTextUntilColon
  props <- properties
  content <- many emptyOrRegularLineWithEol
  return $ Text props [] (txt ++ (concat content))

evaltext n = do
  levelTag n "EVALTEXT"
  txt <- takeTextUntilColon
  props <- properties
  content <- many emptyOrRegularLineWithEol
  return $ EvalText props (txt ++ (concat content))

table n = do
  levelTag n "TABLE" 
  props <- properties
  rows <- many (try tableRow)
  return $ Table props rows

-------------------------------------------------------

tableRow =
  (
  try (tableRowHline) <|>
  try (tableRowRegular)
  ) <?> "tableRow"

tableRowHline = do
  char '|'
  char '-'
  many (noneOf "\n\r")
  eol
  return $ HLine

tableRowRegular = do
  char '|'
  cells <- many (try tableCell)
  eol
  return $ RegularRow cells

tableCell = do
  content <- many (noneOf "|\n\r")
  char '|'
  return (trim content)

----------------------------------------------

evalString :: Map.Map String String -> String -> String
evalString env p =
    case (parse (oneprop env) "error" p) of
      Right result -> result
      Left err -> p

oneprop :: Map.Map String String -> P String
oneprop env = do
  results <- many1 (try $ onepropPart env)
  return $ concat results

onepropPart :: Map.Map String String -> P String
onepropPart props =
  (
  try (onepropSimpleDollarProp props) <|>
  try (many1 (noneOf "$"))
  ) <?> "onepropPart"

onepropSimpleDollarProp :: Map.Map String String -> P String
onepropSimpleDollarProp env = do
  char '$'
  name <- many1 (oneOf lettersAndDigits)
  return $ maybe "" id $ Map.lookup name env

lettersAndDigits = ['a'..'z']++['A'..'Z']++['0'..'9']

-----------------------------------------------------

singleColonProp =
  try colonPropWithValue <|>
  try colonPropEmpty

colonPropEmpty = do
  char ':'
  name <- many (noneOf " :\n\r")
  return $ Map.singleton name ""

colonPropWithValue = do
  char ':'
  name <- many (noneOf " :\n\r")
  char ' '
  value <- many (noneOf ":\n\r")
  return $ Map.singleton name (map (\c -> if c == '÷' then ':' else c) $ trim value)

----------------------------------------------

beginLevel :: Int -> P ()
beginLevel n = do
  try $ string $ take n $ repeat '*'
  space
  return ()

levelTag :: Int -> String -> P ()
levelTag n tag = do
  beginLevel n
  try (string tag)
  return ()  

properties :: P (Map.Map String String)
properties = do
  value <- many (noneOf ":\n\r")
  props <- singleColonProp `sepBy` (many (noneOf ":\n\r"))
  let mergedProps = foldl Map.union (Map.singleton "value" (trim value)) props
  restOfLine
  return mergedProps

takeWordIgnoreUntilEol :: P String
takeWordIgnoreUntilEol = do
  many space
  content <- many (noneOf " \n\r")
  many (noneOf "\n\r")
  eol
  return $ trim content

takeWord :: P String
takeWord = do
  many space
  content <- many (noneOf " :\n\r")
  return $ trim content

takeTextUntilColon :: P String
takeTextUntilColon = do
  content <- many (noneOf ":\n\r")
  return $ drop 1 content

----------------------------------------------------

propLine = string "# -*-" >> restOfLine

restOfLine = do
  content <- many (noneOf "\n\r")
  eol
  return content

eol = try (string "\r\n") <|> string "\n"

emptyOrRegularLineWithEol =
  eol <|> regularLineWithEol

regularLineWithEol = do
  h <- noneOf "*\n\r"
  content <- restOfLine
  return (h:content ++ "\n")

trim = dropWhile isSpace . dropWhileEnd isSpace

nextLevelElements n = many (singleElement $ n+1)


----------------------------------------------------

