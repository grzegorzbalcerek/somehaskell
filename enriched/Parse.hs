module Parse where

import Text.Parsec
import Data.List
import Control.Applicative ((<$>))
import Model
import Data.String (words)
import Data.Char
import Control.Monad
import qualified Data.Map as Map
import Debug.Trace
import Data.Maybe (maybe)

type P = Parsec String ()

parseEnrichedInput :: String -> String -> Either ParseError EDoc
parseEnrichedInput inputPath input = runParser eDoc () inputPath input

eDoc :: P EDoc
eDoc = eDocHeader *> many (eSection <|> eSegment 0) <* eof

eDocHeader :: P ()
eDocHeader = string "Content-Type: text/enriched" >> endOfLine >> string "Text-Width: 70" >> endOfLine >> endOfLine >> return ()

eSection :: P ESegment
eSection = do
  (visibility, title) <- eSectionHeader
  segments <- many1 (eSegment 0)
  return $ ESection visibility title segments

eSectionHeader :: P (Visibility, String)
eSectionHeader = do
  title <- many1 (oneOf "*") *> space *> many1 (noneOf "<\n\r") <* endOfLine
  return $ if "+" `isSuffixOf` title then (Visible, dropWhileEnd (=='+') title) else (Hidden, title)

eSegment :: Int -> P ESegment
eSegment n =
  (try (eFrame n) <|>
   try (eEmptyLines) <|>
   try (eEmptyLine) <|>
   try (eDottedLine n) <|>
   try (eSolidLine n) <|>
   eLine n )

eLine :: Int -> P ESegment
eLine n = do
  notFollowedBy eSectionHeader
  notFollowedBy (eFrameEnd n)
  skipSpaces n
  content <- many eText
  endOfLine
  return $ ELine n content

eEmptyLines :: P ESegment
eEmptyLines = endOfLine >> many1 endOfLine >> return EEmptyLines

eEmptyLine :: P ESegment
eEmptyLine = endOfLine >> return EEmptyLine

eFrame :: Int -> P ESegment
eFrame n = do
  (n', maybeTitle) <- try (eFrameBegin n)
  let newN = n + n'
  content <- many1 (eSegment newN) <* eFrameEnd newN
  return $ EFrame newN maybeTitle content

eFrameBegin :: Int -> P (Int, (Maybe String))
eFrameBegin n = do
  skipSpaces n
  additionalSpaces <- many space
  string "+---"
  maybeTitle <- optionMaybe (many1 (noneOf "\n\r"))
  endOfLine
  return $ (length additionalSpaces, maybeTitle)

eFrameEnd :: Int -> P ()
eFrameEnd n = skipSpaces n *> string "+---" *> endOfLine *> return ()

eSolidLine :: Int -> P ESegment
eSolidLine n = skipSpaces n *> optional (many1 space) *> string "---" *> endOfLine *> return (ESolidLine n)

eDottedLine :: Int -> P ESegment
eDottedLine n = skipSpaces n *> optional (many1 space) *> string "..." *> endOfLine *> return (EDottedLine n)

skipSpaces :: Int -> P ()
skipSpaces n = count n space *> return ()

eText :: P EText
eText = (
    eLt <|>
    eBold <|>
    eItalic <|>
    eSmall <|>
    try eColored <|>
    try eNumPlus <|>
    eString )

eBold :: P EText
eBold = EBold <$> try (string "<bold>" *> many1 eText <* string "</bold>")

eItalic :: P EText
eItalic = EItalic <$> try (string "<italic>" *> many1 eText <* string "</italic>")

eSmall :: P EText
eSmall = ESmall <$> try (string "<x-display><param>" *>
                     many (try (string "(disable-eval ")) *>
                     string "(height 0.5)" *>
                     many (try (string ")")) *>
                     string "</param>" *> many1 eText <* string "</x-display>")

eColor :: P EColor
eColor = string "<param>" *> (
  try (string "blue" >> return Blue) <|>
  try (string "dodger blue" >> return DodgerBlue) <|>
  try (string "cyan" >> return Cyan) <|>
  try (string "forest green" >> return ForestGreen) <|>
  try (string "green" >> return Green) <|>
  try (string "yellow" >> return Yellow) <|>
  try (string "dark orange" >> return DarkOrange) <|>
  try (string "saddle brown" >> return SaddleBrown) <|>
  try (string "red" >> return Red) <|>
  try (string "magenta" >> return Magenta) <|>
  try (string "dark violet" >> return DarkViolet)
  ) <* string "</param>"

eColored :: P EText
eColored = do
  string "<x-color>"
  color <- eColor
  texts <- many1 eText
  string "</x-color>"
  return $ EColored color texts

eNumPlus :: P EText
eNumPlus = do
  num <- many1 (oneOf "0123456789")
  maybeSpace1 <- optionMaybe (string " ")
  maybeSpace2 <- optionMaybe (string " ")
  case (maybeSpace1, maybeSpace2) of
    (Nothing,_) -> return $ EString num
    (Just _,Nothing) -> return $ ENumberSpace num
    (Just _, Just _) -> return $ EString (num ++ "  ")

eString :: P EText
eString = EString <$> many1 (noneOf "<0123456789\n\r")

eLt :: P EText
eLt = try (string "<<" >> return ELt)
