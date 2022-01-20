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

frameStarters = "+#"
frameMarkers = "-~=*^`"

parseEnrichedInput :: String -> String -> Either ParseError EDoc
parseEnrichedInput inputPath input = runParser eDoc () inputPath input

eDoc :: P EDoc
eDoc = eDocHeader *> many (eSection <|> eSegment) <* eof

eDocHeader :: P ()
eDocHeader = string "Content-Type: text/enriched" >> endOfLine >> string "Text-Width: 70" >> endOfLine >> endOfLine >> return ()

eSection :: P ESegment
eSection = do
  (visibility, title) <- eSectionHeader
  segments <- many1 (eSegment)
  return $ ESection visibility title segments

eSectionHeader :: P (Visibility, [EText])
eSectionHeader = do
  title <- many1 (oneOf "*") *> string " " *> many (eText eStringNumPlusFactory) <* endOfLine
  return $ if "+" `isSuffixOf` (stringifyTexts title) then (Visible, title) else (Hidden, title)

eSegment :: P ESegment
eSegment =
  (try (eFrame 0 frameStarters frameMarkers) <|>
   try (eEmptyLines) <|>
   try (eEmptyLine) <|>
   try (eDottedLine 0) <|>
   try (eSolidLine 0) <|>
   eLine 0)

eFrameContent :: Int -> String -> String -> P ESegment
eFrameContent n starters markers =
  (try (eFrame (n+1) starters markers) <|>
   try (eEmptyLines) <|>
   try (eEmptyLine) <|>
   try (eDottedLine n) <|>
   try (eSolidLine n) <|>
   try (eLine n) )

eLine :: Int -> P ESegment
eLine n = do
  notFollowedBy (try eSectionHeader)
  notFollowedBy (try (eFrameBegin n frameStarters frameMarkers))
  skipSpaces n
  content <- many (eText ENumberSpace)
  endOfLine
  return $ ELine n content

eEmptyLines :: P ESegment
eEmptyLines = endOfLine >> many1 endOfLine >> return EEmptyLines

eEmptyLine :: P ESegment
eEmptyLine = endOfLine >> return EEmptyLine

eFrame :: Int -> String -> String -> P ESegment
eFrame n starters markers = do
  (n', starter, marker, title) <- try (eFrameBegin n starters markers)
  let newN = n + n'
  content <- many (eFrameContent newN starter marker)
  return $ EFrame newN marker title content

eFrameBegin :: Int -> String -> String -> P (Int, String, String, [EText])
eFrameBegin n starters markers = do
  skipSpaces n
  additionalSpaces <- many (string " ")
  starter <- count 1 (oneOf starters)
  marker <- count 1 (oneOf markers)
  count 2 (string marker)
  title <- many (eText eStringNumPlusFactory)
  endOfLine
  return $ (length additionalSpaces, starter, marker, title)

eSolidLine :: Int -> P ESegment
eSolidLine n = skipSpaces n *> optional (many1 (string " ")) *> string "---" *> endOfLine *> return (ESolidLine n)

eDottedLine :: Int -> P ESegment
eDottedLine n = skipSpaces n *> optional (many1 (string " ")) *> string "..." *> endOfLine *> return (EDottedLine n)

skipSpaces :: Int -> P ()
skipSpaces n = count n (string " ") *> return ()

eText :: (String -> EText) -> P EText
eText numPlusFactory = (
    eLt <|>
    (eBold numPlusFactory) <|>
    (eItalic numPlusFactory) <|>
    (eUnderline numPlusFactory) <|>
    (eSmall numPlusFactory) <|>
    try (eColored numPlusFactory) <|>
    try (eNumPlus numPlusFactory) <|>
    eString )

eBold :: (String -> EText) -> P EText
eBold numPlusFactory = EBold <$> try (string "<bold>" *> many1 (eText numPlusFactory) <* string "</bold>")

eItalic :: (String -> EText) -> P EText
eItalic numPlusFactory = EItalic <$> try (string "<italic>" *> many1 (eText numPlusFactory) <* string "</italic>")

eUnderline :: (String -> EText) -> P EText
eUnderline numPlusFactory = EUnderline <$> try (string "<underline>" *> many1 (eText numPlusFactory) <* string "</underline>")

eSmall :: (String -> EText) -> P EText
eSmall numPlusFactory = ESmall <$> try (string "<x-display><param>" *>
                     many (try (string "(disable-eval ")) *>
                     string "(height 0.5)" *>
                     many (try (string ")")) *>
                     string "</param>" *> many1 (eText numPlusFactory) <* string "</x-display>")

eColor :: P EColor
eColor = string "<param>" *> (
  try (string "blue" >> return Blue) <|>
  try (string "dodger blue" >> return DodgerBlue) <|>
  try (string "cadet blue" >> return CadetBlue) <|>
  try (string "cyan" >> return Cyan) <|>
  try (string "forest green" >> return ForestGreen) <|>
  try (string "green" >> return Green) <|>
  try (string "yellow green" >> return YellowGreen) <|>
  try (string "yellow" >> return Yellow) <|>
  try (string "dark khaki" >> return DarkKhaki) <|>
  try (string "orange" >> return Orange) <|>
  try (string "tomato" >> return Tomato) <|>
  try (string "saddle brown" >> return SaddleBrown) <|>
  try (string "firebrick" >> return Firebrick) <|>
  try (string "red" >> return Red) <|>
  try (string "magenta" >> return Magenta) <|>
  try (string "dark violet" >> return DarkViolet)
  ) <* string "</param>"

eColored :: (String -> EText) -> P EText
eColored numPlusFactory = do
  string "<x-color>"
  color <- eColor
  texts <- many1 (eText numPlusFactory)
  string "</x-color>"
  return $ EColored color texts

eNumPlus :: (String -> EText) -> P EText
eNumPlus numPlusFactory = do
  num <- many1 (oneOf "0123456789")
  maybeSpace1 <- optionMaybe (string " ")
  maybeSpace2 <- optionMaybe (string " ")
  case (maybeSpace1, maybeSpace2) of
    (Nothing,_) -> return $ EString num
    (Just _,Nothing) -> return $ numPlusFactory num
    (Just _, Just _) -> return $ EString (num ++ "  ")

eStringNumPlusFactory :: String -> EText
eStringNumPlusFactory str = EString (str ++ " ")

eString :: P EText
eString = EString <$> many1 (noneOf "<0123456789\n\r")

eLt :: P EText
eLt = try (string "<<" >> return ELt)
