module Model where

import Data.List (intersperse,groupBy,or)
import Control.Monad.Reader
import Data.Maybe (fromMaybe)
import qualified Data.Map as Map

type EDoc = [ESegment]

data Visibility = Visible | Hidden
  deriving (Eq,Show)

data ESegment =
  EEmptyLine |
  EEmptyLines |
  EDottedLine Int |
  ESolidLine Int |
  ELine Int [EText] |
  EFrame Int String [EText] [ESegment] |
  ESection Visibility [EText] [ESegment]
  deriving (Eq,Show)

data EText =
  ELt |
  EString String |
  ENumberSpace String |
  EBold [EText] |
  EItalic [EText] |
  ESmall [EText] |
  EColored EColor [EText]
  deriving (Eq,Show)

data EColor =
  Blue |
  DodgerBlue |
  Cyan |
  ForestGreen |
  Green |
  Yellow |
  DarkOrange |
  SaddleBrown |
  Red |
  Magenta |
  DarkViolet
  deriving (Eq,Show)

hex :: EColor -> String
hex Blue = "0000ff"
hex DodgerBlue = "1e90ff"
hex Cyan = "00ffff"
hex ForestGreen = "228b22"
hex Green = "00ff00"
hex Yellow = "ffff00"
hex DarkOrange = "ff8c00"
hex SaddleBrown = "8b4513"
hex Red = "ff0000"
hex Magenta = "ff00ff"
hex DarkViolet = "9400d3"

isFrame :: ESegment -> Bool
isFrame (EFrame _ _ _ _) = True
isFrame _ = False

hasFrameOf :: String -> ESegment -> Bool
hasFrameOf marker (EFrame _ m _ _) | marker == m = True
hasFrameOf marker (EFrame _ _ _ xs) = or $ map (hasFrameOf marker) xs
hasFrameOf _ _ = False

stringifyTexts :: [EText] -> String
stringifyTexts texts = concat $ map stringifyText texts

stringifyText :: EText -> String
stringifyText (ELt) = "<"
stringifyText (EString str) = str
stringifyText (ENumberSpace str) = str
stringifyText (EBold texts) = stringifyTexts texts
stringifyText (EItalic texts) = stringifyTexts texts
stringifyText (ESmall texts) = stringifyTexts texts
stringifyText (EColored color texts) = stringifyTexts texts

filterFrames :: String -> [ESegment] -> [ESegment]
filterFrames marker [] = []
filterFrames marker ((EFrame n m title segments):xs) | marker == m =
  ((EFrame n m title (filterFrames marker segments)): filterFrames marker xs)
filterFrames marker ((EFrame n m title segments):xs) | marker /= m =
  let filtered = onlyFrames segments
  in if title == [] && filtered == []
     then filterFrames marker xs
     else ((EFrame n m (title++[EString "…"]) (onlyFrames segments)): filterFrames marker xs)
filterFrames marker (EEmptyLine:xs) = filterFrames marker xs
filterFrames marker (EEmptyLines:xs) = filterFrames marker xs
filterFrames marker (x:xs) = x:filterFrames marker xs

onlyFrames :: [ESegment] -> [ESegment]
onlyFrames ((EFrame n m t s):xs) =
  let filtered = onlyFrames s
  in if filtered == []
     then (onlyFrames xs)
     else (EFrame n m t filtered):(onlyFrames xs)
onlyFrames (_:xs) = onlyFrames xs
onlyFrames [] = []

remodel :: EDoc -> EDoc
remodel ((EDottedLine n):xs) = (EDottedLine n) : remodel xs
remodel ((ESolidLine n):xs) = (ESolidLine n) : remodel xs
remodel ((EFrame n m t s):xs) = (EFrame n m t (remodel s)):remodel xs
remodel ((ESection v t s):xs) = (ESection v t (remodel s)):remodel xs
remodel (x:xs) = x : remodel xs
remodel [] = []
