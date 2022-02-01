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
  EFrame Int EColor [EText] [ESegment] |
  ESection Visibility [EText] [ESegment]
  deriving (Eq,Show)

data EText =
  ELt |
  EString String |
  ENumberSpace String |
  EBold [EText] |
  EItalic [EText] |
  EUnderline [EText] |
  ESmall [EText] |
  EColored EColor [EText]
  deriving (Eq,Show)

data EColor =
  Black |
  Blue |
  DodgerBlue |
  CadetBlue |
  Cyan |
  ForestGreen |
  Green |
  YellowGreen |
  Yellow |
  DarkKhaki |
  Orange |
  Tomato |
  SaddleBrown |
  Firebrick |
  Red |
  Magenta |
  DarkViolet
  deriving (Eq,Show)

hex :: EColor -> String
hex Black = "000000"
hex Blue = "0000ff"
hex DodgerBlue = "1e90ff"
hex CadetBlue = "5f9ea0"
hex Cyan = "00ffff"
hex ForestGreen = "228b22"
hex Green = "00ff00"
hex YellowGreen = "9acd32"
hex Yellow = "ffff00"
hex DarkKhaki = "bdb76b"
hex Orange = "ffa500"
hex Tomato = "ff6347"
hex SaddleBrown = "8b4513"
hex Firebrick = "b22222"
hex Red = "ff0000"
hex Magenta = "ff00ff"
hex DarkViolet = "9400d3"

colorName :: EColor -> String
colorName c = show c

isFrame :: ESegment -> Bool
isFrame (EFrame _ _ _ _) = True
isFrame _ = False

hasFrameOf :: EColor -> ESegment -> Bool
hasFrameOf color (EFrame _ c _ _) | color == c = True
hasFrameOf color (EFrame _ _ _ xs) = or $ map (hasFrameOf color) xs
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

filterFrames :: EColor -> [ESegment] -> [ESegment]
filterFrames color [] = []
filterFrames color ((EFrame n c title segments):xs) | color == c =
  ((EFrame n c title (filterFrames color segments)): filterFrames color xs)
filterFrames color ((EFrame n c title segments):xs) | color /= c =
  let filtered = onlyFrames segments
  in if title == [] && filtered == []
     then filterFrames color xs
     else ((EFrame n c (title++[EString "…"]) (onlyFrames segments)): filterFrames color xs)
filterFrames color (EEmptyLine:xs) = EEmptyLine:filterFrames color xs
filterFrames color (EEmptyLines:xs) = EEmptyLines:filterFrames color xs
filterFrames color (x:xs) = x:filterFrames color xs

onlyFrames :: [ESegment] -> [ESegment]
onlyFrames ((EFrame n c t s):xs) =
  let filtered = onlyFrames s
  in if filtered == []
     then (onlyFrames xs)
     else (EFrame n c t filtered):(onlyFrames xs)
onlyFrames (_:xs) = onlyFrames xs
onlyFrames [] = []

remodel :: EDoc -> EDoc
remodel ((EDottedLine n):xs) = (EDottedLine n) : remodel xs
remodel ((ESolidLine n):xs) = (ESolidLine n) : remodel xs
remodel ((EFrame n c t s):xs) = (EFrame n c t (remodel s)):remodel xs
remodel (s@(ESection _ _ _):xs) =
  maybeRemodelSection Nothing s ++
  maybeRemodelSection (Just Black) s ++
  maybeRemodelSection (Just Blue) s ++
  maybeRemodelSection (Just DodgerBlue) s ++
  maybeRemodelSection (Just CadetBlue) s ++
  maybeRemodelSection (Just Cyan) s ++
  maybeRemodelSection (Just ForestGreen) s ++
  maybeRemodelSection (Just Green) s ++
  maybeRemodelSection (Just YellowGreen) s ++
  maybeRemodelSection (Just Yellow) s ++
  maybeRemodelSection (Just DarkKhaki) s ++
  maybeRemodelSection (Just Orange) s ++
  maybeRemodelSection (Just Tomato) s ++
  maybeRemodelSection (Just SaddleBrown) s ++
  maybeRemodelSection (Just Firebrick) s ++
  maybeRemodelSection (Just Red) s ++
  maybeRemodelSection (Just Magenta) s ++
  maybeRemodelSection (Just DarkViolet) s ++
  remodel xs
remodel (x:xs) = x : remodel xs
remodel [] = []

maybeRemodelSection :: Maybe EColor -> ESegment -> [ESegment]
maybeRemodelSection maybeColor s@(ESection visibility title segments) =
  case (maybeColor, filter isFrame segments) of
    (Nothing, []) -> [s]
    (Just color, (_:_)) ->
      let filtered = filterFrames color segments
      in if (color == Black || any (hasFrameOf color) segments) && filtered /= []
         then [ESection visibility (title ++ colorTitleSuffix color) filtered]
         else []
    _ -> []

colorTitleSuffix :: EColor -> [EText]
colorTitleSuffix color = [EString (" (" ++ show color ++ ")")]

findColor :: [EText] -> EColor
findColor ((EColored c t):_) = c
findColor ((EBold t):xt) = findColor (t++xt)
findColor ((EItalic t):xt) = findColor (t++xt)
findColor ((EUnderline t):xt) = findColor (t++xt)
findColor ((ESmall t):xt) = findColor (t++xt)
findColor (_:xt) = findColor xt
findColor [] = Black
