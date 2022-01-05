module Model where

import Data.List (intersperse,groupBy)
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
  EFrame Int [ESegment] |
  ESection Visibility String [ESegment]
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
