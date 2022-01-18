module RenderModel where

import Model
import Control.Monad.Trans.State
import Control.Monad
import Data.List
import Data.Char
import qualified Data.Map as Map
import Data.Maybe (fromMaybe,maybe)

renderModel :: EDoc -> String
renderModel eDoc = renderSegments eDoc

renderSegments :: [ESegment] -> String
renderSegments eSegments = concat $ "\n" `intersperse` (filter (/="") (map renderSegment eSegments))

renderSegment :: ESegment -> String
renderSegment EEmptyLines = " "
renderSegment EEmptyLine = ""
renderSegment (EDottedLine n) = replicate n ' ' ++ "..."
renderSegment (ESolidLine n) = replicate n ' ' ++ "---"
renderSegment (ESection visibility title segments) = "ESection " ++ show visibility ++ " " ++ renderTexts title ++ "\n" ++ renderSegments segments
renderSegment (ELine n texts) = replicate n ' ' ++ "ELine " ++ show n ++ ": " ++ renderTexts texts
renderSegment (EFrame n marker maybeTitle segments) =
   replicate n ' ' ++ "EFrame " ++ show n ++ " " ++ marker ++ " " ++ show maybeTitle ++ ":\n" ++ renderSegments segments ++ "\n" ++ replicate n ' ' ++ marker

renderTexts :: [EText] -> String
renderTexts texts = concat $ " " `intersperse` map renderText texts

renderText :: EText -> String
renderText (ELt) = "&lt;"
renderText (EString str) = str
renderText (ENumberSpace str) = str
renderText (EBold texts) = "bold(" ++ renderTexts texts ++ ")"
renderText (EItalic texts) = "italic(" ++ renderTexts texts ++ ")"
renderText (ESmall texts) = "small(" ++ renderTexts texts ++ ")"
renderText (EColored color texts) = show color ++ "(" ++ renderTexts texts ++ ")"
