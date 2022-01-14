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
renderSegment EEmptyLines = "EEmptyLines"
renderSegment EEmptyLine = ""
renderSegment (EDottedLine n) = replicate n ' ' ++ "..."
renderSegment (ESolidLine n) = replicate n ' ' ++ "---"
renderSegment (ESection visibility title segments) = "ESection " ++ show visibility ++ " " ++ title ++ "\n" ++ renderSegments segments
renderSegment (ELine n texts) = replicate n ' ' ++ "ELine " ++ show n ++ ": " ++ renderTexts texts
renderSegment (EFrame n maybeTitle segments) =
   replicate n ' ' ++ "+--- " ++ show n ++ " " ++ show maybeTitle ++ ":\n" ++ renderSegments segments ++ "\n" ++ replicate n ' ' ++ "+---"

renderTexts :: [EText] -> String
renderTexts texts = concat $ " " `intersperse` map renderText texts

renderText :: EText -> String
renderText (ELt) = "&lt;"
renderText (EString str) = show str
renderText (ENumberSpace str) = show str
renderText (EBold texts) = "bold(" ++ renderTexts texts ++ ")"
renderText (EItalic texts) = "italic(" ++ renderTexts texts ++ ")"
renderText (ESmall texts) = "small(" ++ renderTexts texts ++ ")"
renderText (EColored color texts) = show color ++ "(" ++ renderTexts texts ++ ")"
