module RenderHtml where

import Model
import Control.Monad.Trans.State
import Control.Monad
import Data.List
import Data.Char
import qualified Data.Map as Map
import Debug.Trace
import Data.Maybe (fromMaybe,maybe)

poczatek = "<?xml version='1.0' encoding='UTF-8' standalone='no'?>\n\
\<html>\n\
\<head>\n\
\<style>\n\
\html, body {\n\
\font-family: sans-serif;\n\
\font-size: 12px;\n\
\}\n\
\</style>\n\
\</head>\n\
\<body>\n"

koniec = "</body>\n\
\</html>\n"


renderHtml :: String -> EDoc -> String
renderHtml _ segments = unlines [poczatek, renderSegments segments, koniec]

renderSegments :: [ESegment] -> String
renderSegments segments = unlines (map renderSegment segments)

renderSegment :: ESegment -> String
renderSegment EEmptyLines = "<div style='height: 0.5em'></div>\n"
renderSegment EEmptyLine = ""
renderSegment (EDottedLine _) = "<div style='width: 95%; margin: 5px; height: 1px; border-bottom: 1px black dotted'></div>\n"
renderSegment (ESolidLine _) = "<div style='width: 95%; margin: 5px; height: 1px; border-bottom: 1px black solid'></div>\n"
renderSegment (ESection Hidden title segments) = ""
renderSegment (ESection Visible title segments) =
  "<details><summary>" ++ renderTexts title ++ "</summary>\n" ++ renderSegments segments ++ "</details>\n"
renderSegment (ELine n texts) = renderTexts texts ++ "<br/>\n"
renderSegment (EFrame n color [] segments) =
 "<div style='" ++ (if n == 0 then "" else "margin: 2px 20px; ") ++
 "border: 1px solid #" ++ hex color ++ "'>\n" ++ renderSegments segments ++ "</div>\n"
renderSegment (EFrame n color title segments) =
 "<details style='margin: 2px; padding: 2px 2px 2px 5px; position: relative; left: " ++ show (fromIntegral n / 4) ++ "%; width: " ++ show (99 - fromIntegral n / 2) ++ "%; border: 2px solid #" ++
  hex color ++ "'><summary>" ++ renderTexts title ++ "</summary>\n" ++ renderSegments segments ++ "</details>\n"

renderTexts :: [EText] -> String
renderTexts texts = concat $ map renderText texts

renderText :: EText -> String
renderText (ELt) = "&lt;"
renderText (ESpaces n) = "<span style='white-space: pre'>" ++ (concat $ replicate n " ") ++ "</span>"
renderText (EString str) = str
renderText (ENumberSpace str) = "<sup><small>" ++ str ++ "</small></sup>"
renderText (EBold texts) = "<strong>" ++ renderTexts texts ++ "</strong>"
renderText (EItalic texts) = "<em>" ++ renderTexts texts ++ "</em>"
renderText (EUnderline texts) = "<u>" ++ renderTexts texts ++ "</u>"
renderText (EColored color texts) = "<span style='color: #" ++ hex color ++ "'>" ++ renderTexts texts ++ "</span>"
renderText (ESmall texts) = ""
