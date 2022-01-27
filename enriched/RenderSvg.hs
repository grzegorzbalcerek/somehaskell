module RenderSvg where

import Model
import Control.Monad.Trans.State
import Control.Monad
import Data.List
import Data.Char
import qualified Data.Map as Map
import Debug.Trace
import Data.Maybe (fromMaybe,maybe)

poczatek = "<?xml version='1.0' encoding='UTF-8' standalone='no'?>\n\
\<svg\n\
\   xml:space='preserve'\n\
\   width='210mm'\n\
\   height='400mm'\n\
\   viewBox='0 0 210 400'\n\
\   version='1.1'\n\
\   id='svg1'\n\
\   inkscape:version='1.1.1 (eb90963e84, 2021-10-02)'\n\
\   inkscape:export-xdpi='96'\n\
\   inkscape:export-ydpi='96'\n\
\   xmlns:inkscape='http://www.inkscape.org/namespaces/inkscape'\n\
\   xmlns:sodipodi='http://sodipodi.sourceforge.net/DTD/sodipodi-0.dtd'\n\
\   xmlns='http://www.w3.org/2000/svg'\n\
\   xmlns:svg='http://www.w3.org/2000/svg'\n\
\   style='\
          \font-style:normal;\
          \font-variant:normal;\
          \font-weight:normal;\
          \font-stretch:normal;\
          \font-size:3.5px;\
          \line-height:120%;\
          \font-family:sans-serif;\
          \-inkscape-font-specification:\"sans-serif, Normal\";\
          \font-variant-ligatures:normal;\
          \font-variant-caps:normal;\
          \font-variant-numeric:normal;\
          \font-variant-east-asian:normal;\
          \text-align:start;\
          \letter-spacing:0pt;\
          \word-spacing:0pt;\
          \writing-mode:lr-tb;\
          \text-anchor:start;\
          \white-space:pre;\
          \display:inline;\
          \fill:#000000;\
          \fill-opacity:1;\
          \stroke:none;\
          \stroke-width:0.25px;\
          \stroke-linecap:butt;\
          \stroke-linejoin:miter;\
          \stroke-opacity:1\
          \'\n\
\>\n\
\<sodipodi:namedview\
  \ id='base'\
  \ pagecolor='#ffffff'\
  \ bordercolor='#666666'\
  \ borderopacity='1.0'\
  \ inkscape:pageopacity='1'\
  \ inkscape:pageshadow='2'\
  \ inkscape:document-rotation='0'\
  \ inkscape:document-units='mm'\
  \ showgrid='false'\
  \ inkscape:zoom='1'\
  \ inkscape:cx='363.5'\
  \ inkscape:cy='315'\
  \ units='px'\
  \ inkscape:window-width='1920'\
  \ inkscape:window-height='1043'\
  \ inkscape:window-x='0'\
  \ inkscape:window-y='0'\
  \ inkscape:window-maximized='1'/>\n"

koniec = "</svg>"

dottedLineStyle = "style='fill:none;stroke:#000000;stroke-width:0.40;stroke-opacity:1;stroke-dasharray:0.25,0.5'"
solidLineStyle = "style='fill:none;stroke:#000000;stroke-width:0.40;stroke-opacity:1'"
rectStyle = "style='fill:none;stroke-width:0.40;stroke-opacity:1'"

startPosX = 2.0
startPosY = 4.0
lineSize = 4.0
halfLineSize = lineSize / 2.0

renderSvg :: String -> EDoc -> String
renderSvg p segments = unlines [poczatek, snd (renderSegments p (startPosX, startPosY) segments), koniec]

arg :: String -> String -> String
arg label value = " " ++ label ++ "='" ++ value ++ "'"

argD :: String -> Double -> String
argD label value = arg label (show value)

argExp :: String -> String
argExp file = arg "inkscape:export-filename" file

argPair :: String -> String -> (Double, Double) -> String
argPair labelX labelY (x,y) = argD labelX x ++ argD labelY y

argXY :: (Double, Double) -> String
argXY = argPair "x" "y"

argX1Y1 :: (Double, Double) -> String
argX1Y1 = argPair "x1" "y1"

argX2Y2 :: (Double, Double) -> String
argX2Y2 = argPair "x2" "y2"

argWidthHeight :: (Double, Double) -> String
argWidthHeight = argPair "width" "height"

argVisibility :: Visibility -> String
argVisibility Visible = ""
argVisibility Hidden = arg "style" "display:none"

renderSegments :: String -> (Double, Double) -> [ESegment] -> ((Double, Double), String)
renderSegments p (x,y) (s:segments) =
    let ((w,h),o) = renderSegment p (x,y) s
        ((ws,hs),os) = renderSegments p (x,y+h) segments
    in ((w `max` ws,h + hs), o ++ os)
renderSegments _ _ [] = ((0,0),"")

expFilter title = filter (`elem` ['a'..'z']++['A'..'Z']++['0'..'9']) title

renderSegment :: String -> (Double, Double) -> ESegment -> ((Double, Double), String)
renderSegment _ (x,y) EEmptyLine = ((0,0),"")
renderSegment _ (x,y) EEmptyLines = ((0,halfLineSize),"")
renderSegment p _ (ESection visibility title segments) =
      let expFile = p ++ expFilter (stringifyTexts title) ++ ".png"
          expArg = argExp expFile
          ((w,h),o) = renderSegments p (startPosX, startPosY + lineSize) segments
          id = (stringifyTexts title) `intersect` (['A'..'Z'] ++ ['a'..'z'] ++ ['0'..'9'])
          g1 = "<g inkscape:groupmode='layer'" ++ arg "id" id ++ arg "inkscape:label" (stringifyTexts title) ++ argVisibility visibility ++ ">\n"
          t = "<text" ++ expArg ++ argXY (startPosX, startPosY) ++ ">" ++ renderTexts title ++ "</text>\n"
          g2 = "</g>\n"
      in ((0,0), g1 ++ t ++ o ++ g2)
renderSegment _ (x,y) (ELine n texts) =
    let o = "<text" ++ argXY (x,y) ++ ">" ++ renderTexts texts ++ "</text>\n"
    in ((fromIntegral (length (stringifyTexts texts)), lineSize), o)
renderSegment _ (x,y) (EDottedLine n) =
  ((0, halfLineSize), renderSeparatorLine dottedLineStyle (x,y) n)
renderSegment _ (x,y) (ESolidLine n) =
  ((0, halfLineSize), renderSeparatorLine solidLineStyle (x,y) n)
renderSegment p (x,y) (EFrame n marker color title segments) =
    let ((_,ht),t) = renderMaybeText (startPosX + fromIntegral n,y + halfLineSize) title
        ((ws,hs),os) = renderSegments p (startPosX + fromIntegral n,y + halfLineSize + ht) segments
        rect = "<rect " ++ rectStyle ++ arg "stroke" ('#':(hex color)) ++
               argXY (startPosX + fromIntegral n - 0.5,y - halfLineSize) ++
               argWidthHeight (fromIntegral 200 - fromIntegral n * 2.0,ht+hs+halfLineSize) ++
                "/>\n"
    in ((ws, ht + hs + lineSize), rect ++ t ++ os)

renderMaybeText :: (Double, Double) -> [EText] -> ((Double, Double), String)
renderMaybeText _ [] = ((0,0),"")
renderMaybeText (x,y) title = ((0,lineSize),"<text" ++ argXY (x,y) ++ ">" ++ renderTexts title ++ "</text>\n")

renderSeparatorLine :: String -> (Double, Double) -> Int -> String
renderSeparatorLine style (x,y) n =
  "<line " ++ style ++
  argX1Y1 (startPosX + fromIntegral n + 1.0,y - halfLineSize) ++
  argX2Y2 (startPosX + fromIntegral 200 - fromIntegral n - 2.0,y - halfLineSize) ++
  "/>\n"
renderTexts :: [EText] -> String
renderTexts texts = concat $ map renderText texts

renderText :: EText -> String
renderText (ELt) = "&lt;"
renderText (EString str) = str
renderText (ENumberSpace str) = "<tspan style='font-size:75%;baseline-shift:super'>" ++ str ++ "</tspan>"
renderText (EBold texts) = "<tspan style='font-weight:bold'>" ++ renderTexts texts ++ "</tspan>"
renderText (EItalic texts) = "<tspan style='font-style:italic'>" ++ renderTexts texts ++ "</tspan>"
renderText (EUnderline texts) = "<tspan style='text-decoration:underline'>" ++ renderTexts texts ++ "</tspan>"
renderText (EColored color texts) = "<tspan style='fill:#" ++ hex color ++ "'>" ++ renderTexts texts ++ "</tspan>"
renderText (ESmall texts) = ""
