module RenderLatex where

import Model
import Control.Monad.Trans.State
import Control.Monad
import Data.List
import Data.Char
import qualified Data.Map as Map
import Debug.Trace
import Data.Maybe (fromMaybe,maybe)

poczatek = "\\documentclass[10pt,notitlepage,oneside,a4paper]{article}\n\
           \\\usepackage[left=10mm,top=10mm,right=10mm,bottom=10mm]{geometry}\n\
           \\\usepackage{fontspec}\n\
           \\\setmainfont{Ubuntu}\n\
           \\\usepackage[dvipsnames]{xcolor}\n\
           \\\pagestyle{empty}\n\
           \\\setlength{\\parindent}{0pt}\n\
           \\\setlength{\\parskip}{3pt}\n\
           \\\frenchspacing\n\
           \\\begin{document}"

koniec = "\\end{document}"

renderLatex :: String -> EDoc -> String
renderLatex _ segments = unlines [poczatek, renderSegments segments, koniec]

renderSegments :: [ESegment] -> String
renderSegments segments = unlines (map renderSegment segments)

renderSegment :: ESegment -> String
renderSegment EEmptyLines = "\\medskip\n"
renderSegment EEmptyLine = ""
renderSegment (EDottedLine _) = "\\dotfill\n\n"
renderSegment (ESolidLine _) = "\\hrule\n"
renderSegment (ESection Hidden title segments) = ""
renderSegment (ESection Visible title segments) = "\\vfill\\eject\n\n\\textbf{" ++ renderTexts title ++ "}\n" ++ renderSegments segments
renderSegment (ELine n texts) = renderTexts texts ++ "\n"
renderSegment (EFrame n marker _ segments) =
 if n == 0
 then "\\fbox{\\parbox{\\linewidth}{\\setlength{\\parskip}{3pt}\n" ++ renderSegments segments ++ "}}"
 else "\\makebox[0.05\\linewidth][l]{}\\fbox{\\parbox{0.92\\linewidth}{\\setlength{\\parskip}{3pt}\n" ++ renderSegments segments ++ "}}"

renderTexts :: [EText] -> String
renderTexts texts = concat $ map renderText texts

renderText :: EText -> String
renderText (ELt) = "<"
renderText (EString str) = str
renderText (ENumberSpace str) = "\\textsuperscript{" ++ str ++ "}"
renderText (EBold texts) = "\\textbf{" ++ renderTexts texts ++ "}"
renderText (EItalic texts) = "\\textit{" ++ renderTexts texts ++ "}"
renderText (EUnderline texts) = "\\underline{" ++ renderTexts texts ++ "}"
renderText (EColored color texts) = "\\textcolor[HTML]{" ++ hex color ++ "}{" ++ renderTexts texts ++ "}"
renderText (ESmall texts) = ""

-- dwie spacje lub wiecej
-- box_width = (len(state['acc'])-last_pos(state['ramki'])-2)/1.6
-- if box_width > 0: '\\makebox['+str(box_width)+'em][l]{' + state['tex'][0:-1] + '}' + state['tex'][-1]
