module Main where

import MainLogic
import RenderLatex
import Model

main = mainLogic remodel renderLatex ".tex"
