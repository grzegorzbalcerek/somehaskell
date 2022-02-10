module Main where

import MainLogic
import RenderSvg
import Model

main = mainLogic remodel renderSvg ".svg"
