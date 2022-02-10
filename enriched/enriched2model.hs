module Main where

import MainLogic
import RenderModel

main = mainLogic id renderModel ".txt"
