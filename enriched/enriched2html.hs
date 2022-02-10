module Main where

import MainLogic
import RenderHtml
import Model

main = mainLogic id renderHtml ".html"
