-- -*- coding: utf-8; -*-
module CopyProps where

import Model
import qualified Data.Map as Map

copyProps :: [Element]
          -> [Element]

copyProps elements =
  let emptyElement = Apply "" Map.empty []
      enchancedElements = emptyElement : elements ++ [emptyElement]
      f a (Apply n p e) c =
        let prev = case a of
                     Apply _ ap _ -> Map.mapKeys ("prev"++) ap
                     _ -> Map.empty
            next = case c of
                     Apply _ cp _ -> Map.mapKeys ("next"++) cp
                     _ -> Map.empty
            firstchild = case e of
                           (Apply _ fcp _):_ -> Map.mapKeys ("firstchild"++) fcp
                           _ -> Map.empty
            newProps = Map.union (Map.union (Map.union p prev) next) firstchild
        in Apply n newProps (copyProps e)
      f a b c = b
  in zipWith3 f (emptyElement : elements) elements (tail elements ++ [emptyElement])
