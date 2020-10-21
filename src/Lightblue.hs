{-# OPTIONS -Wall #-}
{-# LANGUAGE OverloadedStrings #-}

module Lightblue (
  parseSentence
  ) where

import qualified Data.Text.Lazy as T
import qualified Parser.ChartParser as CP
import qualified Interface.HTML as HTML

-- |
parseSentence :: Int -> Int -> T.Text -> IO(T.Text)
parseSentence beam nbest sentence = do
  nodes <- CP.simpleParse beam sentence
  let nbestnodes = take nbest nodes;
  return $ T.concat $ [HTML.startMathML] ++ (map HTML.toMathML nbestnodes) ++ [HTML.endMathML]
