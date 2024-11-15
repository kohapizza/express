{-# OPTIONS -Wall #-}
{-# LANGUAGE OverloadedStrings #-}

module Lightblue (
  -- parseSentence,
  -- parseSentence',
  parse
  ) where

import qualified Data.Text.Lazy as T
import qualified Parser.ChartParser as CP
import qualified Parser.CCG as CCG
import qualified Interface.HTML as HTML
import qualified Parser.Language.Japanese.Lexicon as L (lexicalResourceBuilder)
import qualified Parser.Language.Japanese.Juman.CallJuman as Juman
import qualified Parser.Language as PL (jpOptions)

-- parseSentence :: Int -> Int -> T.Text -> IO(T.Text)
-- parseSentence beam nbest sentence = do
--   nodes <- CP.simpleParse beam sentence
--   let nbestnodes = take nbest nodes;
--   return $ T.concat $ [HTML.startMathML] ++ (map HTML.toMathML nbestnodes) ++ [HTML.endMathML]

-- parseSentence' :: Int -> Int -> T.Text -> IO([CCG.Node])
-- parseSentence' beam nbest sentence = do
--   nodes <- CP.simpleParse beam sentence
--   return $ take nbest nodes

-- parse :: Int           -- ^ The beam width
--          -> T.Text     -- ^ A sentence to be parsed
--          -> IO (CP.Chart) -- ^ A pair of the resulting CYK-chart and a list of CYK-charts for segments
-- parse beam sentence = do 
--    chart <- CP.parse beam True (\_ _ -> id) sentence
--    return chart

defaultParseSetting' :: Int -> IO CP.ParseSetting
defaultParseSetting' beam = do
  lr <- L.lexicalResourceBuilder Juman.KWJA
  return $ CP.ParseSetting PL.jpOptions lr beam (-1) (-1) (-1) True Nothing Nothing True False

-- beamとsentenceのみ用いてchartを得る
parse :: Int           -- ^ The beam width
          -> T.Text        -- ^ A sentence to be parsed
          -> IO CP.Chart   -- ^ The resulting CYK-chart
parse beam sentence = do
    useOnlybeamParseSetting <- defaultParseSetting' beam
    chart <- CP.parse useOnlybeamParseSetting sentence 
    return chart