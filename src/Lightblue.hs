{-# OPTIONS -Wall #-}
{-# LANGUAGE OverloadedStrings #-}

module Lightblue (
  parseSentence,
  parseSentence',
  parse
  ) where

import qualified Data.Text.Lazy as T
import qualified Parser.ChartParser as CP
import qualified Parser.CCG as CCG
import qualified Interface.HTML as HTML
import qualified Parser.Language.Japanese.Lexicon as L (lexicalResourceBuilder)
import qualified Parser.Language.Japanese.Juman.CallJuman as Juman
import qualified Parser.Language as PL (jpOptions)

-- ビーム数と文を用いてパーズし、上位nbest個のノードを抽出してMathMLタグで囲んだT.Textを返す
parseSentence :: Int -> Int -> T.Text -> IO(T.Text)
parseSentence beam nbest sentence = do
  -- useOnlybeamParseSetting :: CP.ParseSetting
  useOnlybeamParseSetting <- defaultParseSetting' beam 
  nodes <- CP.simpleParse useOnlybeamParseSetting sentence -- パーズしてnodeを得る, nodes :: [CCG.Node]
  let nbestnodes = take nbest nodes
  -- startMathML = "<math xmlns='http://www.w3.org/1998/Math/MathML'>"
  -- endMathML = "</math>"
  -- toMathML :: a -> T.Text
  return $ T.concat $ [HTML.startMathML] ++ (map HTML.toMathML nbestnodes) ++ [HTML.endMathML]

-- ビーム数と文を用いてパーズし、上位 nbest 個のノードを抽出してリストとして返す
parseSentence' :: Int -> Int -> T.Text -> IO([CCG.Node])
parseSentence' beam nbest sentence = do
  useOnlybeamParseSetting <- defaultParseSetting' beam 
  nodes <- CP.simpleParse useOnlybeamParseSetting sentence
  return $ take nbest nodes

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