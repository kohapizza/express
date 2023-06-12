{-# OPTIONS -Wall #-}
{-# LANGUAGE OverloadedStrings #-}

module Sentence_process (
-- data
   Test(..),
   InputSentences(..),
 --type
   jsemData,
   emptyTest,
 -- function
   jsemSearch,
   sentenceLookup,
   eithertostring,
   sentence_filter_count,
   sentence_filter,
   chart2nodes
) where

import  qualified Data.Text as StrictT
import  qualified Data.Text.Lazy as T
import  qualified Data.Map as Map
import  System.IO.Unsafe (unsafePerformIO)
import  qualified JSeM
import  qualified Corpus.JSeM as C
import  Parser.CCG (Node(..),RuleSymbol(..),Cat(..),isBaseCategory,Feature(..),FeatureValue(..))

--Test：JSeMDataからjsem_id・answer・premises・hypothesisをとったもの
data Test = Test{
     jsem_id_w    :: StrictT.Text,
     answer_w     :: JSeM.JSeMLabel,
     premises_w   :: [StrictT.Text],
     hypothesis_w :: StrictT.Text
} deriving (Eq, Show)


--Sentences : 入力文
data InputSentences = InputSentences {
    input_Sentence :: StrictT.Text
} deriving (Show)

--文探索
type Sentence = T.Text

type SentenceMap = Map.Map Int Sentence


jsemData :: [C.JSeMData]
jsemData = unsafePerformIO C.fetchJSeMData


emptyTest :: Test
emptyTest = Test { jsem_id_w = "", answer_w = JSeM.OTHER, premises_w = [], hypothesis_w = "" } 


jsemSearch ::  [C.JSeMData] -> String -> Test
jsemSearch jsemD sentID = do
      case jsemD of
          [] -> emptyTest
          (C.JSeMData {C.jsem_id = i, C.link = l, C.description = d, C.answer = a, C.phenomena = ph, C.inference_type = it, C.note = n, C.premises = p, C.hypothesis = h} : xs )-> 
                     if(i /= StrictT.pack sentID)
                          then jsemSearch xs sentID
                          else Test{jsem_id_w = i, answer_w = a, premises_w = p, hypothesis_w = h}


sentenceLookup :: Int -> SentenceMap -> Either Sentence Sentence
sentenceLookup number smap = case Map.lookup number smap of
  Nothing -> Left $ "存在しません"
  Just sentence -> Right sentence 


--Either型からString型にする
eithertostring :: Either Sentence Sentence -> Sentence
eithertostring result =
  case result of Left sentence -> sentence
                 Right sentence -> sentence

-- '。'を取り除く
sentence_filter_count :: StrictT.Text -> Int
sentence_filter_count sentence = case (StrictT.null sentence) of
     True -> 0
     False ->  let sen_filter = StrictT.filter (/='。') sentence
                    in StrictT.length sen_filter

-- (') を文の先頭と末尾につける
sentence_filter :: StrictT.Text -> String
sentence_filter sentence = case (StrictT.null sentence) of
     True -> " "
     False -> let sen_filter = StrictT.filter (/='。') sentence in
                   let plus_sen1 = StrictT.cons '"' sen_filter in
                   let plus_sen2 =StrictT.snoc plus_sen1 '"'
                    in StrictT.unpack plus_sen2



chart2nodes :: Maybe [Node] -> [Node]
chart2nodes nodes = case nodes of
       Nothing -> []
       Just nodes -> nodes

      