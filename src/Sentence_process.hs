{-# OPTIONS -Wall #-}
{-# LANGUAGE OverloadedStrings #-}

module Sentence_process (
-- data
   Test(..),
   InputSentences(..),
 --type
   ChartMap,
   emptychart,
   jsemData,
   emptyTest,
 -- function
   jsemSearch,
   sentenceLookup,
   eithertostring,
   sentence_filter_count,
   sentence_filter,
   make_chartlist,
   make_onechartlist,
   chart2nodes,
   expresscat
) where

import  qualified Data.Text as StrictT
import  qualified Data.Text.Lazy as T
import  qualified Data.Map as Map
import  System.IO.Unsafe (unsafePerformIO)
import  qualified JSeM
import  qualified Corpus.JSeM as C
--import  Parser.CCG (Node(..),RuleSymbol(..),Cat(..),isBaseCategory,Feature(..),FeatureValue(..))
import qualified Parser.CCG as CCG
import  qualified Parser.ChartParser as CP

--Test：JSeMDataからjsem_id・answer・premises・hypothesisをとったもの
data Test = Test{
     jsem_id_w    :: StrictT.Text,
     answer_w     :: JSeM.JSeMLabel,
     premises_w   :: [StrictT.Text],
     hypothesis_w :: StrictT.Text
} deriving (Eq, Show)


--Sentences : 入力文
data InputSentences = InputSentences {
    input_Sentence :: StrictT.Text,
--    sen_start :: Int,
--    sen_end :: Int,
    sen_beam :: Int
} deriving (Eq,Show)

--文探索
type Sentence = T.Text

type SentenceMap = Map.Map Int Sentence

-- チャートMap
type ChartMap = Map.Map Int [CCG.Node]
emptychart :: ChartMap
emptychart = Map.fromList $ []


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


-- '。'を取り除いた文字数
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


-- let maybe_nodes = Map.lookup (senS,senE) chart
-- type Chart = M.Map (Int,Int) [CCG.Node]
make_chartlist :: Int -> Int -> Int -> CP.Chart -> ChartMap -> ChartMap
make_chartlist bd num count lightbluechart chartmap = 
     let conum = count
     in
      case bd of
         conum -> case conum of 
                                    count  ->  chartmap
                                    otherwise   -> 
                                         let conum = count 
                                         in let chartlist =lookupChart (bd+1) conum lightbluechart
                                          in
                                           if num == 1  then let chartmap = insert_chartlist num chartlist chartmap  
                                                                         in make_chartlist bd num count lightbluechart chartmap
                                                               else let chartmap = insert_chartlist (num+1) chartlist chartmap  
                                                                    in make_chartlist bd num count lightbluechart chartmap
         otherwise ->  case conum of 
                              count -> let chartlist =lookupChart bd conum lightbluechart 
                                                in insert_chartlist num chartlist chartmap
                              otherwise -> let chartlist = lookupChart bd conum lightbluechart 
                                            in
                                            if num == 1 then let chartmap = insert_chartlist num chartlist chartmap  in make_chartlist bd num count lightbluechart chartmap
                                                                 else let chartmap = insert_chartlist (num+1) chartlist chartmap  in make_chartlist bd num count lightbluechart chartmap

make_onechartlist :: Int -> Int -> CP.Chart -> ChartMap -> ChartMap
make_onechartlist start end lbnode chartmap =
   let node = lookupChart start end lbnode
     in insert_chartlist 1 node chartmap

 
lookupChart :: Int -> Int -> CP.Chart -> [CCG.Node]
lookupChart bnum num lightbluechart = 
   let chart = Map.lookup (bnum, num) lightbluechart
     in chart2nodes chart

insert_chartlist :: Int -> [CCG.Node] -> ChartMap -> ChartMap
insert_chartlist num chartlist chartmap = case chartmap of 
   empty -> Map.insert num chartlist chartmap
   otherwise -> Map.insert (num + 1) chartlist chartmap


chart2nodes :: Maybe [CCG.Node] -> [CCG.Node]
chart2nodes nodes = case nodes of
       Nothing -> []
       Just nodes -> nodes

expresscat ::  [CCG.Node] -> CCG.Cat
expresscat node =
   let one = head node
    in CP.cat one
    
    
    
      