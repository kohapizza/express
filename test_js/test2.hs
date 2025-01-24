{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE QuasiQuotes          #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE ViewPatterns         #-}

import qualified Data.Text as StrictT
import qualified Data.Text.Lazy as T
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe --base
import System.IO.Unsafe (unsafePerformIO)
import qualified Lightblue as L
import Interface.Text (SimpleText(..))
import Interface.TeX (Typeset(..))
import Parser.CCG (Node(..),RuleSymbol(..),Cat(..),isBaseCategory,Feature(..),FeatureValue(..))
import Yesod
import qualified Text.Julius as J

data App = App

mkYesod "App" [parseRoutes|
/ HomeR GET
/examples/#Int Examples1R GET
|]

--文探索
type Sentence = T.Text

type SentenceMap = Map.Map Int Sentence

sentences :: SentenceMap
sentences = Map.fromList
  [(1, "ああああ")
  ,(2, "いいいい")
  ,(3, "おはよう")
  ,(4, "僕は学生だ")
  ,(5, "私は猫と住みたい")
  ,(6, "犬が猫を追いかける")
  ,(7, "二匹の羊が寝ている")
  ,(8, "白い猫と黒い猫がいる")
  ,(9, "家にテレビがある")
  ,(10, "庭に花が咲く")
  ]

sentenceLookup :: Int -> SentenceMap -> Either Sentence Sentence
sentenceLookup number map = case Map.lookup number map of
  Nothing -> Left $ "存在しません"
  Just sentence -> Right sentence 


--Either型からString型にする
eithertostring :: Either Sentence Sentence -> Sentence
eithertostring result =
  case result of Left sentence -> sentence
                 Right sentence -> sentence


instance Yesod App

getExamples1R :: Int -> Handler Html
getExamples1R num = 
   do 
    let s = eithertostring (sentenceLookup num sentences)
    defaultLayout $ do
      btn <- newIdent
      toWidget [julius|
          function toggle(id){
            var objID1 = document.getElementById( id + "layerA" );
            var objID2 = document.getElementById( id + "layerB" );
            var buttonID = document.getElementById( id + "button" );
            if(objID1.className=='close') {
              objID1.style.display = 'block';
              objID1.className = 'open';
              objID2.style.display = 'none';
              objID2.className = 'close';
              buttonID.innerHTML = "-";
              }else{
              objID1.style.display = 'none';
              objID1.className = 'close';
              objID2.style.display = 'block';
              objID2.className = 'open';
              buttonID.innerHTML = "+";
            }};
          |]     
      toWidget [hamlet|
            <button type="button" id=#{StrictT.concat [btn, "button"]} onclick="toggle('#{btn}')"> - 
            <div id=#{StrictT.concat [btn, "layerA"]} style="display: block" class="open">
              <table border="1" rules="rows" frame="void" cellpadding="5">
               <tr>
                   <td>John
                   <td>runs
               <tr>
                   <td>NP
                   <td>S¥NP
               <tr class=denom>
                   <td colspan="3" align="center">S
            <div id=#{StrictT.concat [btn, "layerB"]} style="display: none" class="close">
              <table border="1" rules="rows" frame="void" cellpadding="5">
               <tr class=numer>
                   <td>
                     John
                   <td>
                     runs
               <tr class=denom>
                  <td colspan="3" align="center">S
         |]

myLayout :: Widget
myLayout  = do
        aaa <- newIdent
        toWidget [lucius|
                     .#{aaa}{
		      color : red
		      
		     }
	            {-  body {
		       font-family: verdana
		       
		     } -}
	         |]
        toWidget [hamlet|<h1>こんにちは
         |]
        

getHomeR :: Handler Html
getHomeR = defaultLayout $ do
  setTitle "Page title"
  myLayout


main :: IO ()
main = warp 3000 App
