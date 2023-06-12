{-# LANGUAGE ExtendedDefaultRules  #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE ViewPatterns          #-}
{-# LANGUAGE TypeApplications #-}

import  qualified Data.Text as StrictT
import  qualified Data.Text.Lazy as T
--import  Data.String.IsString (Char)
import  qualified Data.Map as Map
import  qualified Data.Maybe as Maybe --base
import  Text.Blaze.Html.Renderer.String
import  System.IO.Unsafe (unsafePerformIO)
import qualified Interface.HTML as HTML
import  Interface.Text (SimpleText(..))
import  Interface.TeX (Typeset(..))
import  Parser.CCG (Node(..),RuleSymbol(..),Cat(..),isBaseCategory,Feature(..),FeatureValue(..))
import  qualified DTS.UDTT as UDTT
import  qualified DTS.UDTTwithName as UDWN
import  Yesod
import Text.Hamlet
import Text.Cassius
import  Text.Julius
import  qualified Lightblue as L
import  qualified Sentence_process as SP
import  qualified WidgetExpress as WE
import  Control.Monad (forM_)           --base
import  Control.Applicative

data App = App
  
  
mkYesod "App" [parseRoutes|
/ HomeR GET
/jsem/#String JsemR GET
/input InputR GET
|]

instance Yesod App

instance RenderMessage App FormMessage where
   renderMessage _ _ = defaultFormMessage
   

getHomeR :: Handler Html
getHomeR = do
  let count = 5
  defaultLayout $ do
   [whamlet|
       <body onLoad="koushi(#{count})">
          <form action=@{InputR}>
            <p>文を入力してね！
              <input type=text name=sen>
              <input type=submit value="送信">
              <h2>あ
              <canvas id="sample" width="500" height="500" style="background-color:yellow;">      
   |]
   myFunction
 

getJsemR :: String -> Handler Html
getJsemR var = do
   -- contentsはTest型
  let contents = SP.jsemSearch SP.jsemData var
-- preは[T.Text]型・hyはT.Text型
  let ans = show $ SP.answer_w contents
      pre = map T.fromStrict (SP.premises_w contents)
      hy = T.fromStrict $ SP.hypothesis_w contents
  if( pre == [] || hy == "" ) 
   then defaultLayout $ do [whamlet|<p><font color=red> Notfound.|]
   else do
     let psIOnode = map (L.parseSentence' 16 2) pre
     let ps = head <$> map unsafePerformIO psIOnode
     let m = unsafePerformIO $ L.parseSentence' 16 2 hy
     defaultLayout $ do
         [whamlet|
          <head>
               <title> #{var}
          <header>
              <b>[#{var}]</b>
                   <br>&ensp;answer : #{ans}
                   $forall pr <- pre
                        &ensp;premise : <span class="pre-under">#{pr}</span>
                   <br>&ensp;hypothesis : <span class="hy-under">#{hy}  
          <body>
               <main id="main">     
                 <input type="checkbox" id="cat-toggle"/>
                 <input type="checkbox" id="sem-toggle"/>
                 <label for="cat-toggle" id="catbtn"><b>&ensp;cat&ensp;&ensp;</b></label><br>
                 <label for="sem-toggle" id="sembtn"><b>&ensp;sem&ensp;</b></label>            
                      ^{mapM_ WE.widgetize $ ps}
                      ^{mapM_ WE.widgetize $ take 1 m}

         |]
         myDesign
         myFunction


     
getInputR :: Handler Html
getInputR = do
   sentence <- runInputGet $ SP.InputSentences
               <$> ireq textField "sen"
   let string_sen = SP.sentence_filter $ SP.input_Sentence sentence
   let a_sen = T.fromStrict $ SP.input_Sentence sentence
   let count = SP.sentence_filter_count $ SP.input_Sentence sentence
-- type Chart = M.Map (Int, Int) [CCG.Node] 
-- k：(Int, Int) , v：[CCG.Node] ???
   let chart = unsafePerformIO $ L.parse 32 a_sen
-- nodes は Maybe [CCG.Node]のはず
   let maybe_nodes = Map.lookup (0,2) chart
   let nodes = SP.chart2nodes maybe_nodes
   if nodes == [] then  defaultLayout $ do [whamlet| <h2> ノードないよ|]
    else do
      defaultLayout $ do
        [whamlet|
           <header>
              <p>&ensp;<b>入力文：#{a_sen}</b>
              <form action=@{InputR}>
                <p>&ensp;文を入力してね！
                   <input type=text name=sen>
                   <input type=submit value="送信">
           <body onLoad="koushi(#{count},#{string_sen})">
             <main id="main">
                <p>
                <canvas id="sample" width="1000" height="800" style="background-color:yellow;">
                <p>
                <input type="checkbox" id="cat-toggle"/>
                <input type="checkbox" id="sem-toggle"/>
                <label for="cat-toggle" id="catbtn"><b>&ensp;cat&ensp;&ensp;</b></label><br>
                <label for="sem-toggle" id="sembtn"><b>&ensp;sem&ensp;</b></label>       
                       ^{mapM_ WE.widgetize $ nodes}             
        |]
        myDesign
        myFunction


--CSS（cassius）
myDesign :: Widget
myDesign = do
   toWidget $(cassiusFile "templates/express.cassius")

--javascript（julius）
myFunction :: Widget
myFunction = do
    toWidget $(juliusFile "templates/express.julius")
   

 

main :: IO ()
main = warp 3000 App
 
 