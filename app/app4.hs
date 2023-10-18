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
--import  qualified Data.Text.Internal.Builder as BT
--import  Data.String.IsString (Char)
import  qualified Data.Map as Map
import  qualified Data.Maybe as Maybe --base
--import  Text.Blaze.Html.Renderer.String
import  System.IO.Unsafe (unsafePerformIO)
import  Yesod
import  Text.Hamlet
import  Text.Cassius
import  Text.Julius
import  qualified Lightblue as L
import  qualified Sentence_process as SP
--import qualified Parser.CCG as CCG
import  qualified WidgetExpress as WE
import  Control.Monad (forM_)           --base
import  Control.Applicative

data App = App
  
  
mkYesod "App" [parseRoutes|
/ HomeR GET
/jsem/#String JsemR GET
/input InputR GET
--/chart ChartR GET
--/link LinkR GET
--/chart/sentence/#T.Text/beam/#Int/senstart/#Int/senend/#Int ChartR GET
/chart/#String/#Int/#Int/#Int ChartR GET
|]

instance Yesod App
--      approot = ApprootStatic "http://localhost:3000"

instance RenderMessage App FormMessage where
   renderMessage _ _ = defaultFormMessage
   

getHomeR :: Handler Html
getHomeR = do
  --(widget, enctype) <- generateFormPost personForm
  defaultLayout $ do
   [whamlet|
      <header class="home_header">
              <h2>日本語CCG ChartParser
      <body class="home_body">
               例文：私は花子だ。
          <p>
               (開始位置, 終了位置)
          <p>
               この例文の（0,1）は 「私」
          <p>
               この例文の（2,5）は「花子だ」
          <div class="home_btn">
             <form action=@{InputR}>
                   入力文
                   <input type=text name=sen>
               <p>    
                   beam
                   <input type=string name=sen_b class="number_input">
               <p>     
                   <input type=submit value="Let's ChartParser !" class="input_submit">
                  
   |]
   myDesign
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
          <header class="ccg_header">
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
                         <*> ireq intField "sen_b"
   let string_sen = SP.sentence_filter $ SP.input_Sentence sentence
   let text_sen = T.fromStrict $ SP.input_Sentence sentence
--"。"を取り除いた文字数
   let count = SP.sentence_filter_count $ SP.input_Sentence sentence
   let beam = SP.sen_beam sentence
   let chart = unsafePerformIO $ L.parse beam text_sen
--   type ChartMap = Map.Map Int [CCG.Node]
   let chartmap = SP.make_onechartlist 0 1 chart Map.empty
   let maybechart = Map.lookup 1 chartmap
   let charts = SP.chart2nodes maybechart
   let cat = SP.expresscat charts
   --putStrLn chart
   if charts == [] then  defaultLayout $ do [whamlet| <h2>ないよ|]
   else
      defaultLayout $ do
        addScriptRemote "https://code.createjs.com/1.0.0/createjs.min.js"
        [whamlet|
           <header class="ccg_header">
               <p>&ensp;<b>入力文：#{text_sen}</b> 
               <p>&ensp; 現在のbeam : #{beam}
           <body onLoad="express_chart(#{count},#{string_sen},#{beam})">
              <main id="main">
                <p>
                <canvas id="canvas" width="1200" height="1200">
                <div id="chartexpress">
                      #{show cat}
                      ^{mapM_ WE.widgetize $ charts}
        |]
        myDesign
        myFunction
 





getChartR :: String -> Int -> Int -> Int -> Handler Html
getChartR sentences beams senS senE = do
    -- T.pack : String -> Text
     let lazytext_sen = T.pack $ sentences
     --let int_beam = StrictT.length $ StrictT.pack $ stringbeams
    -- chart :: [CCG.Node]
     --let chart = unsafePerformIO $ L.parse int_beam lazytext_sen
     let chart = unsafePerformIO $ L.parse beams lazytext_sen
     -- maybe_nodes : Maybe [CCG.Node]
     let maybe_nodes = Map.lookup (senS,senE) chart
    -- nodes : [CCG.Node]
     let nodes = SP.chart2nodes maybe_nodes    
     defaultLayout $ do
        addScriptRemote "https://code.createjs.com/1.0.0/createjs.min.js"
        [whamlet|
           <body>
              <main id="main">
                <input type="checkbox" id="cat-toggle"/>
                <input type="checkbox" id="sem-toggle"/>
                <label for="cat-toggle" id="catbtn"><b>&ensp;cat&ensp;&ensp;</b></label><br>
                <label for="sem-toggle" id="sembtn"><b>&ensp;sem&ensp;</b></label>
                     <div id="chartexpress">
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
 
 