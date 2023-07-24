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
--import  Text.Blaze.Html.Renderer.String
import  System.IO.Unsafe (unsafePerformIO)
import  Yesod
import  Text.Hamlet
import  Text.Cassius
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
--/link LinkR GET
--/chart/#T.Text/#Int/#Int ChartR GET
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
                   <input type=int name=sen_b class="number_input">
               <p>     
                   <input type=submit value="Let's ChartParser !" class="input_submit">
                  
   |]
   myDesign
   myFunction

--sentenceForm :: Html -> MForm Handler (FormResult InputSentences, Widget)
--sentenceForm = renderDivs $ InputSentences
 --   <$> areq textField "sen" Nothing
--    <*> areq intField "sen_s" 0
--    <*> areq intField "sen_e" 4
--    <*> areq urlField "beam" 32


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
--                         <*> ireq intField "sen_s"
--                         <*> ireq intField "sen_e"
                         <*> ireq intField "sen_b"
--   homepath <- getResourcePath HomeR
--   pagepath1 <- getResourcePath $ JsemR String
--   pagepath2 <- getResourcePath $ ChartR String Int Int
   let string_sen = SP.sentence_filter $ SP.input_Sentence sentence
   let a_sen = T.fromStrict $ SP.input_Sentence sentence
   let count = SP.sentence_filter_count $ SP.input_Sentence sentence
 --  let list = senList;
{-   render <- getUrlRender
   render2 <- getUrlRenderParams
   path   <- getResourcePath HomeR
   path2 <- return $ render InputR
   path3 <- return $ render2 a_sen 0 4 ChartR -}
-- type Chart = M.Map (Int, Int) [CCG.Node] 
-- k：(Int, Int) , v：[CCG.Node] ???
   let beam = SP.sen_beam sentence
-- chart :: [CCG.Node]
   let chart = unsafePerformIO $ L.parse beam a_sen
-- nodes は Maybe [CCG.Node]のはず
   --let maybe_nodes = Map.lookup (SP.sen_start sentence, SP.sen_end sentence) chart
   let maybe_nodes = Map.lookup (0,1) chart
   let nodes = SP.chart2nodes maybe_nodes
   if nodes == [] then  defaultLayout $ do [whamlet| <h2> ノードないよ|]
   else do
      defaultLayout $ do
        addScriptRemote "https://code.createjs.com/1.0.0/createjs.min.js"
        [whamlet|
           <header class="ccg_header">
               <p>&ensp;<b>入力文：#{a_sen}</b> &ensp; 現在のbeam : #{beam}
                 <form action=@{InputR}>
                     <p>
                     beam
                     <input type=int name=sen_b class="number_input">
                     <input type=submit value="Let's ChartParser !" class="input_submit">
           <body onLoad="express_chart(#{count},#{string_sen})">
              <main id="main">
                <p>
                <canvas id="canvas" width="1000" height="800">
                <p>
                     <div id="chartexpress">
                                    <!--^{mapM_ WE.widgetize $ nodes}-->
        |]
        myDesign
        myFunction
 


{-getChartR :: T.Text -> Int -> Int -> Handler Html
getChartR :: Handler Html
getChartR sentence senS senE = do
     let chart = unsafePerformIO $ L.parse 32 sentence
     -- nodes は Maybe [CCG.Node]のはず
     --let maybe_nodes = Map.lookup (SP.sen_start sentence, SP.sen_end sentence) chart
     let maybe_nodes = Map.lookup (senS,senE) chart
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
        myFunction -}
        

    
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
 
 