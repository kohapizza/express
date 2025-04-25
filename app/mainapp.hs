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
import qualified DTS.NaturalLanguageInference as NLI
import qualified Parser.ChartParser as CP
import Parser.Language (jpOptions)
import qualified Parser.Language.Japanese.Lexicon (lexicalResourceBuilder)
import qualified System.IO as S
import qualified Interface as I
import Debug.Trace
import ListT (ListT(..),fromFoldable,toList) --list-t
import Data.List
import Debug.Trace

data App = App
  
  
mkYesod "App" [parseRoutes|
/ HomeR GET
/jsem/ JsemR GET
/input InputR GET
/chart/#String/#Int/#Int/#Int ChartR GET
/diagram/#String/#Int DiagramR GET
/jsemMenu JSeMMenuR GET
/parsingMenu ParsingMenuR GET
/parsing ParsingR GET
/chartParsingMenu ChartParsingMenuR GET
/error ErrorR GET
/not-found NotFoundR GET
|]

instance Yesod App where
        errorHandler NotFound = fmap toTypedContent $ defaultLayout $ do
                setTitle "Request page not located"
                toWidget [hamlet|
                                      <h1>Not Found
                                      <p> the request page could not located
                |]
        errorHandler other = defaultErrorHandler other


instance RenderMessage App FormMessage where
   renderMessage _ _ = defaultFormMessage

getHomeR :: Handler Html
getHomeR = do
     defaultLayout $ do
          addStylesheetRemote "https://fonts.googleapis.com/css2?family=Alumni+Sans+Pinstripe:ital@0;1&display=swap"
          addStylesheetRemote "https://fonts.googleapis.com/css2?family=Audiowide&display=swap"
          addScriptRemote "https://code.createjs.com/1.0.0/createjs.min.js"
          [whamlet|
            <div class="header">
              <h1>
                <a href=@{HomeR} class="home-link">express
            <div class="container">
              <div class="title">What is express?
              <div class="description">
                express is the development support tool for the Japanese inference system "lightblue". 
                <br> With express, you can parse sentences, perform natural language inference, and perform type checking.
              <div class="cards">
                <a href=@{JSeMMenuR} class="card">
                  <h3>JSeM
                  <p>Use JSeM data and get its inference results
                  <img src="https://cdn.glitch.global/54cdd4d5-e51e-4420-ab33-9b872b90505a/Group%205.png?v=1735105638686" alt="JSeM">
                <a href=@{ParsingMenuR} class="card">
                  <h3>Parsing
                  <p>Parse sentence(s) and get its result(s)
                  <img src="https://cdn.glitch.global/54cdd4d5-e51e-4420-ab33-9b872b90505a/Group%206.png?v=1735105643759" alt="Parsing">
                <a href=@{ChartParsingMenuR} class="card">
                  <h3>Chart Parsing
                  <p>Parse a sentence and get its parsing chart
                  <img src="https://cdn.glitch.global/54cdd4d5-e51e-4420-ab33-9b872b90505a/Group%207.png?v=1735105856172" alt="ChartParsing">
          |]
          myDesign
          myFunction
          toggleJS

getJSeMMenuR :: Handler Html 
getJSeMMenuR = do
     defaultLayout $ do
          addStylesheetRemote "https://fonts.googleapis.com/css2?family=Audiowide&display=swap"
          addStylesheetRemote "https://fonts.googleapis.com/css2?family=Alumni+Sans+Pinstripe:ital@0;1&display=swap"
          [whamlet|
            <div class="header">
              <h1>
                <a href=@{HomeR} class="home-link">express
              <a href=@{HomeR} class="homelink">
            <div class="container">
              <div class="title">JSeM
              <div class="description">
                Enter JSeMID to display the Node of the hypotheses and the premise. 
                <br>If the inference is successful, you can also view the proof diagram.
              <section class="parsingButton">
                <form action=@{JsemR} > 
                  <input name="jsem_id" type="text" class="parsing-input" placeholder="JSeM ID" required>
                  <input type="submit" class="btn_parse" value="Parse!" />
          |]
          myDesign
          myFunction
          toggleJS

getParsingMenuR :: Handler Html
getParsingMenuR = do
     defaultLayout $ do
          addStylesheetRemote "https://fonts.googleapis.com/css2?family=Alumni+Sans+Pinstripe:ital@0;1&display=swap"
          addStylesheetRemote "https://fonts.googleapis.com/css2?family=Audiowide&display=swap"
          addScriptRemote "https://code.createjs.com/1.0.0/createjs.min.js"
          [whamlet|
            <div class="header">
              <h1>
                <a href=@{HomeR} class="home-link">express
              <a href=@{HomeR} class="homelink">
            <div class="container">
              <div class="title">Parsing
              <div class="description">
                If you enter the number of beams and the sentence you want to parse, you can see the syntactic information and semantic display. type check is also available, and if type check is successful, you can see the proof diagram.
              <section class="parsingButton">
                <form action=@{ParsingR} >
                  <input name="sen" type="text" class="parsing-input" placeholder="Sentence" required>   
                  <input name="sen_b" type="text" class="parsing-input" placeholder="Beam width" required>
                  <input type="submit" class="btn_parse" value="Parse!" />
          |]
          myDesign
          myFunction
          toggleJS


getParsingR :: Handler Html
getParsingR = do
     sentence <- runInputGet $ SP.InputSentences
                         <$> ireq textField "sen"
                         <*> ireq intField "sen_b"
     let text_sen = T.fromStrict $ SP.input_Sentence sentence
     let beam = SP.sen_beam sentence
     let tabs = [1..beam]
     -- tabs個のnode
     -- parseSentence' :: Int -> Int -> T.Text -> IO ([CCG.Node])
     nodes <- liftIO $ L.parseSentence' beam beam text_sen

     -- tabs個のType Check Query
     -- parseSentenceForQuery :: Int -> Int -> T.Text -> IO ([UDTT.TypeCheckQuery])
     tcqs <- liftIO $ L.parseSentenceForQuery beam beam text_sen
     
     -- tcds 
     tcds <- liftIO $ L.parseSentenceForDiagram beam beam text_sen
     let tabClasses = map (\tcdList -> if null tcdList then "tab-label error" else "tab-label") tcds

     defaultLayout $ do
          addStylesheetRemote "https://fonts.googleapis.com/css2?family=Alumni+Sans+Pinstripe:ital@0;1&display=swap"
          addStylesheetRemote "https://fonts.googleapis.com/css2?family=Audiowide&display=swap"
          [whamlet|
            <div class="header">
              <div class="parsing-content">
                <p>sentence: #{text_sen}
                <br>beam: #{beam}
            <div class="container-tab">
              <div .tab-wrap>
                $forall (tabNum, node, tcq, tcdList, tabClass) <- Data.List.zip5 tabs nodes tcqs tcds tabClasses
                  <input id="TAB-#{tabNum}" type="radio" name="TAB" class="tab-switch" :tabNum == 1:checked>
                  <label for="TAB-#{tabNum}" class=#{tabClass}>#{tabNum}
                  <div class="tab-content">
                    <div class="tab-node">
                      <h1>Node
                      <div class="tab-node-content">^{WE.widgetize node}
                    <div class="tab-tcq">
                      <h1>Type Check Query
                      <div class="tab-tcq-content">^{WE.widgetize tcq}
                    <div class="tab-tcd">
                      <h1>Type Check Diagram
                      $if null tcdList
                        <p .error-message>⚠️ Type Check Failed... ⚠️
                      $else
                        <div class="tab-tcds-content">^{mapM_ WE.widgetize $ Prelude.take 1 tcdList}
          |]
          myFunction
          toggleJS
          myDesign

getChartParsingMenuR :: Handler Html
getChartParsingMenuR = do
     defaultLayout $ do
          addStylesheetRemote "https://fonts.googleapis.com/css2?family=Alumni+Sans+Pinstripe:ital@0;1&display=swap"
          addStylesheetRemote "https://fonts.googleapis.com/css2?family=Audiowide&display=swap"
          [whamlet|
            <div class="header">
              <h1>
                <a href=@{HomeR} class="home-link">express

            <div class="container">
              <div class="title">Chart Parsing
              <div class="description">
                Enter the sentence and beam to be parsed, and left-corner chart parsing is performed. You can view the chart.
          |]
          myDesign
          myFunction
          toggleJS

-- 引数はjsem_id
-- ここにproof search diagramも表示させたい
-- つまり、discource :: [T.Text] をparseWithTypeCheckに渡して証明図を可視化
getJsemR :: Handler Html
getJsemR = do
  -- jsem_id :: StrictT.Text
  jsem_id <- runInputGet $ ireq textField "jsem_id"
  let var = StrictT.unpack jsem_id
   -- contentsはTest型
   -- jsemData :: [C.JSeMData]
   -- jsemSearch :: [C.JSeMData] -> String -> Test
  let contents = SP.jsemSearch SP.jsemData var
  -- preは[T.Text]型・hyはT.Text型
  let ans = show $ SP.answer_w contents
      pre = map T.fromStrict (SP.premises_w contents) --前提
      hy = T.fromStrict $ SP.hypothesis_w contents --仮説
      discourse = pre ++ [hy] -- [前提, 仮説]
  parseResult <- liftIO $ L.parseWithTypeCheck3 discourse
  proofSearchDiagrams <- liftIO $ toList $ L.getProofSearchDiagram parseResult

  if( pre == [] || hy == "" ) 
   then defaultLayout $ do [whamlet|<p><font color=red> Notfound.|]
  else if  (null proofSearchDiagrams)
     then do 
       -- psIOnode :: [IO([CCG.Node])]
       let psIOnode = map (L.parseSentence' 16 2) pre
       -- ps :: [CCG.Node] psIONodeの各要素からIOとって１個目の要素を得る
       let ps = head <$> map unsafePerformIO psIOnode
       -- m :: [CCG.Node] 
       let m = unsafePerformIO $ L.parseSentence' 16 2 hy
       defaultLayout $ do
          [whamlet|
            <head>
               <title> #{var}
            <header class="ccg_header">
              <div class="header-content">
                <b>jsemID: [#{var}]</b>
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

                  <div class="jsem-pre">
                    <h1>Premise-Node
                    <div class="jsem-pre-node">^{mapM_ WE.widgetize $ ps}
                  <div class="jsem-hy">
                    <h1>Hypothesis-Node
                    <div class="jsem-hy-node">^{mapM_ WE.widgetize $ take 1 m}
                  <div class="jsem-psd">
                    <h1>Proof Search Diagram
                    <p .error-message>⚠️ Proof Search Failed... ⚠️
          |]
          myDesign
          myFunction
          toggleJS
   else do
     -- psIOnode :: [IO([CCG.Node])]
     let psIOnode = trace ("proof search finished") map (L.parseSentence' 16 2) pre
     -- ps :: [CCG.Node] psIONodeの各要素からIOとって１個目の要素を得る
     let ps = head <$> map unsafePerformIO psIOnode
     -- m :: [CCG.Node] 
     let m = unsafePerformIO $ L.parseSentence' 16 2 hy
     defaultLayout $ do
         [whamlet|
          <head>
               <title> #{var}
          <header class="ccg_header">
            <div class="header-content">
              <b>jsemID: [#{var}]</b>
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

                 <div class="jsem-pre">
                   <h1>Premise-Node
                   <div class="jsem-pre-node">^{mapM_ WE.widgetize $ ps}
                 <div class="jsem-hy">
                   <h1>Hypothesis-Node
                   <div class="jsem-hy-node">^{mapM_ WE.widgetize $ take 1 m}
                 <div class="jsem-psd">
                   <h1>Proof Search Diagram
                   <div class="jsem-psd-diagram">^{mapM_ WE.widgetize $ take 1 proofSearchDiagrams}
         |]
         myDesign
         myFunction
         toggleJS

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
   let nodelist = SP.chart2nodelist count (count-1) chart [] 
   let dtr_nodes = map SP.expressDtrs nodelist
   let cat_nodes = map SP.expressCat nodelist
   let sem_nodes = map SP.expressSem nodelist
   let sig_nodes = map SP.expressSig nodelist
   let score_nodes = map SP.expressScore nodelist
   let addspace_cats = SP.stlist2string cat_nodes
   let addspace_scores = SP.scorelist2string score_nodes
   let html_cats = map toHtml addspace_cats
   let cat_length = length cat_nodes
   if cat_nodes == [] then  defaultLayout $ do [whamlet| <h2>ないよ|]
   else
      defaultLayout $ do
        addScriptRemote "https://code.createjs.com/1.0.0/createjs.min.js"
        [whamlet|
           <header class="ccg_header">
               <p>&ensp;<b>Sentence: #{text_sen}</b> 
               <p>&ensp;beam : #{beam}
           <body onLoad="express_chart(#{count},#{string_sen},#{beam},#{html_cats},#{addspace_scores})">
              <main id="main">
                <canvas id="canvas" width="1500" height="2000">
                 <ul> 
        |]
        myDesign
        toggleJS
        chartParsingJS


getChartR :: String -> Int -> Int -> Int -> Handler Html
getChartR sentences beams senS senE = do
    -- T.pack : String -> Text
     let lazytext_sen = T.pack $ sentences
     -- chart :: CP.Chart
     let chart = trace ("getChartR") unsafePerformIO $ L.parse beams lazytext_sen
     -- maybe_nodes : Maybe [CCG.Node]
     -- (senS, senE)に対応する[CCG.Node]を取り出す
     let maybe_nodes = trace ("maybe_nodes") Map.lookup (senS,senE) chart
    -- maybe_nodes2nodes :: Maybe [CCG.Node] -> [CCG.Node]
     let nodes = trace ("nodes") SP.maybe_nodes2nodes maybe_nodes    
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
        chartParsingJS
        toggleJS


getDiagramR :: String -> Int -> Handler Html
getDiagramR sentences beams = do
   let sentence' = T.pack $ sentences
   parseResult <- liftIO $ L.parseWithTypeCheck beams sentence'
   let diagrams = L.getTypeCheckDiagram parseResult
   defaultLayout $ do
        if null diagrams
            then [whamlet|
                    <body>
                      <p>解析結果が見つかりませんでした。
                 |]
            else [whamlet|
                    <body>
                      <p>解析結果：
                      <p>^{mapM_ WE.widgetize diagrams}
                 |]
        myDesign
        myFunction
        toggleJS

    
--CSS（cassius）
myDesign :: Widget
myDesign = do
   toWidget $(cassiusFile "templates/express.cassius")

--javascript（julius）
myFunction :: Widget
myFunction = do
    toWidget $(juliusFile "templates/express.julius")

-- chartParsing
chartParsingJS :: Widget
chartParsingJS = do
     toWidget $(juliusFile "templates/chartparsing.julius")

-- 共通
toggleJS :: Widget
toggleJS = do
     toWidget $(juliusFile "templates/toggle.julius")
   
getErrorR :: Handler ()
getErrorR = error "This is an error"

getNotFoundR :: Handler ()
getNotFoundR = notFound

main :: IO ()
main = warp 3000 App
 
