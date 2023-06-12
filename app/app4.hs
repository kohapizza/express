{-# LANGUAGE ExtendedDefaultRules  #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE ViewPatterns          #-}

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
--import  WidgetExpress(nodewidget)
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
                      ^{mapM_ widgetize $ ps}
                      ^{mapM_ widgetize $ take 1 m}

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
   let chart = unsafePerformIO $ L.parse 5 a_sen
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
                       ^{mapM_ widgetize $ nodes}             
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
 
 
class Widgetizable a where
  widgetize :: a -> Widget

instance Widgetizable T.Text where
  widgetize = toWidget 

instance Widgetizable Node where
  widgetize node = case daughters node of
    [] -> do
      id <- newIdent
      [whamlet|
        <table>
          <tr valign="bottom">
            <td valign="baseline">
              <table border="1" rules="rows" frame="void" cellpadding="2">
                <tr>
                  <td align="center" bgcolor="#ffd700">#{pf node}
                <tr>
                  <td align="center">
                    <table border="0" cellpadding="0">
                      <tr class="cathide">
                        <td align="center">
                          <math xmlns='http://www.w3.org/1998/Math/MathML'>^{widgetize $ cat node}
                      <tr class="semhide">
                        <td align="center">
                          <math xmlns='http://www.w3.org/1998/Math/MathML'>^{widgetize $ sem node}
            <td valign="baseline">
              <span>LEX
        |]
    dtrs -> do
      let len = (length dtrs)*2
      id <- newIdent
      [whamlet|
         <table>
          <tr valign="bottom">
            <td valign="baseline">
              <div id=#{StrictT.concat [id, "layerA"]} style="display: block" class="open">
                <table border="1" rules="rows" frame="void" cellpadding="2">
                  <tr valign="bottom">
                    $forall dtr <- dtrs
                      <td align="center" valign="bottom">^{widgetize dtr}&nbsp;
                  <tr>
                    <td align="center" colspan=#{len}>
                      <table border="0" cellpadding="0">
                        <tr class="cathide">
                          <td align="center">
                            <math xmlns='http://www.w3.org/1998/Math/MathML'>^{widgetize $ cat node}
                        <tr class="semhide">
                          <td align="center">
                            <math xmlns='http://www.w3.org/1998/Math/MathML'>^{widgetize $ sem node}
              <div id=#{StrictT.concat [id, "layerB"]} style="display: none" class="close">
                <table border="2" rules="rows" cellpadding="5" border="3px solid #808080">
                  <tr>
                    <td bgcolor="#ffd700">^{widgetize $ pf node}
                  <tr>
                    <td align="center" colspan=#{len}>
                      <table border="0" cellpadding="0">
                        <tr class="cathide">
                          <td align="center">
                            <math xmlns='http://www.w3.org/1998/Math/MathML'>^{widgetize $ cat node}
                        <tr class="semhide">
                          <td align="center">
                            <math xmlns='http://www.w3.org/1998/Math/MathML'>^{widgetize $ sem node}
            <td valign="baseline">
              <table border="1" rules="rows" frame="void" cellpadding="5">
                <tr>
                  <td>
                   <button type="button" class="btn-design" id=#{StrictT.concat [id, "button"]} onclick=toggle('#{id}')>-
                  <span>^{widgetize $ rs node}
        |]



instance Widgetizable RuleSymbol where
  widgetize rs = [whamlet|#{toText rs}|]

instance Widgetizable Cat where
  widgetize category = case category of
    SL x y      -> [whamlet|<mrow>
                              ^{widgetize x}
                              <mo>/
                              ^{widgetize' y}
                              |]
    BS x y      -> [whamlet|<mrow>
                              ^{widgetize x}
                              <mo>\
                              ^{widgetize' y}
                              |]
    T True i _  -> [whamlet|<msub>
                              <mi>T
                              <mn>#{T.pack $ show i}
                              |]
    T False i u -> [whamlet|<msub>
                              ^{widgetize' u}
                              <mn>:[#{T.pack $ show i}]
                              |]
    S (pos:(conj:pm)) -> let x = toText pm
                             nullx = T.null x in
                         [whamlet|
                           <msub>
                             <mi>S
                             <mstyle color='Purple'>
                               <mtable columnalign='left'>
                                 <mtr>
                                   <mtd>^{widgetize pos}
                                 <mtr>
                                   <mtd>
                                     <mpadded height='-0.5em'>^{widgetize conj}
                                 <mtr>
                                   <mtd>
                                     <mpadded height='-0.5em'>^{widgetize pm}
                                 |]
    NP [cas]    -> [whamlet|<msub>
                              <mi>NP
                              <mtext color='Purple'>^{widgetize cas}
                           |]
    Sbar [sf]   -> [whamlet|<msub>
                              <menclose notation='top'>
                                <mi>S
                              <mtext color='Purple'>^{widgetize sf}
                           |]
    N           -> [whamlet|<mi>N|]
    CONJ        -> [whamlet|<mi>CONJ|]
    LPAREN      -> [whamlet|<mi>LPAREN|]
    RPAREN      -> [whamlet|<mi>RPAREN|]
    _           -> [whamlet|<mtext>Error: #{toText category}|]
    where widgetize' c = if isBaseCategory c 
                           then widgetize c
                           else [whamlet|<mrow>
                                           <mo>(
                                           ^{widgetize c}
                                           <mo>)
                                           |]

instance Widgetizable Feature where 
  widgetize (SF i f) = [whamlet|
                          <mtext>
                            #{toTeX f}
                          <mo>:
                          <mn>[#{T.pack (show i)}]
                          |]
  widgetize (F f) = [whamlet|<mtext>#{toTeX f}|]

instance Widgetizable [Feature] where
  widgetize pmfs = [whamlet|
                     #{T.intercalate "," $ Maybe.catMaybes $ pmfs2MathMLLoop ["t","p","n","N","T"] pmfs}
                     |]

pmfs2MathMLLoop :: [T.Text] -> [Feature] -> [Maybe T.Text]
pmfs2MathMLLoop labels pmfs = case (labels,pmfs) of
  ([],[])         -> []
  ((l:ls),(p:ps)) -> (pmf2MathML l p):(pmfs2MathMLLoop ls ps)
  _ -> [Just $ T.concat ["Error: mismatch in ", T.pack (show labels), " and ", T.pack (show pmfs)]]

pmf2MathML :: T.Text -> Feature -> Maybe T.Text
pmf2MathML label pmf = case (label,pmf) of
  (l,F [P])   -> Just $ T.concat ["+", l]
  (_,F [M])   -> Nothing -- if shared then Just $ T.concat ["{-}", l] else Nothing
  (l,F [P,M]) -> Just $ T.concat ["±", l]
  (l,F [M,P]) -> Just $ T.concat ["±", l]
  (l,SF i f)  -> do
                 x <- pmf2MathML l (F f)
                 return $ T.concat [x, ":[", T.pack $ show i, "]"]
  _ -> return $ T.pack "Error: pmf2MathML"

instance Widgetizable UDTT.Preterm where
  widgetize = widgetize . UDTT.initializeIndex . (UDTT.fromDeBruijn [])

instance Widgetizable UDWN.VarName where
  widgetize (UDWN.VarName v i) =
    [whamlet|
      <msub>
        <mi>#{T.singleton v}
        <mn>#{T.pack (show i)}
        |]

instance Widgetizable UDWN.Preterm where
  widgetize preterm = case preterm of
    UDWN.Var vname -> widgetize vname
    UDWN.Con cname -> [whamlet|<mtext>#{cname}|]
    UDWN.Type -> [whamlet|<mi>type|]
    UDWN.Kind -> [whamlet|<mi>kind|]
    UDWN.Pi vname a b -> [whamlet|
      <mrow>
        <mo>(
        ^{widgetize vname}
        <mo>:
        ^{widgetize a}
        <mo>)
        <mo>&rarr;
        ^{widgetize b}
        |]
    UDWN.Not a -> [whamlet|
      <mrow>
        <mo>&not;
        <mi>toMathML a
        |]
    UDWN.Lam vname m -> [whamlet|
      <mrow>
        <mi>&lambda;
        ^{widgetize vname}
        <mpadded lspace='-0.2em' width='-0.2em'>
          <mo>.
        ^{widgetize m}
        |]
    UDWN.App (UDWN.App (UDWN.Con cname) y) x -> [whamlet|
      <mrow>
        <mtext>#{cname}
        <mo>(
        ^{widgetize x}
        <mo>,
        ^{widgetize y}
        <mo>)
        |]
    UDWN.App (UDWN.App (UDWN.App (UDWN.Con cname) z) y) x -> [whamlet|
      <mrow>
        <mtext>#{cname}
        <mo>(
        ^{widgetize x}
        <mo>,
        ^{widgetize y}
        <mo>,
        ^{widgetize z}
        <mo>)
        |]
    UDWN.App (UDWN.App (UDWN.App (UDWN.App (UDWN.Con cname) u) z) y) x -> [whamlet|
      <mrow>
        <mtext>#{cname}
        <mo>(
        ^{widgetize x}
        <mo>,
        ^{widgetize y}
        <mo>,
        ^{widgetize z}
        <mo>,
        ^{widgetize u}
        <mo>)
        |]
    UDWN.App m n -> [whamlet|
      <mrow>
        ^{widgetize m}
        <mo>(
        ^{widgetize n}
        <mo>)
        |]
    UDWN.Sigma vname a b -> case b of 
      UDWN.Top -> widgetize  a
      _   -> [whamlet|
        <mrow>
          <mo>[
          <mtable>
            <mtr>
              <mtd columnalign="left">
                <mrow>
                  ^{widgetize vname}
                  <mo>:
                  ^{widgetize a}
            <mtr>
              <mtd columnalign="left">
                <mpadded height='-0.5em'>^{widgetize b}
          <mo>]
        |]
    UDWN.Pair m n  -> [whamlet|
      <mrow>
        <mo>(
        ^{widgetize m}
        <mo>,
        ^{widgetize n}
        <mo>)
        |]
    UDWN.Proj s m  -> [whamlet|
      <mrow>
        <msub>
          <mi>&pi;
          <mi>#{toText s}
        <mo>(
        ^{widgetize m}
        <mo>)
        |]
    UDWN.Lamvec vname m  -> [whamlet|
      <mrow>
        <mi>&lambda;
        <mover>
          ^{widgetize vname}
          <mo>&rarr;
        <mo>.
        ^{widgetize m}
        |]
    UDWN.Appvec vname m -> [whamlet|
      <mrow>
        ^{widgetize m}
        <mover>
          ^{widgetize vname}
          <mo>&rarr;
          |]
    UDWN.Unit       -> [whamlet|<mi>()|]
    UDWN.Top        -> [whamlet|<mi>&top;|]
    UDWN.Bot        -> [whamlet|<mi>&bot;|]
    UDWN.Asp j m    -> [whamlet|
      <mrow>
        <msub>
          <mo>@
          <mn>#{T.pack (show j)}
        <mo>:
        ^{widgetize m}
      |]
    UDWN.Nat    -> [whamlet|<mi>N|]
    UDWN.Zero   -> [whamlet|<mi>0|]
    UDWN.Succ n -> [whamlet|
      <mrow>
        <mi>s
        ^{widgetize n}
        |]
    UDWN.Natrec n e f -> [whamlet|
      <mrow>
        <mi>natrec
        <mo>(
        ^{widgetize n}
        <mo>,
        ^{widgetize e}
        <mo>,
        ^{widgetize f}
        <mo>)
        |]
    UDWN.Eq a m n -> [whamlet|
      <mrow>
        ^{widgetize m}
        <msub>
          <mo>=
          ^{widgetize a}
        ^{widgetize n}
        |]
    UDWN.Refl a m -> [whamlet|
      <mrow>
        <mi>refl
        ^{widgetize a}
        <mo>(
        ^{widgetize m}
        <mo>)
        |]
    UDWN.Idpeel m n -> [whamlet|
      <mrow>
        <mi>idpeel
        <mo>(
        ^{widgetize m}
        <mo>,
        ^{widgetize n}
        <mo>)
        |]
