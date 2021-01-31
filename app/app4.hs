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
import Corpus.JSeM (JSeMData(..), fetchJSeMData)
import Interface.Text (SimpleText(..))
import Interface.TeX (Typeset(..))
import Parser.CCG (Node(..),RuleSymbol(..),Cat(..),isBaseCategory,Feature(..),FeatureValue(..))
import qualified DTS.UDTT as UDTT
import qualified DTS.UDTTwithName as UDWN
import Yesod
import qualified Text.Julius as J
import qualified JSeM as Js               --jsem
--import qualified JSeM.XML as Js           --jsem
import Control.Monad (forM_)           --base
import qualified Corpus.JSeM as C

data App = App

mkYesod "App" [parseRoutes|
/ HomeR GET
/examples/#Int Examples1R GET
/jsem/#String JsemR GET
|]

--文探索
type Sentence = T.Text

type SentenceMap = Map.Map Int Sentence

sentences :: SentenceMap
sentences = Map.fromList
  [(1, T.fromStrict $ hypothesis (jsemData!!2))
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

jsemData :: [JSeMData]
jsemData = unsafePerformIO fetchJSeMData


--Test：JSeMDataからjsem_id・premises・hypothesisをとったもの
data Test = Test{
     jsem_id_w    :: StrictT.Text,
     answer_w     :: Js.JSeMLabel,
     premises_w   :: [StrictT.Text],
     hypothesis_w :: StrictT.Text
} deriving (Eq, Show)


emptyTest :: Test
emptyTest = Test { jsem_id_w = "", answer_w = Js.OTHER, premises_w = [], hypothesis_w = "" } 

jsemSearch :: [JSeMData] -> String -> Test
jsemSearch jsemData sentID = do
      case jsemData of
          [] -> emptyTest
          (JSeMData{C.jsem_id = i, C.link = l, C.description = d, C.answer = a, C.phenomena = ph, C.inference_type = it, C.note = n, C.premises = p, C.hypothesis = h}:xs) -> 
                 if (i /= StrictT.pack sentID) 
                     then jsemSearch xs sentID
                     else Test{jsem_id_w = i, answer_w = a, premises_w = p, hypothesis_w = h}

     

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
  if (num < 0 || num > 10) 
    then notFound
    else do
      let s = eithertostring (sentenceLookup num sentences)
          m = unsafePerformIO $ L.parseSentence' 16 2 s
      defaultLayout $ do
        id <- newIdent
        [whamlet|<h2> 文 #{num}：#{s}|]
        toWidget [cassius|
          .rule
            position: relative;
            top: 10px;
          body
            font-size: 1em;
          .font-main
            padding: 2px;
          #btn1
            margin-bottom: 4px;
          h2
            border-bottom: dashed 3px #ffd700
            padding: 0.3em
          .btn-design
            color: #696969;
            font-size: 18pt;
            text-align: center;
            background: #ffffff;
            border-radius: 10%;
            border: 2px solid #c0c0c0;
          .btn-design:hover
            color: #696969;
            font-size: 18pt;
            text-align: center;
            background: #f5f5f5;
            border-radius: 10%;
            border: 2px solid #c0c0c0;
          |]
        --toWidget $ J.juliusFile "Interface/MathJax/MathJax.js?config=TeX-AMS-MML_HTMLorMML
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
        mapM_ widgetize $ take 1 m


getJsemR :: String -> Handler Html
getJsemR var = do
-- contentsはTest型
  let contents = jsemSearch jsemData var
-- preは[T.Text]型・hyはT.Text型
  let ans = show $ answer_w contents
      pre = map T.fromStrict (premises_w contents)
      hy = T.fromStrict $ hypothesis_w contents
  if( pre == [] || hy == "" ) 
   then defaultLayout $ do [whamlet|<p><font color=red> Notfound.|]
   else do
     let psIOnode = map (L.parseSentence' 16 2) pre
     let ps = head <$> map unsafePerformIO psIOnode
     let m = unsafePerformIO $ L.parseSentence' 16 2 hy
     defaultLayout $ do
        id <- newIdent
        [whamlet|<head> [#{var}] answer : #{ans}
                 $forall pr <- pre
                    <p>premise : <span class="pre-under">#{pr}
                 <p>hypothesis : <span class="hy-under">#{hy}
        |]
        toWidget [cassius|
          .rule
            position: relative;
            top: 10px;
          body
            font-size: 1em;
          .font-main
            padding: 2px;
          #btn1
            margin-bottom: 4px;
          h2
            border-bottom: dashed 3px #ffd700
            padding: 0.3em
          .btn-design
            color: #696969;
            font-size: 18pt;
            text-align: center;
            background: #ffffff;
            border-radius: 10%;
            border: 2px solid #c0c0c0;
          .btn-design:hover
            color: #696969;
            font-size: 18pt;
            text-align: center;
            background: #f5f5f5;
            border-radius: 10%;
            border: 2px solid #c0c0c0;
          .pre-under
            border-bottom: solid 3px #ffd700;
          .hy-under
            border-bottom: solid 3px #7fffd4;
          |]
        --toWidget $ J.juliusFile "Interface/MathJax/MathJax.js?config=TeX-AMS-MML_HTMLorMML
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
-- psはpremises・mはhypothesis
        mapM_ widgetize $ ps
        mapM_ widgetize $ take 1 m


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
        toWidget [hamlet|
         <h1>こんにちは
         <p .#{aaa}>Hello World!!
         |]


getHomeR :: Handler Html
getHomeR = defaultLayout $ do
  setTitle "Page title"
  myLayout

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
          <tr>
            <td valign="baseline">
              <table border="1" rules="rows" frame="void" cellpadding="5">
                <tr>
                  <td align="center" bgcolor="#ffd700">#{pf node}
                <tr>
                  <td align="center">
                    <table border="0" cellpadding="0">
                      <tr>
                        <td align="center">
                          <math xmlns='http://www.w3.org/1998/Math/MathML'>^{widgetize $ cat node}
                      <tr>
                        <td align="center">
                          <math xmlns='http://www.w3.org/1998/Math/MathML'>^{widgetize $ sem node}
            <td valign="baseline">
              <span .rule>LEX
        |]
    dtrs -> do
      let len = (length dtrs)*2
      id <- newIdent
      [whamlet|
         <table>
          <tr>
            <td valign="baseline">
              <div id=#{StrictT.concat [id, "layerA"]} style="display: block" class="open">
                <table border="1" rules="rows" frame="void" cellpadding="5">
                  <tr>
                    $forall dtr <- dtrs
                      <td align="center" valign="bottom">^{widgetize dtr}&nbsp;
                  <tr>
                    <td align="center" colspan=#{len}>
                      <table border="0" cellpadding="0">
                        <tr>
                          <td align="center">
                            <math xmlns='http://www.w3.org/1998/Math/MathML'>^{widgetize $ cat node}
                        <tr>
                          <td align="center">
                            <math xmlns='http://www.w3.org/1998/Math/MathML'>^{widgetize $ sem node}
              <div id=#{StrictT.concat [id, "layerB"]} style="display: none" class="close">
                <table border="2" rules="rows" cellpadding="5" border="3px solid #808080">
                  <tr>
                    <td bgcolor="#ffd700">^{widgetize $ pf node}
                  <tr>
                    <td align="center" colspan=#{len}>
                      <table border="0" cellpadding="0">
                        <tr>
                          <td align="center">
                            <math xmlns='http://www.w3.org/1998/Math/MathML'>^{widgetize $ cat node}
                        <tr>
                          <td align="center">
                            <math xmlns='http://www.w3.org/1998/Math/MathML'>^{widgetize $ sem node}
            <td valign="baseline">
              <table border="1" rules="rows" frame="void" cellpadding="5">
                <tr>
                  <td>
                   <button type="button" class="btn-design" id=#{StrictT.concat [id, "button"]} onclick=toggle('#{id}')>-
                  <span .rule>^{widgetize $ rs node}
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
                              <mn>#{T.pack $ show i}
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

