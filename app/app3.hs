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
         <p .#{aaa}>Hello World!
         |]




getHomeR :: Handler Html
getHomeR = defaultLayout $ do
  setTitle "Page title"
  myLayout


getExamples1R :: Int -> Handler Html
getExamples1R num =
  if (num < 0 || num > 10) 
    then notFound
    else do
      let s = eithertostring (sentenceLookup num sentences)
          m = unsafePerformIO $ L.parseSentence' 16 2 s
      defaultLayout $ do
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
          |]
        --toWidget $ J.juliusFile "Interface/MathJax/MathJax.js?config=TeX-AMS-MML_HTMLorMML"
        toWidget [julius|
          MathJax.Hub.Config({
            tex2jax: {
              inlineMath: [['$','$'], ['\\(','\\)']],
              processEscapes: true
              },
            CommonHTML: { matchFontHeight: false },
            displayAlign: left,
            displayIndent: 2em
            });
          MathJax.Hub.Config({
            'HTML-CSS': {
            availableFonts: [],
            preferredFont: null,webFont: 'Neo-Euler'}});
          |]
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
              buttonID.intterHTML = "+";
            }};
          |]
        mapM_ widgetize $ take 1 m

main :: IO ()
main = warp 3000 App

class Widgetizable a where
  widgetize :: a -> Widget

instance Widgetizable T.Text where
  widgetize = toWidget 

instance Widgetizable Node where
  widgetize node = case daughters node of
    [] ->
      [whamlet|
        <table>
          <tr>
            <td valign="baseline">
              <table border="1" rules="rows" frame="void" cellpadding="5">
                <tr>
                  <td align="center">#{pf node}
                <tr>
                  <td align="center">
                    <math xmlns='http://www.w3.org/1998/Math/MathML'>^{widgetize $ cat node}
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
              <div id=#{StrictT.concat [id, "layerA"]} style="display: block" class=open>
                <table border="1" rules="rows" frame="void" cellpadding="5">
                  <tr>
                    $forall dtr <- dtrs
                      <td align="center" valign="bottom">^{widgetize dtr}&nbsp;
                  <tr>
                    <td align="center" colspan=#{len}>
                      <math xmlns='http://www.w3.org/1998/Math/MathML'>^{widgetize $ cat node}
              <div id=#{StrictT.concat [id, "layerB"]} style="display: none" class=close>
                <table border="1" rules="rows" frame="void" cellpadding="5">
                  <tr>
                    <td>^{widgetize $ pf node}
                  <tr>
                    <td align="center" colspan=#{len}>
                      <math xmlns='http://www.w3.org/1998/Math/MathML'>^{widgetize $ cat node}
            <td valign="baseline">
              <table>
                <tr>
                  <td>
                    <button type="button" id=#{StrictT.concat [id, "button"]} onclick=toggle('#{id}')>-
                <tr>
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
                          <menclose notation='box'>
                            <mn>#{T.pack (show i)}
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
  (l,F [P,M]) -> Just $ T.concat ["&plusmn;", l]
  (l,F [M,P]) -> Just $ T.concat ["&plusmn;", l]
  (l,SF i f)  -> do
                 x <- pmf2MathML l (F f)
                 return $ T.concat [x, ":<menclose notation='box'><mn>", T.pack $ show i, "</mn></menclose>"]
  _ -> return $ T.pack "Error: pmf2MathML"

