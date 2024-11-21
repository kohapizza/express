{-# OPTIONS -Wall #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE FlexibleInstances     #-}

module WidgetExpress (
    Widgetizable(..)
  ) where

import  Yesod
import  qualified Data.Maybe as Maybe --base
import  Interface.Text (SimpleText(..))
import  Interface.TeX (Typeset(..))
import  qualified Data.Text.Lazy as T
import  qualified Data.Text as StrictT
import  Parser.CCG (Node(..),RuleSymbol(..),Cat(..),isBaseCategory,Feature(..),FeatureValue(..))
import qualified DTS.DTTdeBruijn as DTT
import qualified DTS.DTTwithName as DWN
import qualified DTS.UDTTdeBruijn as UDTT
import qualified DTS.Index as Index
import  qualified DTS.Index as UDID
import  qualified DTS.UDTTwithName as UDWN
import qualified DTS.QueryTypes as QT
import  Control.Monad (forM_)           --base
import  Control.Applicative
import  qualified Text.Julius as J


-- Widgetizableクラスを定義、widgetizeという関数を持つ
class Widgetizable a where
  widgetize :: forall b. a -> WidgetT b IO ()
  -- type Widget = WidgetT MyApp IO ()
  -- 任意の型のbにWidget（WidgetT 型）としてページに描画できるように変換する
  -- widgetize :: forall b. a -> Widgetと同じ
  
-- WidgetizableクラスのインスタンスにT.Text型を定義
instance Widgetizable T.Text where
  widgetize = toWidget 

-- WidgetizableクラスのインスタンスにNode型を定義
instance Widgetizable Node where
  -- 子ノードなかったら
  widgetize node = case daughters node of
    [] -> do
      -- newIdent 関数で一意のidを得る
      id <- newIdent
      [whamlet|
        <table>
          <tr valign="bottom">
            <td valign="baseline">
              <table border="1" rules="rows" frame="void" cellpadding="2">
                <!-- Nodeの音韻表示 -->
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
      -- 子の数*２
      let len = (length dtrs)*2
      id <- newIdent
      [whamlet|
         <table>
          <tr valign="bottom">
            <td valign="baseline">
              <div id=#{StrictT.concat [id, "layerA"]} style="display: none" class="close">
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
              <div id=#{StrictT.concat [id, "layerB"]} style="display: block" class="open">
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
                   <button type="button" class="btn-design" id=#{StrictT.concat [id, "button"]} onclick=toggle('#{id}')>+
                  <span>^{widgetize $ rs node}
        |]


-- toText :: a -> Text
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

-- semのinstance化
-- Index.initializeIndex :: Indexed a -> a
-- UDWN.fromDeBruijnLoop :: [VarName] -- ^ A context (= a list of variable names)
--                    -> UDTT.Preterm  -- ^ A preterm in de Bruijn notation
--                    -> Indexed UDWN.Preterm -- ^ A preterm with variable names
-- UDWN.fromDeBruijnLoop [] :: UDTT.Preterm -> Indexed UDWN.Preterm
-- Index.initializeIndex . (UDWN.fromDeBruijnLoop []) :: UDWN.Preterm
instance Widgetizable UDTT.Preterm where
  widgetize = widgetize . Index.initializeIndex . (UDWN.fromDeBruijnLoop [])

-- 変えなくていいはず。
instance Widgetizable UDWN.VarName where
  widgetize (UDWN.VarName v i) =
    [whamlet|
      <msub>
        <mi>#{T.singleton v}
        <mn>#{T.pack (show i)}
        |]

-- 後で要対応
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
    UDWN.Disj a b -> [whamlet|
      <mrow>
        <mi>
          ^{widgetize a}
        <mo>+
        <mi>
          ^{widgetize b}
        |]
    UDWN.Iota s p -> [whamlet|
        <mrow>
        <msub>
          <mi>&iota;
          <mi>#{toText s}
        <mo>(
        ^{widgetize p}
        <mo>)
        |]
    UDWN.Unpack p l m n -> [whamlet|
      <mrow>
        <msubsup>
          <mi>unpack
          <mn>^{widgetize l}
          <mn>^{widgetize p}
        <mo>(
        ^{widgetize m}
        <mo>,
        ^{widgetize n}
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
    UDWN.Entity     -> [whamlet|<mi>entity|]
    UDWN.Bot        -> [whamlet|<mi>&bot;|]
    UDWN.Asp m    -> [whamlet|
      <mrow>
        <mo>@
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