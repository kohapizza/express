{-# OPTIONS -Wall #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE FlexibleInstances     #-}

module HTML (
    MathML(..)
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
import  qualified DTS.UDTTwithName as UDWN
import qualified DTS.Index as Index
import  qualified DTS.Index as UDID
import qualified DTS.QueryTypes as QT
import  Control.Monad (forM_)
import  Control.Applicative
import  qualified Text.Julius as J
import Interface.Tree
import Text.Blaze.Html


class MathML a where
    toMathML :: a -> Html

-- インスタンスにT.Text型を定義
instance MathML T.Text where
  toMathML = toHtml

-- MathMLクラスのインスタンスにNode型を定義
instance MathML Node where
  -- 子ノードなかったら
  -- toMathML :: Node -> Html
  toMathML node = case Parser.CCG.daughters node of
    [] -> do
      -- newIdent 関数で一意のidを得る
      let id = makeId []
      [shamlet|
        <table>
          <tr valign="bottom">
            <td valign="baseline">
              <table border="1" rules="rows" frame="void" cellpadding="2">
                <!-- Nodeの音韻表示 -->
                <tr>
                  <td align="center" bgcolor="#002b5c" style="color: #ffffff;">#{pf node}
                <tr>
                  <td align="center">
                    <table border="0" cellpadding="0">
                      <tr class="cathide">
                        <td align="center">
                          <math xmlns='http://www.w3.org/1998/Math/MathML'>^{toMathML $ cat node}
                      <tr class="semhide">
                        <td>
                          <math xmlns='http://www.w3.org/1998/Math/MathML'>^{toMathML $ sem node}
            <td valign="baseline">
              <span>LEX
        |]
    dtrs -> do
      -- 子の数*２
      let len = (length dtrs)*2
      let id = makeId []
      [shamlet|
         <table>
          <tr valign="bottom">
            <td valign="baseline">
              <div id=#{StrictT.concat [id, "layerA"]} style="display: none" class="close">
                <table border="1" rules="rows" frame="void" cellpadding="2">
                  <tr valign="bottom">
                    $forall dtr <- dtrs
                      <td align="center" valign="bottom">^{toMathML dtr}&nbsp;
                  <tr>
                    <td align="center" colspan=#{len}>
                      <table border="0" cellpadding="0">
                        <tr class="cathide">
                          <td align="center">
                            <math xmlns='http://www.w3.org/1998/Math/MathML'>^{toMathML $ cat node}
                        <tr class="semhide">
                          <td>
                            <math xmlns='http://www.w3.org/1998/Math/MathML'>^{toMathML $ sem node}
              <div id=#{StrictT.concat [id, "layerB"]} style="display: block" class="open">
                <table border="2" rules="rows" cellpadding="5" border="3px solid #808080">
                  <tr>
                    <td align="center" bgcolor="#002b5c" style="color: #ffffff;">^{toMathML $ pf node}
                  <tr>
                    <td align="center" colspan=#{len}>
                      <table border="0" cellpadding="0">
                        <tr class="cathide">
                          <td align="center">
                            <math xmlns='http://www.w3.org/1998/Math/MathML'>^{toMathML $ cat node}
                        <tr class="semhide">
                          <td align="center">
                            <math xmlns='http://www.w3.org/1998/Math/MathML'>^{toMathML $ sem node}
            <td valign="baseline">
              <table border="1" rules="rows" frame="void" cellpadding="5">
                <tr>
                  <td>
                   <button type="button" class="btn-design" id=#{StrictT.concat [id, "button"]} onclick=toggle('#{id}')>+
                  <span>^{toMathML $ rs node}
        |]

-- -- MathMLクラスのインスタンスにNode型を定義
-- instance MathML Node where
--   -- toMathML :: Node -> Html
--   toMathML node = [shamlet|
--     <html>
--     <head>
--       <title>Static Yesod Output
--     <body>
--       <h1>Hello, Yesod Static!
--   |]

-- toText :: a -> Text
instance MathML RuleSymbol where
  toMathML rs = [shamlet|#{toText rs}|]

instance MathML Cat where
  toMathML category = case category of
    SL x y      -> [shamlet|<mrow>
                              ^{toMathML x}
                              <mo>/
                              ^{toMathML' y}
                              |]
    BS x y      -> [shamlet|<mrow>
                              ^{toMathML x}
                              <mo>\
                              ^{toMathML' y}
                              |]
    T True i _  -> [shamlet|<msub>
                              <mi>T
                              <mn>#{T.pack $ show i}
                              |]
    T False i u -> [shamlet|<msub>
                              ^{toMathML' u}
                              <mn>:[#{T.pack $ show i}]
                              |]
    S (pos:(conj:pm)) -> let x = toText pm
                             nullx = T.null x in
                         [shamlet|
                           <msub>
                             <mi>S
                             <mstyle color='Purple'>
                               <mtable columnalign='left'>
                                 <mtr class="sf">
                                   <mtd>^{toMathML pos}
                                 <mtr class="sf">
                                   <mtd>
                                     <mpadded height='-0.5em'>^{toMathML conj}
                                 <mtr class="sf">
                                   <mtd>
                                     <mpadded height='-0.5em'>^{toMathML pm}
                                 |]
    NP [cas]    -> [shamlet|<msub>
                              <mi>NP
                              <mtext class="sf">^{toMathML cas}
                           |]
    Sbar [sf]   -> [shamlet|<msub>
                              <menclose notation='top'>
                                <mi>S
                              <mtext class="sf">^{toMathML sf}
                           |]
    N           -> [shamlet|<mi>N|]
    CONJ        -> [shamlet|<mi>CONJ|]
    LPAREN      -> [shamlet|<mi>LPAREN|]
    RPAREN      -> [shamlet|<mi>RPAREN|]
    _           -> [shamlet|<mtext>Error: #{toText category}|]
    where toMathML' c = if isBaseCategory c 
                           then toMathML c
                           else [shamlet|<mrow>
                                           <mo>(
                                           ^{toMathML c}
                                           <mo>)
                                           |]

instance MathML Feature where 
  toMathML (SF i f) = [shamlet|
                          <mtext>
                            #{toTeX f}
                          <mo>:
                          <mn>[#{T.pack (show i)}]
                          |]
  toMathML (F f) = [shamlet|<mtext>#{toTeX f}|]

instance MathML [Feature] where
  toMathML pmfs = [shamlet|
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
-- fromDeBruijn :: [VarName] -> UDTTdB(=UDTT).Preterm -> UDWN.Preterm
instance MathML UDTT.Preterm where
  toMathML = toMathML . (UDWN.fromDeBruijn [])

instance MathML UDWN.VarName where
  toMathML (UDWN.VarName v i) =
    [shamlet|
      <msub>
        <mi>#{T.singleton v}
        <mn>#{T.pack (show i)}
        |]

-- UDWN: DTS.UDTTwithName
instance MathML UDWN.Preterm where
  toMathML preterm = case preterm of
    UDWN.Var vname -> toMathML vname
    UDWN.Con cname -> [shamlet|<mtext>#{cname}|]
    UDWN.Type -> [shamlet|<mi>type|]
    UDWN.Kind -> [shamlet|<mi>kind|]
    UDWN.Pi vname a b -> [shamlet|
      <mrow>
        <mo>(
        ^{toMathML vname}
        <mo>:
        ^{toMathML a}
        <mo>)
        <mo>&rarr;
        ^{toMathML b}
        |]
    UDWN.Not a -> [shamlet|
      <mrow>
        <mo>&not;
        <mi>toMathML a
        |]
    UDWN.Lam vname m -> [shamlet|
      <mrow>
        <mi>&lambda;
        ^{toMathML vname}
        <mpadded lspace='-0.2em' width='-0.2em'>
          <mo>.
        ^{toMathML m}
        |]
    UDWN.App (UDWN.App (UDWN.Con cname) y) x -> [shamlet|
      <mrow>
        <mtext>#{cname}
        <mo>(
        ^{toMathML x}
        <mo>,
        ^{toMathML y}
        <mo>)
        |]
    UDWN.App (UDWN.App (UDWN.App (UDWN.Con cname) z) y) x -> [shamlet|
      <mrow>
        <mtext>#{cname}
        <mo>(
        ^{toMathML x}
        <mo>,
        ^{toMathML y}
        <mo>,
        ^{toMathML z}
        <mo>)
        |]
    UDWN.App (UDWN.App (UDWN.App (UDWN.App (UDWN.Con cname) u) z) y) x -> [shamlet|
      <mrow>
        <mtext>#{cname}
        <mo>(
        ^{toMathML x}
        <mo>,
        ^{toMathML y}
        <mo>,
        ^{toMathML z}
        <mo>,
        ^{toMathML u}
        <mo>)
        |]
    UDWN.App m n -> [shamlet|
      <mrow>
        ^{toMathML m}
        <mo>(
        ^{toMathML n}
        <mo>)
        |]
    UDWN.Sigma vname a b -> case b of 
      UDWN.Top -> toMathML a
      _   -> [shamlet|
        <mrow>
          <mo>[
          <mtable style="text-align: left;">
            <mtr style="text-align: left;">
              <mtd columnalign="left">
                <mrow>
                  ^{toMathML vname}
                  <mo>:
                  ^{toMathML a}
            <mtr style="text-align: left;">
              <mtd columnalign="left">
                <mpadded height='-0.5em'>^{toMathML b}
          <mo>]
        |]
    UDWN.Pair m n  -> [shamlet|
      <mrow>
        <mo>(
        ^{toMathML m}
        <mo>,
        ^{toMathML n}
        <mo>)
        |]
    UDWN.Proj s m  -> [shamlet|
      <mrow>
        <msub>
          <mi>&pi;
          <mi>#{toText s}
        <mo>(
        ^{toMathML m}
        <mo>)
        |]
    UDWN.Disj a b -> [shamlet|
      <mrow>
        <mi>
          ^{toMathML a}
        <mo>+
        <mi>
          ^{toMathML b}
        |]
    UDWN.Iota s p -> [shamlet|
        <mrow>
        <msub>
          <mi>&iota;
          <mi>#{toText s}
        <mo>(
        ^{toMathML p}
        <mo>)
        |]
    UDWN.Unpack p l m n -> [shamlet|
      <mrow>
        <msubsup>
          <mi>unpack
          <mn>^{toMathML l}
          <mn>^{toMathML p}
        <mo>(
        ^{toMathML m}
        <mo>,
        ^{toMathML n}
        <mo>)
        |]   
    UDWN.Lamvec vname m  -> [shamlet|
      <mrow>
        <mi>&lambda;
        <mover>
          ^{toMathML vname}
          <mo>&rarr;
        <mo>.
        ^{toMathML m}
        |]
    UDWN.Appvec vname m -> [shamlet|
      <mrow>
        ^{toMathML m}
        <mover>
          ^{toMathML vname}
          <mo>&rarr;
          |]
    UDWN.Unit       -> [shamlet|<mi>()|]
    UDWN.Top        -> [shamlet|<mi>&top;|]
    UDWN.Entity     -> [shamlet|<mi>entity|]
    UDWN.Bot        -> [shamlet|<mi>&bot;|]
    UDWN.Asp m    -> [shamlet|
      <mrow>
        <mo>@
        ^{toMathML m}
      |]
    UDWN.Nat    -> [shamlet|<mi>N|]
    UDWN.Zero   -> [shamlet|<mi>0|]
    UDWN.Succ n -> [shamlet|
      <mrow>
        <mi>s
        ^{toMathML n}
        |]
    UDWN.Natrec n e f -> [shamlet|
      <mrow>
        <mi>natrec
        <mo>(
        ^{toMathML n}
        <mo>,
        ^{toMathML e}
        <mo>,
        ^{toMathML f}
        <mo>)
        |]
    UDWN.Eq a m n -> [shamlet|
      <mrow>
        ^{toMathML m}
        <msub>
          <mo>=
          ^{toMathML a}
        ^{toMathML n}
        |]
    UDWN.Refl a m -> [shamlet|
      <mrow>
        <mi>refl
        ^{toMathML a}
        <mo>(
        ^{toMathML m}
        <mo>)
        |]
    UDWN.Idpeel m n -> [shamlet|
      <mrow>
        <mi>idpeel
        <mo>(
        ^{toMathML m}
        <mo>,
        ^{toMathML n}
        <mo>)
        |]

-- fromDeBruijn :: [VarName] -> DTTdB(=DTT).Preterm -> DWN.Preterm
instance MathML DTT.Preterm where
  toMathML = toMathML . (DWN.fromDeBruijn [])

instance MathML DWN.VarName where
  toMathML (DWN.VarName v i) =
    [shamlet|
      <msub>
        <mi>#{T.singleton v}
        <mn>#{T.pack (show i)}
        |]


instance MathML DWN.Preterm where
  toMathML preterm = case preterm of
    DWN.Var vname -> toMathML vname
    DWN.Con cname -> [shamlet|<mtext>#{cname}|]
    DWN.Type -> [shamlet|<mi>type|]
    DWN.Kind -> [shamlet|<mi>kind|]
    DWN.Pi vname a b -> [shamlet|
      <mrow>
        <mo>(
        ^{toMathML vname}
        <mo>:
        ^{toMathML a}
        <mo>)
        <mo>&rarr;
        ^{toMathML b}
        |]
    DWN.Not a -> [shamlet|
      <mrow>
        <mi>&not;
        <mi>toMathML a
        |]
    DWN.Sigma vname a b -> case b of 
      DWN.Top -> toMathML a
      _   -> [shamlet|
        <mrow>
          <mo>[
          <mtable style="text-align: left;">
            <mtr style="text-align: left";>
              <mtd columnalign="left">
                <mrow>
                  ^{toMathML vname}
                  <mo>:
                  ^{toMathML a}
            <mtr style="text-align: left";>
              <mtd columnalign="left">
                <mpadded height='-0.5em'>^{toMathML b}
          <mo>]
        |]
    DWN.Lam vname m -> [shamlet|
      <mrow>
        <mi>&lambda;
        ^{toMathML vname}
        <mpadded lspace='-0.2em' width='-0.2em'>
          <mo>.
        ^{toMathML m}
        |]
    DWN.App (DWN.App (DWN.Con cname) y) x -> [shamlet|
      <mrow>
        <mtext>#{cname}
        <mo>(
        ^{toMathML x}
        <mo>,
        ^{toMathML y}
        <mo>)
        |]
    DWN.App (DWN.App (DWN.App (DWN.Con cname) z) y) x -> [shamlet|
      <mrow>
        <mtext>#{cname}
        <mo>(
        ^{toMathML x}
        <mo>,
        ^{toMathML y}
        <mo>,
        ^{toMathML z}
        <mo>)
        |]
    DWN.App (DWN.App (DWN.App (DWN.App (DWN.Con cname) u) z) y) x -> [shamlet|
      <mrow>
        <mtext>#{cname}
        <mo>(
        ^{toMathML x}
        <mo>,
        ^{toMathML y}
        <mo>,
        ^{toMathML z}
        <mo>,
        ^{toMathML u}
        <mo>)
        |]
    DWN.App m n -> [shamlet|
      <mrow>
        ^{toMathML m}
        <mo>(
        ^{toMathML n}
        <mo>)
        |]
    DWN.Pair m n  -> [shamlet|
      <mrow>
        <mo>(
        ^{toMathML m}
        <mo>,
        ^{toMathML n}
        <mo>)
        |]
    DWN.Proj s m  -> [shamlet|
      <mrow>
        <msub>
          <mi>&pi;
          <mi>#{toText s}
        <mo>(
        ^{toMathML m}
        <mo>)
        |]
    DWN.Disj a b -> [shamlet|
      <mrow>
        <mi>
          ^{toMathML a}
        <mo>+
        <mi>
          ^{toMathML b}
        |]
    DWN.Iota s p -> [shamlet|
        <mrow>
        <msub>
          <mi>&iota;
          <mi>#{toText s}
        <mo>(
        ^{toMathML p}
        <mo>)
        |]
    DWN.Unpack p l m n -> [shamlet|
      <mrow>
        <msubsup>
          <mi>unpack
          <mn>^{toMathML l}
          <mn>^{toMathML p}
        <mo>(
        ^{toMathML m}
        <mo>,
        ^{toMathML n}
        <mo>)
        |] 
    DWN.Unit       -> [shamlet|<mi>()|]
    DWN.Top        -> [shamlet|<mi>&top;|]
    DWN.Entity     -> [shamlet|<mi>entity|]
    DWN.Bot        -> [shamlet|<mi>&bot;|]
    DWN.Nat    -> [shamlet|<mi>N|]
    DWN.Zero   -> [shamlet|<mi>0|]
    DWN.Succ n -> [shamlet|
      <mrow>
        <mi>s
        ^{toMathML n}
        |]
    DWN.Natrec n e f -> [shamlet|
      <mrow>
        <mi>natrec
        <mo>(
        ^{toMathML n}
        <mo>,
        ^{toMathML e}
        <mo>,
        ^{toMathML f}
        <mo>)
        |]
    DWN.Eq a m n -> [shamlet|
      <mrow>
        ^{toMathML m}
        <msub>
          <mo>=
          ^{toMathML a}
        ^{toMathML n}
        |]
    DWN.Refl a m -> [shamlet|
      <mrow>
        <mi>refl
        ^{toMathML a}
        <mo>(
        ^{toMathML m}
        <mo>)
        |]
    DWN.Idpeel m n -> [shamlet|
      <mrow>
        <mi>idpeel
        <mo>(
        ^{toMathML m}
        <mo>,
        ^{toMathML n}
        <mo>)
        |]

instance MathML QT.DTTrule where
  toMathML rule = case rule of
    QT.Var -> [shamlet| <mi>#{toText QT.Var}|]
    QT.Con -> [shamlet| <mi>#{toText QT.Con}|]
    QT.TypeF -> [shamlet| <mi>#{toText QT.TypeF}|]
    QT.Conv -> [shamlet| <mi>#{toText QT.Conv}|]
    QT.WK -> [shamlet| <mi>#{toText QT.WK}|]
    QT.PiF -> [shamlet| <mi>ΠF|]
    QT.PiI -> [shamlet| <mi>ΠI|]
    QT.PiE -> [shamlet| <mi>ΠE|]
    QT.SigmaF -> [shamlet| <mi>ΣF|]
    QT.SigmaI -> [shamlet| <mi>ΣI|]
    QT.SigmaE ->[shamlet| <mi>ΣE|]
    QT.DisjF -> [shamlet| <mi>+F|]
    QT.DisjI -> [shamlet| <mi>+I|]
    QT.DisjE -> [shamlet| <mi>+E|]
    QT.EnumF ->[shamlet| <mi>{}F|]
    QT.EnumI -> [shamlet| <mi>{}I|]
    QT.EnumE -> [shamlet| <mi>{}E|]
    QT.IqF -> [shamlet| <mi>=F|]
    QT.IqI ->[shamlet| <mi>=I|]
    QT.IqE -> [shamlet| <mi>=E|]
    QT.NatF ->[shamlet| <mi>NatF|]
    QT.NatI ->[shamlet| <mi>NatI|]
    QT.NatE ->[shamlet| <mi>NatE|]

-- type Signature = [(T.Text, Preterm)]
instance MathML DTT.Signature where
  toMathML signature = 
    let reversedItems = reverse signature
        itemsWithFlags = case reversedItems of
          [] -> []
          _  -> zip reversedItems (repeat False) ++ [(last reversedItems, True)]
    in [shamlet|
       <mrow>
         $forall ((nm, tm), isLast) <- itemsWithFlags
           <math xmlns="http://www.w3.org/1998/Math/MathML">^{toMathML nm}
           <mo>:
           <math xmlns="http://www.w3.org/1998/Math/MathML">^{toMathML tm}
           $if not isLast
             <mo>,
     |]

-- type Signature = [(T.Text, Preterm)]
instance MathML DWN.Signature where
  toMathML signature = 
    let reversedItems = reverse signature
        itemsWithFlags = case reversedItems of
          [] -> []
          _  -> zip reversedItems (repeat False) ++ [(last reversedItems, True)]
    in [shamlet|
       <math xmlns="http://www.w3.org/1998/Math/MathML">
       <mrow>
         $forall ((nm, tm), isLast) <- itemsWithFlags
           <math xmlns="http://www.w3.org/1998/Math/MathML">^{toMathML nm}
           <mo>:
           <math xmlns="http://www.w3.org/1998/Math/MathML">^{toMathML tm}
           $if not isLast
             <mo>,
     |]

-- type Context = [(VarName, Preterm)]
instance MathML DWN.Context where
  toMathML context = 
    let reversedItems = reverse context
        itemsWithFlags = case reversedItems of
          [] -> []
          _  -> zip (init reversedItems) (repeat False) ++ [(last reversedItems, True)]
    in [shamlet|
         <math xmlns="http://www.w3.org/1998/Math/MathML">
           <mrow>
             $forall ((nm, tm), isLast) <- itemsWithFlags
               <math xmlns="http://www.w3.org/1998/Math/MathML">^{toMathML nm}
               <mo>:
               <math xmlns="http://www.w3.org/1998/Math/MathML">^{toMathML tm}
               $if not isLast
                 <mo>,
      |]


-- type Context = [Preterm]
instance MathML DTT.Context where
  toMathML context = 
    let reversedItems = reverse context
        itemsWithFlags = case reversedItems of
          [] -> []
          _ -> zip reversedItems (repeat False) ++ [(last reversedItems, True)]
    in [shamlet|
         <math xmlns="http://www.w3.org/1998/Math/MathML">
           <mrow>
             $forall (cotx, isLast) <- itemsWithFlags
               <math xmlns="http://www.w3.org/1998/Math/MathML">^{toMathML cotx}
               $if not isLast
                 <mo>,
       |]

-- 
instance MathML DTT.Judgment where 
  toMathML (DTT.Judgment sig cxt trm typ) =
    [shamlet|
      <math xmlns="http://www.w3.org/1998/Math/MathML">
        <mrow>
          <math xmlns="http://www.w3.org/1998/Math/MathML">^{toMathML cxt}
          <mo>&vdash;
          <math xmlns="http://www.w3.org/1998/Math/MathML">^{toMathML trm}
          <mo>:
          <math xmlns="http://www.w3.org/1998/Math/MathML">^{toMathML typ}
    |]

instance MathML UDWN.Judgment where
  toMathML (UDWN.Judgment sig cxt trm typ) = 
    [shamlet|
      <math xmlns="http://www.w3.org/1998/Math/MathML">
        <mrow>
          <math xmlns="http://www.w3.org/1998/Math/MathML">^{toMathML cxt}
          <mo>&vdash;
          <math xmlns="http://www.w3.org/1998/Math/MathML">^{toMathML trm}
          <mo>:
          <math xmlns="http://www.w3.org/1998/Math/MathML">^{toMathML typ}
    |]

instance MathML DWN.Judgment where
  toMathML (DWN.Judgment sig cxt trm typ) = 
    [shamlet|
      <math xmlns="http://www.w3.org/1998/Math/MathML">
        <mrow>
          <math xmlns="http://www.w3.org/1998/Math/MathML">^{toMathML cxt}
          <mo>&vdash;
          <math xmlns="http://www.w3.org/1998/Math/MathML">^{toMathML trm}
          <mo>:
          <math xmlns="http://www.w3.org/1998/Math/MathML">^{toMathML typ}
    |]

makeId :: [Int] -> StrictT.Text
makeId path = StrictT.pack ("node-" ++ concatMap (\i -> '-' : show i) path)

-- node :: DTT.Judgment
-- DWN.fromDeBruijnJudgment :: DTTdB.Judgment -> DWN.Judgment
instance (MathML r, a ~ DTT.Judgment) => MathML (Tree r a) where
  toMathML (Tree rule node dtrs) = case dtrs of
    [] -> do
      let id = makeId []
      [shamlet|
        <math xmlns="http://www.w3.org/1998/Math/MathML">
          <mstyle displaystyle="true">
            <mfrac linethickness="medium">
              <mrow>
              <mrow>^{toMathML $ DWN.fromDeBruijnJudgment node}
          <mstyle fontsize="0.8" color="Black">
              <mo>(</mo>
              <math xmlns="http://www.w3.org/1998/Math/MathML">^{toMathML rule}
              <mo>)</mo>
      |]
    dtrs -> do
      let len = (length dtrs)*5
      let id = makeId []
      [shamlet|
        <math xmlns="http://www.w3.org/1998/Math/MathML">
          <table>
            <tr valign="bottom">
              <td valign="baseline">
                <div id=#{StrictT.concat [id, "layerA"]} style="display: none" class="close">
                  <table border="1" rules="rows" frame="void" cellpadding="2">
                    <tr valign="bottom">
                      $forall dtr <- dtrs
                        <td align="center" valign="bottom">^{toMathML dtr}&nbsp;
                    <tr>
                      <td align="center" colspan=#{len}>
                        <table border="0" cellpadding="0">
                          <math xmlns="http://www.w3.org/1998/Math/MathML">^{toMathML $ DWN.fromDeBruijnJudgment node}
                <div id=#{StrictT.concat [id, "layerB"]} style="display: block" border="2px solid #000" class="open">
                  <table border="20" rules="rows" cellpadding="5" border="3px solid #808080">
                    <tr align="center" colspan=#{len}>
                      <math xmlns="http://www.w3.org/1998/Math/MathML">^{toMathML $ DWN.fromDeBruijnJudgment node}
              <td valign="baseline">
                <table border="1" rules="rows" frame="void" cellpadding="5">
                  <tr>
                    <td>
                      <mo>(</mo>
                      <math xmlns="http://www.w3.org/1998/Math/MathML">^{toMathML rule}
                      <mo>)</mo>
                    <td>
                      <button type="button" class="btn-design" id=#{StrictT.concat [id, "button"]} onclick=toggle('#{id}')>+
      |]


instance MathML UDTT.Judgment where 
  toMathML (UDTT.Judgment sig cxt trm typ) = 
    [shamlet|
      <math xmlns="http://www.w3.org/1998/Math/MathML">
        <mrow>
          <math xmlns="http://www.w3.org/1998/Math/MathML">^{toMathML cxt}
          <mo>&vdash;
          <math xmlns="http://www.w3.org/1998/Math/MathML">^{toMathML trm}
          <mo>:
          <math xmlns="http://www.w3.org/1998/Math/MathML">^{toMathML typ}
    |]