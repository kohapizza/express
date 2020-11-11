{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE QuasiQuotes          #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE ViewPatterns         #-}

import qualified Data.Text.Lazy as T
import qualified Data.Map as Map
import System.IO.Unsafe (unsafePerformIO)
import qualified Lightblue as L
import Parser.CCG (Node(..))
import Yesod

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
  ,(3, "うううう")
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
               .frac
                 align: center;
               .numer
                 valign: bottom;
                 align: center;
               .denom
                 border-top: 5px;
                 border-bottom: 0px;
                 border-left: 0px;
                 border-right: 0px;
                 align: center;
               .rule
                 position: relative;
                 top: 10px;
               |]
             mapM_ widgetize m

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
                  <td align="center">#{show $ cat node}
            <td valign="baseline">
              <span .rule>LEX
        |]
    dtrs ->
      let len = (length dtrs)*2 in
      [whamlet|
        <table>
          <tr>
            <td valign="baseline">
              <table border="1" rules="rows" frame="void" cellpadding="5">
                <tr>
                  $forall dtr <- dtrs
                    <td align="center" valign="bottom">^{widgetize dtr}
                    <td>&nbsp;
                <tr>
                  <td align="center" colspan=#{len}>#{show $ cat node}
            <td valign="baseline">
              <span .rule>#{show $ rs node}
        |]



