{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE QuasiQuotes          #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE ViewPatterns      #-}
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Map as Map
import Yesod

data App = App

mkYesod "App" [parseRoutes|
/ HomeR GET
/examples/#Int Examples1R GET
|]

--文探索
type Sentence = String

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

sentenceLookup :: Int -> SentenceMap -> Either String String
sentenceLookup number map = case Map.lookup number map of
  Nothing -> Left $ "存在しません"
  Just sentence -> Right sentence 


--Either型からString型にする
eithertostring :: Either String String -> String
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
           defaultLayout [whamlet|<h2> 例文#{num}：#{s}|]



main :: IO ()
main = warp 3000 App
