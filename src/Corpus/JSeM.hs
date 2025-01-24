{-# OPTIONS -Wall #-}
{-# LANGUAGE OverloadedStrings #-}

module Corpus.JSeM (
  J.JSeMData(..),
  fetchJSeMData
  ) where

import System.FilePath ((</>),isExtensionOf) --filepath
import qualified System.Directory as D   --directory
import qualified JSeM as J               --jsem
import qualified JSeM.XML as J           --jsem

jsemDataFolder :: FilePath
jsemDataFolder = "/Users/koharusaeki/codes/lightblue/JSeM/data/v1.0"

-- | dataFolderにある拡張子.txtファイルのすべてについて、
fetchJSeMData :: IO [J.JSeMData]
fetchJSeMData = do
  -- ディレクトリが存在するかcheck
  _ <- D.doesDirectoryExist jsemDataFolder 
  -- jsemDataFolder以下にあるxml形式のファイル、ファイルパスリスト
  xmlFiles <- map (jsemDataFolder </>) <$> filter (isExtensionOf "xml") <$> D.listDirectory jsemDataFolder
  -- 全てのxmlファイルをJSeM型データのリストにし、返す
  concat <$> mapM J.xmlFile2jsemData xmlFiles 

