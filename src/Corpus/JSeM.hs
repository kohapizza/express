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
jsemDataFolder = "/home/bekki/program/jsem/data/v1.0"

-- | dataFolderにある拡張子.txtファイルのすべてについて、
fetchJSeMData :: IO [J.JSeMData]
fetchJSeMData = do
  _ <- D.doesDirectoryExist jsemDataFolder 
  xmlFiles <- map (jsemDataFolder </>) <$> filter (isExtensionOf "xml") <$> D.listDirectory jsemDataFolder
  concat <$> mapM J.xmlFile2jsemData xmlFiles 

