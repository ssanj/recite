{-# LANGUAGE DeriveGeneric #-}

module Yaml(readYaml, readYamlString) where

import qualified GHC.Generics as GH
import qualified Data.Yaml as Y
import qualified Types as T
import qualified Data.ByteString as B
import qualified Data.Maybe as MB
import qualified Data.Bifunctor as BF

data ConfigYaml = ConfigYaml { name :: String, tags :: [String], uri :: String } deriving (Show, GH.Generic)

instance Y.FromJSON ConfigYaml

readYaml :: String -> IO (Either String [T.Entry])
readYaml configFile =
  do configYamls <- Y.decodeFileEither configFile :: IO (Either Y.ParseException [ConfigYaml])
     return $ BF.bimap show id (configsToEntries configYamls)

readYamlString :: B.ByteString -> Either String [T.Entry]
readYamlString source =
     let configYamls = Y.decodeEither' source :: Either Y.ParseException [ConfigYaml]
     in BF.bimap show id (configsToEntries configYamls)

configsToEntries :: Either Y.ParseException [ConfigYaml] -> Either Y.ParseException [T.Entry]
configsToEntries = fmap (MB.catMaybes . fmap mkEntry)

mkEntry :: ConfigYaml -> Maybe T.Entry
mkEntry cy = T.entry (name cy) (uri cy) (tags cy)
