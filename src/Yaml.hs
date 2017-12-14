{-# LANGUAGE DeriveGeneric #-}

module Yaml(readYaml) where

import qualified GHC.Generics as GH
import qualified Data.Yaml as Y
import qualified Types as T
import qualified Data.Maybe as MB
import qualified Data.Bifunctor as BF

data ConfigYaml = ConfigYaml { name :: String, tags :: [String], uri :: String } deriving (Show, GH.Generic)

instance Y.FromJSON ConfigYaml

readYaml :: String -> IO (Either String [T.Entry])
readYaml configFile =
  do configYamls <- Y.decodeFileEither configFile :: IO (Either Y.ParseException [ConfigYaml])
     let mkEntry cy = T.entry (name cy) (uri cy) (tags cy)
         entries    = fmap (MB.catMaybes . fmap mkEntry) configYamls
     return (BF.bimap show id entries)
