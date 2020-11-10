{-# LANGUAGE DeriveGeneric #-}

module Grendel.Types where

-- import Data.Aeson (FromJSON, ToJSON)
-- import GHC.Generics (Generic)

type ComicSlug = FilePath

-- instance FromJSON ComicSlug
-- instance ToJSON ComicSlug

type Comic = [FilePath]