{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module Grendel.API where

import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import Control.Monad.IO.Class (liftIO)
import Codec.Picture (DynamicImage)
import Servant
import Servant.JuicyPixels

import Grendel.Types (ComicSlug, Comic)
import Grendel.Database (comicsFromDisk, pageFromDisk)

type API = "ping"   :> Get '[JSON] Bool
      :<|> "comics" :> Get '[JSON] [ComicSlug]
      :<|> "comic"  :> Capture "slug" ComicSlug :> Get '[JSON] Comic
      :<|> "comic"  :> Capture "slug" ComicSlug
                    :> Capture "page" FilePath
                    :> Get '[JPEG 100] DynamicImage

api :: Proxy API
api = Proxy

app :: Application
app = serve api server

server :: Server API
server = ping
    :<|> getComics
    :<|> getComic
    :<|> getComicPage

ping :: Handler Bool
ping = pure True

getComics :: Handler [ComicSlug]
getComics = liftIO (Map.keys <$> comicsFromDisk)

getComic :: ComicSlug -> Handler Comic
getComic slug = do
  x <- liftIO (Map.lookup slug <$> comicsFromDisk)
  case x of
    Nothing -> throwError err404
    Just comic -> pure comic

getComicPage :: ComicSlug -> FilePath -> Handler DynamicImage
getComicPage slug page = do
  x <- liftIO (pageFromDisk slug page)
  case x of
    Left _s -> throwError err404
    Right img -> pure img
