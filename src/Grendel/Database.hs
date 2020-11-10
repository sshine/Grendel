{-# LANGUAGE TupleSections #-}

module Grendel.Database where

import           Codec.Picture (DynamicImage, readImage)
import           Control.Monad (forM, filterM)
import           Data.Char (toLower)
import           Data.List (sort)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           System.Directory (listDirectory, doesDirectoryExist)
import           System.FilePath ((</>), takeExtension)

import Grendel.Types

galleriesDir :: FilePath
galleriesDir = "data" </> "galleries"

comicsFromDisk :: IO (Map ComicSlug [FilePath])
comicsFromDisk = fmap Map.fromList $ do
  galleries <- galleriesFromDisk
  forM galleries $ \gallery -> do
    pages <- galleryFromDisk gallery
    pure (gallery, pages)

-- | Each directory corresponds to one gallery.
galleriesFromDisk :: IO [FilePath]
galleriesFromDisk = galleriesDir `fromDiskWhen` isDirectory
  where
    isDirectory :: FilePath -> IO Bool
    isDirectory = doesDirectoryExist . (galleriesDir </>)

-- | Each gallery consists of a list of JPEG files.
galleryFromDisk :: FilePath -> IO [FilePath]
galleryFromDisk dirPath =
  (galleriesDir </> dirPath) `fromDiskWhen` (pure . isValidExtension)
  where
    isValidExtension :: FilePath -> Bool
    isValidExtension = (`elem` validExtensions) . map toLower . takeExtension

    validExtensions :: [String]
    validExtensions = [".png", ".jpg"]

pageFromDisk :: ComicSlug -> FilePath -> IO (Either String DynamicImage)
pageFromDisk slug page = readImage (galleriesDir </> slug </> page)

fromDiskWhen :: FilePath -> (FilePath -> IO Bool) -> IO [FilePath]
fromDiskWhen dirPath p = listDirectory dirPath >>= filterM p >>= pure . sort
