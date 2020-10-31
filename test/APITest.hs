{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module APITest where

import           Network.HTTP.Client
import           Network.Wai
import qualified Network.Wai.Handler.Warp as Warp
--import           Servant.Client.Core (BaseUrl(..))
import           Servant.Client -- (BaseUrl(..), runClientM)

import           Test.Hspec

import Grendel.API (app, api, ping)

withGrendelApp :: (Warp.Port -> IO ()) -> IO ()
withGrendelApp = Warp.testWithApplication (pure app)

spec_businessLogic :: Spec
spec_businessLogic = do
  around withGrendelApp $ do
    let pingClient = client api
    baseUrl <- runIO (parseBaseUrl "http://localhost")
    manager <- runIO (newManager defaultManagerSettings)
    let clientEnv port = mkClientEnv manager (baseUrl { baseUrlPort = port })

    describe "GET /ping" $ do
      it "should reply with True" $ \port -> do
        result <- runClientM pingClient (clientEnv port)
        result `shouldBe` Right True