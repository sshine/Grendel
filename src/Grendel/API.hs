{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Grendel.API where

import Servant

type API = "ping" :> Get '[JSON] Bool

api :: Proxy API
api = Proxy

app :: Application
app = serve api server

server :: Server API
server = ping

ping :: Server API
ping = pure True