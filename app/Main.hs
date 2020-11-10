
module Main where

import Network.Wai.Handler.Warp (run)
import Grendel.API (app)

main :: IO ()
main = run 8000 app
