
module Grendel.Drive where

import           Control.Monad.Trans.Resource (runResourceT)
import           Data.Functor ((<&>))
import           Lens.Micro ((.~))
import qualified Network.Google as Google
import qualified Network.Google.Drive as Drive
import           System.IO (stdout)

getDriveAbout :: IO ()
getDriveAbout = do
  logger <- Google.newLogger Google.Debug stdout
  env <- Google.newEnv <&> (Google.envLogger .~ logger)
                         . (Google.envScopes .~ Drive.driveReadOnlyScope)

  eh <- runResourceT . Google.runGoogle env $
    Google.send Drive.aboutGet

  print eh