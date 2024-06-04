{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Slab.Serve
  ( run
  ) where

import Network.Wai qualified as Wai
import Network.Wai.Handler.Warp qualified as Warp
import Protolude hiding (Handler)
import Servant hiding (serve)
import Servant.HTML.Blaze qualified as B
import Servant.Server qualified as Server
import Text.Blaze.Html5 (Html)
import WaiAppStatic.Storage.Filesystem
  ( defaultWebAppSettings
  )

------------------------------------------------------------------------------
run :: FilePath -> IO ()
run distDir =
  Warp.run 9000 $ serve distDir

-- | Turn our `serverT` implementation into a Wai application, suitable for
-- Warp.run.
serve :: FilePath -> Wai.Application
serve root =
  Servant.serveWithContext appProxy Server.EmptyContext $
    Server.hoistServerWithContext appProxy settingsProxy identity $
      serverT root

------------------------------------------------------------------------------
type ServerSettings = '[]

settingsProxy :: Proxy ServerSettings
settingsProxy = Proxy

------------------------------------------------------------------------------
type App =
  "hello" :> Get '[B.HTML] Html
    :<|> Servant.Raw -- Fallback handler for the static files, in particular the

appProxy :: Proxy App
appProxy = Proxy

------------------------------------------------------------------------------
serverT :: FilePath -> ServerT App Handler
serverT root =
  showHelloPage
    :<|> serveStatic root

------------------------------------------------------------------------------
showHelloPage :: Handler Html
showHelloPage = pure "Hello."

------------------------------------------------------------------------------
serveStatic :: FilePath -> Server.Tagged Handler Server.Application
serveStatic root = Servant.serveDirectoryWith settings
 where
  settings = defaultWebAppSettings root
