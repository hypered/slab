{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Slab.Serve
  ( run
  ) where

import Control.Concurrent.STM qualified as STM
import Data.Map qualified as M
import Data.Text qualified as T
import Network.HTTP.Types (status200)
import Network.Wai qualified as Wai
import Network.Wai.Handler.Warp qualified as Warp
import Protolude hiding (Handler)
import Servant hiding (serve)
import Servant.HTML.Blaze qualified as B
import Servant.Server qualified as Server
import Slab.Build qualified as Build
import Slab.Command qualified as Command
import Slab.Render qualified as Render
import Slab.Watch qualified as Watch
import Text.Blaze.Html5 (Html)
import WaiAppStatic.Storage.Filesystem
  ( defaultWebAppSettings
  )

------------------------------------------------------------------------------
run :: FilePath -> FilePath -> IO ()
run srcDir distDir = do
  store <- atomically $ STM.newTVar M.empty
  -- Initial build to populate the store.
  Build.buildDirInMemory srcDir Command.RenderNormal store
  -- Then rebuild one file upon change.
  _ <-
    forkIO $
      Watch.run srcDir (Build.buildFileInMemory srcDir Command.RenderNormal store)
  Warp.run 9000 $ serve distDir store

-- | Turn our `serverT` implementation into a Wai application, suitable for
-- Warp.run.
serve :: FilePath -> Build.StmStore -> Wai.Application
serve root store =
  Servant.serveWithContext appProxy Server.EmptyContext $
    Server.hoistServerWithContext appProxy settingsProxy identity $
      serverT root store

------------------------------------------------------------------------------
type ServerSettings = '[]

settingsProxy :: Proxy ServerSettings
settingsProxy = Proxy

------------------------------------------------------------------------------
type App =
  "hello" :> Get '[B.HTML] Html
    :<|> Servant.Raw -- Fallback handler for the static files.

appProxy :: Proxy App
appProxy = Proxy

------------------------------------------------------------------------------
serverT :: FilePath -> Build.StmStore -> ServerT App Handler
serverT root store =
  showHelloPage
    :<|> app root store

------------------------------------------------------------------------------
showHelloPage :: Handler Html
showHelloPage = pure "Hello."

------------------------------------------------------------------------------

-- | Try to serve a built page, and fallback to static files if the page
-- doesn't exist.
app :: FilePath -> Build.StmStore -> Server.Tagged Handler Server.Application
app root store = Tagged $ \req sendRes -> app' root store req sendRes

app' :: FilePath -> Build.StmStore -> Application
app' root store req sendRes = do
  templates <- liftIO . atomically $ STM.readTVar store
  let path = T.intercalate "/" $ Wai.pathInfo req
      path' = if T.null path then "index.html" else path
  -- TODO Check requestMethod is GET.
  case M.lookup (T.unpack path') templates of
    Just blocks ->
      sendRes $
        Wai.responseLBS
          status200
          [("Content-Type", "text/html")]
          (Render.renderHtmlsUtf8 $ Render.renderBlocks blocks)
    Nothing -> do
      let Tagged staticApp = serveStatic root
      staticApp req sendRes

------------------------------------------------------------------------------
serveStatic :: FilePath -> Server.Tagged Handler Server.Application
serveStatic root = Servant.serveDirectoryWith settings
 where
  settings = defaultWebAppSettings root
