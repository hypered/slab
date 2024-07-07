{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- |
-- Module      : Slab.Serve
-- Description : Run a development server to preview Slab templates
--
-- @Slab.Serve@ watches a set of Slab templates, continuously rebuilding them
-- as they change, and runs a web server to serve them. Pages containing a
-- @head@ element are modified to include a bit of JavaScript. It serves at
-- auto-reloading the page when it is rebuilt (by connecting to a websocket).
module Slab.Serve
  ( run
  ) where

import Control.Concurrent.Chan qualified as Chan
import Control.Concurrent.STM qualified as STM
import Data.Map qualified as M
import Data.Text qualified as T
import Data.Text.Lazy.Encoding qualified as TLE
import Network.HTTP.Types (status200)
import Network.Wai qualified as Wai
import Network.Wai.Handler.Warp qualified as Warp
import Network.WebSockets.Connection
  ( Connection
  , sendTextData
  , withPingThread
  )
import Protolude hiding (Handler)
import Servant hiding (serve)
import Servant.API.WebSocket (WebSocket)
import Servant.HTML.Blaze qualified as B
import Servant.Server qualified as Server
import Slab.Build qualified as Build
import Slab.Command qualified as Command
import Slab.Evaluate qualified as Evaluate
import Slab.Render qualified as Render
import Slab.Syntax qualified as Syntax
import Slab.Watch qualified as Watch
import System.FilePath (takeExtension)
import Text.Blaze.Html5 (Html)
import Text.Pretty.Simple (pShowNoColor)
import WaiAppStatic.Storage.Filesystem
  ( defaultWebAppSettings
  )

------------------------------------------------------------------------------
run :: FilePath -> FilePath -> IO ()
run srcDir distDir = do
  store <- atomically $ STM.newTVar M.empty
  chan <- Chan.newChan
  -- Initial build to populate the store...
  Build.buildDirInMemory srcDir Command.RenderNormal Command.RunPassthrough store
  -- ...then rebuild individual files upon change, and notify the channel.
  _ <-
    forkIO $
      Watch.run srcDir $ \path -> do
        when (takeExtension path == ".slab") $
          Build.buildFileInMemory srcDir Command.RenderNormal Command.RunPassthrough store path
        when (takeExtension path /= ".slab") $
          -- Rebuild everything. TODO create a dependency graph and rebuild
          -- only what is needed.
          Build.buildDirInMemory srcDir Command.RenderNormal Command.RunPassthrough store
        Chan.writeChan chan path
  Warp.run 9000 $ serve distDir store chan

-- | Turn our `serverT` implementation into a Wai application, suitable for
-- Warp.run.
serve :: FilePath -> Build.StmStore -> Chan FilePath -> Wai.Application
serve root store chan =
  Servant.serveWithContext appProxy Server.EmptyContext $
    Server.hoistServerWithContext appProxy settingsProxy identity $
      serverT root store chan

------------------------------------------------------------------------------
type ServerSettings = '[]

settingsProxy :: Proxy ServerSettings
settingsProxy = Proxy

------------------------------------------------------------------------------
type App =
  "hello" :> Get '[B.HTML] Html
    :<|> WebSocketApi
    :<|> Servant.Raw -- Fallback handler for the static files.

appProxy :: Proxy App
appProxy = Proxy

------------------------------------------------------------------------------
serverT :: FilePath -> Build.StmStore -> Chan FilePath -> ServerT App Handler
serverT root store chan =
  showHelloPage
    :<|> websocket chan
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
    Just mblocks -> do
      case mblocks of
        Right blocks -> do
          let blocks' = Syntax.addScript autoreloadScript $ Evaluate.simplify blocks
          sendRes $
            Wai.responseLBS
              status200
              [("Content-Type", "text/html")]
              (Render.renderHtmlsUtf8 $ Render.renderBlocks blocks')
        Left err -> do
          sendRes $
            Wai.responseLBS
              status200
              [("Content-Type", "text/plain")]
              (TLE.encodeUtf8 $ pShowNoColor err)
    Nothing -> do
      let Tagged staticApp = serveStatic root
      staticApp req sendRes

------------------------------------------------------------------------------
serveStatic :: FilePath -> Server.Tagged Handler Server.Application
serveStatic root = Servant.serveDirectoryWith settings
 where
  settings = defaultWebAppSettings root

------------------------------------------------------------------------------
-- Accept websocket connections, and keep them alive.
type WebSocketApi = "ws" :> WebSocket

-- | Sends messages to the browser whenever a message is written to the channel.
websocket :: Chan FilePath -> Connection -> Handler ()
websocket chan con =
  liftIO $ do
    chan' <- dupChan chan
    withPingThread con 30 (pure ()) $
      liftIO . forever $ do
        path <- Chan.readChan chan'
        sendTextData con (T.pack $ "updated: " <> path)

-- | The auto-reload script. It connects to a websocket and refreshes the
-- current page when it receives a message from the server. Such a message is
-- sent whenever a @.slab@ file is rebuilt.
autoreloadScript :: Text
autoreloadScript =
  "function connect(isInitialConnection) {\n\
  \  // Create WebSocket connection.\n\
  \  var ws = new WebSocket('ws://' + location.host + '/ws');\n\
  \\n\
  \  // Connection opened\n\
  \  ws.onopen = function() {\n\
  \    ws.send('Hello server.');\n\
  \    if (isInitialConnection) {\n\
  \      console.log('autoreload: Initial connection.');\n\
  \    } else {\n\
  \      console.log('autoreload: Reconnected.');\n\
  \      location.reload();\n\
  \    }\n\
  \  };\n\
  \\n\
  \  // Listen for messages.\n\
  \  ws.onmessage = function(ev) {\n\
  \    console.log('autoreload: Message from server:', ev.data);\n\
  \    if (ev.data.startsWith('updated:')) {\n\
  \      location.reload();\n\
  \    }\n\
  \  };\n\
  \\n\
  \  // Trying to reconnect when the socket is closed.\n\
  \  ws.onclose = function(ev) {\n\
  \    console.log('autoreload: Socket closed. Trying to reconnect in 0.5 second.');\n\
  \    setTimeout(function() { connect(false); }, 500);\n\
  \  };\n\
  \\n\
  \  // Close the socker upon error.\n\
  \  ws.onerror = function(err) {\n\
  \    console.log('autoreload: Socket errored. Closing socket.');\n\
  \    ws.close();\n\
  \  };\n\
  \}\n\
  \\n\
  \connect(true);\n"
