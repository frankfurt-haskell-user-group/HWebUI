{-# LANGUAGE TemplateHaskell, QuasiQuotes, OverloadedStrings, TypeFamilies, MultiParamTypeClasses, Arrows #-}


module Server (
  Widget (..),
  Webgui (..),
  
  -- ** Functions to run the GUI  
  forkChild,
  runWebserver,
  waitForChildren,
 
  ) where

import Yesod
import Network.Wai.Handler.Warp (runSettings, Settings(..), defaultSettings)
import qualified Network.WebSockets             as WS
import qualified Network.Wai.Handler.WebSockets as WS
import qualified Data.Aeson                     as J
import System.IO (hFlush, stdout)
import Control.Applicative
import Control.Monad
import Text.Julius (rawJS)
import Control.Concurrent
import Control.Exception (SomeException, mask, try)
import System.IO.Unsafe
import Control.Wire
import Prelude hiding ((.), id)
import Data.Map
import Data.Text
import Data.Vector (toList, fromList)
import Data.Attoparsec.Number as N

import GUIValue
import GUIEvent
import GUICommand
import GUISignal
import Messaging

-------------------------------------
-- Yesod widget parts of GUI elements
-------------------------------------

{- $guilayout

Yesod provides a mechanism, to combine Javascript, HTML and Templating to build a powerful abstraction for Widgets. Within HWebUI this 
mechanism is used to build the GUI Layout from single elements. Basically, HWebUI provides Yesod widgets, which you can incorporate directly
into HTML to obtain your GUI Layout. Those Yesod widgets include already all needed Javascript and HTML functionality for the individual
GUI Elements. 

Each HWebUI program has the following sections: settings, create layout, create functionality, run GUI. Here we focus on the section: create layout.

Take as example the layout part of the currency converter application example:

>    let guiLayout = do    
>        wInitGUI port
>        
>        toWidget [hamlet|
>              <H1>HWebUI - Currency Example
>              <p>
>                    |]
>
>        [whamlet|
>           <table>
>                   <tr>
>                     <td> US Dollars
>                     <td> ^{wTextBox "tb-dollars"}
>                   <tr>
>                     <td> Euros
>                     <td> ^{wTextBox "tb-euros"} 
>                             |]

You can see nicely, how the guiLayout is composed from Yesod widgets. In the beginning the wInitGUI widget set up needed Javascript libraries. After that you can find GUI element widgets like \"wTextBox\" interspersed with regular HTML given in hamlet or whamlet notation. In whamlet templates it is even possible, to intermix Yesod widgets with HTML, which makes GUI layout a snap. 

-}

data Webgui = Webgui {
  channelMap :: (Map String GSChannel), 
  guiLayout :: GWidget Webgui Webgui () }

mkYesod "Webgui" [parseRoutes|
/webgui   GuioneR GET
|]

instance Yesod Webgui 

claroLayout w = do
        p <- widgetToPageContent w
        mmsg <- getMessage
        hamletToRepHtml [hamlet|
            $newline never
            $doctype 5
            <html>
                <head>
                    <title>#{pageTitle p}
                    ^{pageHead p}
                <body class="claro">
                    $maybe msg <- mmsg
                        <p .message>#{msg}
                    ^{pageBody p}
            |]


jsUtils = [julius|
             function evtJson(evt, f) {
               var str;
               if(evt.data instanceof Blob) {
                 var reader = new FileReader();
                 reader.onload = function() {
                   f($.parseJSON(reader.result));
                 }
                 reader.readAsText(evt.data);
               } else {
                 f($.parseJSON(evt.data));
               }
             };
          |]

-----------------------
-- misc Yesod functions
-----------------------

getGuioneR :: Handler RepHtml
getGuioneR = do
  ys <- getYesod
  let lt = guiLayout ys
  claroLayout $ do
    lt

receiveJson :: J.FromJSON a => WS.WebSockets WS.Hybi10 a
receiveJson = do
  bs <- WS.receiveData
--  liftIO $ print ("to browser: " ++ (show bs))
--  liftIO $ hFlush stdout
  case J.decode' bs of
    Nothing -> sendJson (jsonError "invalid message") >> receiveJson
    Just a  -> return a

sendJson :: J.ToJSON a => a -> WS.WebSockets WS.Hybi10 ()
sendJson j = WS.sendBinaryData (J.encode j)

sendSinkJson :: J.ToJSON a => WS.Sink WS.Hybi10 -> a -> IO ()
sendSinkJson sink j = WS.sendSink sink $ WS.textData (J.encode j)

jsonError :: String -> Value
jsonError msg = J.object [ "error" .= msg ]

sockets :: Webgui -> WS.Request -> WS.WebSockets WS.Hybi10 ()
sockets y req
  | WS.requestPath req == "/guisocket" = accept messageSocket
  | otherwise                      = WS.rejectRequest req "Not found"
  where
    accept a = WS.acceptRequest req >> a y


-- routine, which regularly reads the messages from socket and distribute to the MVAR Lists
-- 
    
readSocket :: Map String GSChannel -> WS.WebSockets WS.Hybi10 ()
readSocket gsmap = do
    item <- receiveJson
    case item of
         Just (GUIMessage gmId gmSignal gmValue gmType) -> do
                
--                liftIO $ print ("from browser: " ++ (show item))
--                liftIO $ hFlush stdout
                    
                liftIO $ sendGS' (gsmap ! gmId) (GUIMessage gmId gmSignal gmValue gmType)
                return ()
         _ -> do
           return ()
           
    liftIO $ threadDelay 1000
    readSocket gsmap
    return ()
  

messageSocket :: Webgui -> WS.WebSockets WS.Hybi10 ()
messageSocket (Webgui gsmap gl) = do
--  liftIO $ print "message Socket created"
--  liftIO $ hFlush stdout
  sink <- WS.getSink
  
  -- run forever loop, which checks incoming messages from the MVar lists and forwards them over the websockets interface
  
  let gsList = Data.Map.toList gsmap
  _ <- liftIO . forkIO . forever $ do
                   sequence $ fmap (\(k, v) -> do
                        gmsMB <- liftIO $ receiveGS' v
                        case gmsMB of
                          Just guimsg -> do
                            liftIO $ sendSinkJson sink $ guimsg
                            return ()
                          Nothing -> do
                            return ()) gsList
                   liftIO $ threadDelay 1000
                   return ()
                          
  -- start readsocket
  readSocket gsmap
  return ()
  
------------------
-- Running the GUI
------------------

{- $rungui
  
Two processes needs to be started to run the GUI functionality. The Yesod webserver and the netwire FRP loop. This is done by calling the functions as shown in the following example:

>    -- run the webserver   
>    forkChild $ runWebserver port gsmap2 guiLayout
>    -- loop netwire
>    loop1 theWire clockSession
>    -- wait for the webserver to terminate
>    waitForChildren

-}

runWebserver :: Int -> Map String GSChannel -> GWidget Webgui Webgui () -> IO ()
runWebserver port gsmap guiLayout = do
    let master = Webgui gsmap guiLayout
        s      = defaultSettings
                  { settingsPort = port
                  , settingsIntercept = WS.intercept (sockets master)
                  }
    runSettings s =<< toWaiApp master




children :: MVar [MVar ()]
children = unsafePerformIO (newMVar [])
    
waitForChildren :: IO ()
waitForChildren = do
      cs <- takeMVar children
      case cs of
        []   -> return ()
        m:ms -> do
           putMVar children ms
           takeMVar m
           waitForChildren

forkChild :: IO () -> IO ThreadId
forkChild io = do
        mvar <- newEmptyMVar
        childs <- takeMVar children
        putMVar children (mvar:childs)
        Server.forkFinally io (\_ -> putMVar mvar ())

forkFinally :: IO a -> (Either SomeException a -> IO ()) -> IO ThreadId
forkFinally action and_then =
  mask $ \restore ->
    forkIO $ try (restore action) >>= and_then
    

