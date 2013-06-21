{-# LANGUAGE TemplateHaskell, QuasiQuotes, OverloadedStrings, TypeFamilies, MultiParamTypeClasses, Arrows #-}
module HWebUI (
  GSChannel,
  
  wInitGUI,
  wButton,
  wRadioButton,
  wTextBox,
  wCheckBox,
  wNumberTextBox,
  wHtml,
  
  checkBoxW,
  textBoxW,
  numberTextBoxW,
  radioButtonW,
  htmlW,
  buttonW,
  
  runWebserver,
  forkChild,
  waitForChildren
  
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
import Data.Attoparsec.Number as N


-- communication infrastructure
--

-- there are two communication channels, one from the web javascript to the backends yesod server, another one 
-- from the backend yesod server to the GUI FRP frontend process. For both different types of messages are used.
-- In the first case, messages are translated to JSON for sending over websockets.

-- purpose of signal, currently only value setting or change notification are used
data GUISignal = SetValue | OnChange deriving (Show, Read, Eq)
instance J.FromJSON GUISignal where
  parseJSON (String sig) = return (read (unpack sig))
  parseJSON _ = mzero
instance J.ToJSON GUISignal where
  toJSON sig = String (pack (show sig))

-- the values transmitted being the signal itself
data GUISignalValue = SVDouble Double | SVString String | SVInt Integer | SVBool Bool | SVEvent deriving (Show, Read)
instance J.FromJSON GUISignalValue where
  parseJSON (String "Event") = return SVEvent
  parseJSON (Number (N.I i)) = return $ SVInt i
  parseJSON (Number (N.D d)) = return $ SVDouble d
  parseJSON (Bool b) = return $ SVBool b
  parseJSON (String s) = return $ SVString (unpack s)
  parseJSON _ = mzero
  
instance J.ToJSON GUISignalValue where
  toJSON (SVDouble d) = toJSON d
  toJSON (SVString s) = toJSON s
  toJSON (SVInt i) = toJSON i
  toJSON (SVBool b) = toJSON b
  toJSON (SVEvent) = String "Event"

-- GuiElementId is the String identifying a GUI element
type GUIElementId = String
data GUIElementType = Button | CheckBox | TextBox | NumberTextBox | RadioButton | Html deriving (Show, Read)
instance J.FromJSON GUIElementType where
  parseJSON (String sig) = return (read (unpack sig))
  parseJSON _ = mzero
instance J.ToJSON GUIElementType where
  toJSON sig = String (pack (show sig))
 
-- The Messages, sended over the websocket wires

data GUIMessage = GUIMessage {
  gmId :: GUIElementId,
  gmSignal :: GUISignal,
  gmValue :: GUISignalValue,
  gmType :: GUIElementType
  } deriving Show

instance J.FromJSON GUIMessage where
  parseJSON (Object v) = GUIMessage <$>
                         v J..: "gmId" <*>
                         v J..: "gmSignal" <*>
                         v J..: "gmValue" <*>
                         v J..: "gmType"
  parseJSON _ = mzero
instance J.ToJSON GUIMessage where
  toJSON (GUIMessage gmId gmSignal gmValue gmType) = Yesod.object ["gmId" .= gmId, "gmSignal" .= gmSignal, "gmValue" .= gmValue, "gmType" .= gmType]
  
-- communication between GUI Frontend and webserver over MVar lists as GSProvider, Channels

type GSProvider = MVar [GUIMessage]

_sendGS :: GSProvider -> GUIMessage -> IO ()
_sendGS gsp gs = do
  gsList <- takeMVar gsp
  let gsListNew = gsList ++ [gs]
  putMVar gsp gsListNew
  return ()
  
_receiveGS :: GSProvider -> IO (Maybe GUIMessage)
_receiveGS gsp = do
  gsList <- takeMVar gsp
  let (gsListNew, rval) = case gsList of
        (gs:gss) -> (gss, Just gs)
        [] -> ([], Nothing)
  putMVar gsp gsListNew
  return rval
  
type GSChannel = (GUIMessage -> IO (), IO (Maybe GUIMessage), GUIMessage -> IO (), IO (Maybe GUIMessage))

createChannel :: IO GSChannel
createChannel = do
  rMV <- newMVar ([]::[GUIMessage])
  sMV <- newMVar ([]::[GUIMessage])
  return (_sendGS rMV, _receiveGS sMV, _sendGS sMV, _receiveGS rMV )
                                                  
receiveGS :: GSChannel -> IO (Maybe GUIMessage)
receiveGS (s, r, s', r') = r

sendGS :: GSChannel -> GUIMessage -> IO ()
sendGS (s, r, s', r') = s

receiveGS' :: GSChannel -> IO (Maybe GUIMessage)
receiveGS' (s, r, s', r') = r'

sendGS' :: GSChannel -> GUIMessage -> IO ()
sendGS' (s, r, s', r') = s'


-- Netwire Types
--
type GUIWire a b = Wire () IO a b

-- Webgui - Yesod definitions
--

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


                            
wInitGUI :: Int -> Widget
wInitGUI port = do
  let portStr = show port
  addScriptRemote("http://ajax.googleapis.com/ajax/libs/dojo/1.9.0/dojo/dojo.js")
  addStylesheetRemote("//ajax.googleapis.com/ajax/libs/dojo/1.9.0/dijit/themes/claro/claro.css")
  toWidget [julius|
            theGuiSocket = new WebSocket("ws://localhost:#{rawJS portStr}/guisocket");

            createMessage = function (gmId, gmSignal, gmValue, gmType) {
                                 var msg = new Object();
                                 msg.gmId = gmId;
                                 msg.gmSignal = gmSignal;
                                 msg.gmValue = gmValue;
                                 msg.gmType = gmType;
                                 return msg;
                           };
            sendMessage = function (gmId, gmSignal, gmValue, gmType) {
                                 var msg = createMessage(gmId, gmSignal, gmValue, gmType);
                                 theGuiSocket.send(JSON.stringify(msg));
                           };
            transferMessage = function (msg) {
                                 theGuiSocket.send(JSON.stringify(msg));
                           };

            require(["dojo/ready", "dijit/form/Button", "dojo/dom", "dojo/json", "dijit/registry"], function(ready, Button, dom, JSON, registry){

                       theGuiSocket.onmessage = function(evt) {
                            // get data object
                            if (evt.data instanceof Blob) {
                                // do not handle
                            } 
                            else
                            {
                              message = JSON.parse(evt.data, true);
                              if (message.gmSignal == "SetValue")
                              {
                                elem =  registry.byId(message.gmId)
                                if (message.gmType != "Html") {
                                   elem.set("value", message.gmValue);
                                } else {
                                   dom.byId(message.gmId).innerHTML = message.gmValue;
                                }                              
                                if (message.gmType == "CheckBox") {
                                   elem.checked = message.gmValue;
                                }
                              } 
                            }
                       }
                       theGuiSocket.onopen = function(evt) {
                       }
             });  

           |]

wButton :: String -> String -> Widget
wButton wid label = do
  toWidget [julius|
            require(["dojo/ready", "dijit/form/Button", "dojo/dom", "dojo/json"], function(ready, Button, dom, JSON){
                       ready(function(){
                                // Create a button programmatically:
                                var myButton = new Button({
                                                             label: "#{rawJS label}",
                                                             onClick: function(){
                                                               sendMessage("#{rawJS wid}", "OnChange", "Event", "Button");
                                                             }
                                                           }, '#{rawJS wid}');
                       });
             });  
           |]
  toWidget [hamlet|
           <button id="#{wid}" type="button">
            |]

wCheckBox :: String -> Widget
wCheckBox wid = do
  toWidget [julius|
            require(["dojo/ready", "dijit/form/CheckBox", "dojo/dom", "dojo/json"], function(ready, CheckBox, dom, JSON){
                       ready(function(){
                                // Create a checkbox programmatically:
                                var myCheckBox = new CheckBox({
                                                             onChange: function(val){
                                                               sendMessage("#{rawJS wid}", "OnChange", val, "CheckBox");
                                                             }
                                                           }, '#{rawJS wid}');
                       });
             });  
           |]
  toWidget [hamlet|
           <input id="#{wid}" type="checkbox">
            |]

wTextBox :: String -> Widget
wTextBox wid = do
  toWidget [julius|
            require(["dojo/ready", "dijit/form/TextBox", "dojo/dom", "dojo/json"], function(ready, TextBox, dom, JSON){
                       ready(function(){
                                // Create a text box programmatically:
                                var myTextBox = new TextBox({
                                                             onChange: function(val){
                                                               sendMessage("#{rawJS wid}", "OnChange", val, "TextBox");
                                                             },
                                                             intermediateChanges: true
                                                             }, '#{rawJS wid}');
                       });
             });  
           |]
  toWidget [hamlet|
           <input id="#{wid}" type="textbox">
            |]

wNumberTextBox :: String -> Widget
wNumberTextBox wid = do
  toWidget [julius|
            require(["dojo/ready", "dijit/form/NumberTextBox", "dojo/dom", "dojo/json"], function(ready, NumberTextBox, dom, JSON){
                       ready(function(){
                                // Create a number text box programmatically:
                                var myTextBox = new NumberTextBox({
                                                             onChange: function(val){
                                                               sendMessage("#{rawJS wid}", "OnChange", val, "NumberTextBox");
                                                             },
                                                             intermediateChanges: true,
                                                             name: "#{rawJS wid}",
                                                             constraints: {pattern: "0.##", min: -100, max: 100, places: 0}
                                                             }, '#{rawJS wid}');
                       });
             });  
           |]
  toWidget [hamlet|
           <input id="#{wid}" type="text">
            |]

wHtml :: String -> Widget
wHtml wid = do
  toWidget [hamlet|
           <div id="#{wid}">
                   |]

wRadioButton :: String -> String -> String -> Bool -> Widget
wRadioButton wid name value checked = do
  toWidget [julius|
            require(["dojo/ready", "dijit/form/RadioButton", "dojo/dom", "dojo/json"], function(ready, RadioButton, dom, JSON){
                       ready(function(){
                                var myRadioButton = new RadioButton({
                                                             onChange: function(val){
                                                               sendMessage("#{rawJS wid}", "OnChange", val, "RadioButton");
                                                             },
                                                             name: "#{rawJS name}",
                                                             value: "#{rawJS value}",
                                                             checked: #{rawJS checked}
                                                             }, '#{rawJS wid}');
                       });
             });  
           |]
  toWidget [hamlet|
           <input type="radio" id="#{wid}" name="#{name}">
            |]




getGuioneR :: Handler RepHtml
getGuioneR = do
  ys <- getYesod
  let lt = guiLayout ys
  claroLayout $ do
    lt

receiveJson :: J.FromJSON a => WS.WebSockets WS.Hybi10 a
receiveJson = do
  bs <- WS.receiveData
  liftIO $ print bs
  liftIO $ hFlush stdout
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
    sink <- WS.getSink
    case item of
         Just (GUIMessage gmId gmSignal gmValue gmType) -> do
           
                if True then do -- debug flag
                  liftIO $ print ""
                  liftIO $ print ("gui element: " ++ gmId)
                  liftIO $ print "--------------------------"
                  liftIO $ print ("signal: " ++ (show gmSignal))
                  liftIO $ print ("value: " ++ (show gmValue))
                  liftIO $ print ("type: " ++ (show gmType))
                  liftIO $ hFlush stdout
                  return ()
                  else do
                    return ()
                    
                liftIO $ sendGS' (gsmap ! gmId) (GUIMessage gmId gmSignal gmValue gmType)
                return ()
         _ -> do
           return ()
           
    readSocket gsmap
    return ()
  

messageSocket :: Webgui -> WS.WebSockets WS.Hybi10 ()
messageSocket (Webgui gsmap gl) = do
--  liftIO $ print "message Socket created"
--  liftIO $ hFlush stdout
  sink <- WS.getSink
  
  -- run forever loop, which checks incoming messages from the MVar lists and forwards them over the websockets interface
  
  let gsList = toList gsmap
  _ <- liftIO . forkIO . forever $ do
                   sequence $ fmap (\(k, v) -> do
                        gmsMB <- liftIO $ receiveGS' v
                        case gmsMB of
                          Just guimsg -> do
                            liftIO $ sendSinkJson sink $ guimsg
                            return ()
                          Nothing -> do
                            return ()) gsList
                   return ()
                          
  liftIO $ threadDelay 1000
    
  -- read sockets
  readSocket gsmap
  return ()
  



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
        HWebUI.forkFinally io (\_ -> putMVar mvar ())

forkFinally :: IO a -> (Either SomeException a -> IO ()) -> IO ThreadId
forkFinally action and_then =
  mask $ \restore ->
    forkIO $ try (restore action) >>= and_then
    
--
-- here we start with the netwire gui elements
-- 

-- simple IO based wire type with no exception types


-- checkbox wire
--


guiWireGen :: String -> Map String GSChannel -> (String -> GSChannel -> IO (GUIWire a b)) -> IO (GUIWire a b, Map String GSChannel)
guiWireGen elid gsMap wireIn = do
  chan <- createChannel
  let gsMapNew = Data.Map.insert elid chan gsMap
  wireOut <- wireIn elid chan
  return (wireOut, gsMapNew)  

checkBoxW' :: String -> GSChannel -> IO (GUIWire (Maybe Bool) Bool)
checkBoxW' boxid channel = do
  let wire =  mkStateM False (\t (trigger, s) -> do
                                 case trigger of
                                   Just bState -> do
                                     sendGS channel (GUIMessage boxid SetValue (SVBool bState) CheckBox)
                                     return (Right bState, bState)
                                   Nothing -> do
                                     rcv <- receiveGS channel
                                     case rcv of
                                       Just guimsg -> do
                                         if (gmSignal guimsg) == OnChange then do
                                             let (SVBool bState) = gmValue guimsg
                                             return (Right bState, bState)
                                             else
                                               return (Right s, s)
                                       Nothing -> do
                                         return (Right s, s)   )
  return wire                                                         

checkBoxW elid gsMap = guiWireGen elid gsMap checkBoxW'
  
radioButtonW' :: String -> GSChannel -> IO (GUIWire (Maybe Bool) Bool)
radioButtonW' boxid channel = do
  let wire =  mkStateM False (\t (trigger, s) -> do
                                 case trigger of
                                   Just bState -> do
                                     sendGS channel (GUIMessage boxid SetValue (SVBool bState) RadioButton)
                                     return (Right bState, bState)
                                   Nothing -> do
                                     rcv <- receiveGS channel
                                     case rcv of
                                       Just guimsg -> do
                                         if (gmSignal guimsg) == OnChange then do
                                             let (SVBool bState) = gmValue guimsg
                                             return (Right bState, bState)
                                             else
                                               return (Right s, s)
                                       Nothing -> do
                                         return (Right s, s)   )
  return wire                                                         

radioButtonW elid gsMap = guiWireGen elid gsMap radioButtonW'
  
textBoxW' :: String -> GSChannel -> IO (GUIWire (Maybe String) String)
textBoxW' boxid channel = do
  let wire =  mkStateM "" (\t (trigger, s) -> do
                                 case trigger of
                                   Just bState -> do
                                     sendGS channel (GUIMessage boxid SetValue (SVString bState) TextBox)
                                     return (Right bState, bState)
                                   Nothing -> do
                                     rcv <- receiveGS channel
                                     case rcv of
                                       Just guimsg -> do
                                         if (gmSignal guimsg) == OnChange then do
                                           let (SVString bState) = gmValue guimsg
                                           return (Right bState, bState)
                                           else
                                             return (Right s, s)
                                       Nothing -> do
                                         return (Right s, s)   )
  return wire                                                         
  
textBoxW elid gsMap = guiWireGen elid gsMap textBoxW'

numberTextBoxW' :: String -> GSChannel -> IO (GUIWire (Maybe Double) Double)
numberTextBoxW' boxid channel = do
  let wire =  mkStateM 0 (\t (trigger, s) -> do
                                 case trigger of
                                   Just bState -> do
                                     sendGS channel (GUIMessage boxid SetValue (SVDouble bState) NumberTextBox)
                                     return (Right bState, bState)
                                   Nothing -> do
                                     rcv <- receiveGS channel
                                     case rcv of
                                       Just guimsg -> do
                                         if (gmSignal guimsg) == OnChange then do
                                           let (SVDouble bState) = gmValue guimsg
                                           return (Right bState, bState)
                                           else
                                             return (Right s, s)
                                       Nothing -> do
                                         return (Right s, s)   )
  return wire                                                         
  
numberTextBoxW elid gsMap = guiWireGen elid gsMap numberTextBoxW'

htmlW' :: String -> GSChannel -> IO (GUIWire (Maybe String) String)
htmlW' boxid channel = do
  let wire =  mkStateM "" (\t (trigger, s) -> do
                                 case trigger of
                                   Just bState -> do
                                     sendGS channel (GUIMessage boxid SetValue (SVString bState) Html)
                                     return (Right bState, bState)
                                   Nothing -> do
                                     return (Right s, s) )
  return wire                                                         
  
htmlW elid gsMap = guiWireGen elid gsMap htmlW'

-- button wire
--

buttonW' :: String -> GSChannel -> IO (GUIWire a a)
buttonW' boxid channel = do
  let wire =  mkFixM (\t var -> do
                                     rcv <- receiveGS channel
                                     case rcv of
                                       Just gs -> do
                                           return $ Right var
                                       _ -> do
                                           return $ Left () )
  return wire                                                         
  
buttonW elid gsMap = guiWireGen elid gsMap buttonW'

