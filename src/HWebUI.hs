{-# LANGUAGE TemplateHaskell, QuasiQuotes, OverloadedStrings, TypeFamilies, MultiParamTypeClasses, Arrows #-}


{- | HWebUI is providing FRP-based GUI functionality for Haskell by utilizing the Web-Browser. It is build on top of Yesod for the Web technologies and on netwire for the FRP interface. The status is \"early prototype\". The implementation uses a Javascript library (Dojo toolkit) for providing typical widgets, HTML for the layout of the widgets. With Javascript and websockets events are transferred between the Web and the Haskell world. This happens behind the scenes. The Haskell programmer is using a FRP based interface. See also: <http://www.github.com/althainz/HWebUI>.
-}
module HWebUI (
  -- * Creating the GUI Layout with Yesod Widgets
  
  -- ** How HWebUI Yesod widgets can be used 
  -- $guilayout
  
  -- ** Initializing Yesod widget
  wInitGUI,
 
  -- ** The Yesod widgets
  wButton,
  wCheckBox,
  wHtml,
  wMultiSelect,
  wNumberTextBox,
  wRadioButton,
  wTextBox,
  
  -- * Creating the GUI Functionality with FRP
  
  -- ** How HWebUI wires (based on netwire) work
  -- $wiremechanism
  
  -- ** The netwire wires (basic functionality)
  buttonW,
  checkBoxW,
  htmlW,
  multiSelectW,
  numberTextBoxW,
  radioButtonW,
  textBoxW,
  
  -- ** functions to extend basic wires with additional functionality
  -- $advancedwire
  
  
  
  -- * Running the GUI
  
  -- ** How to run the GUI
  -- $rungui
  
  -- ** Functions to run the GUI  
  forkChild,
  runWebserver,
  waitForChildren,
  
  -- ** Miscellaneous
  GSChannel,
 
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

-------------------------------
-- communication infrastructure
-------------------------------

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
data GUISignalValue = SVDouble Double | SVString String | SVStringList [String] | SVInt Integer | SVBool Bool | SVEvent deriving (Show, Read)

instance J.FromJSON GUISignalValue where
  parseJSON (String "Event") = return SVEvent
  parseJSON (Number (N.I i)) = return $ SVInt i
  parseJSON (Number (N.D d)) = return $ SVDouble d
  parseJSON (Bool b) = return $ SVBool b
  parseJSON (String s) = return $ SVString (unpack s)
  parseJSON (Array v) = 
    case Data.Vector.toList v of
      (String s:ss) -> do
        sr <- J.parseJSON (Array (Data.Vector.fromList ss))
        let s1 = unpack s
        return $ SVStringList (s1 : sr)
      [] -> return $ SVStringList []
  parseJSON _ = mzero
  
instance J.ToJSON GUISignalValue where
  toJSON (SVDouble d) = toJSON d
  toJSON (SVString s) = toJSON s
  toJSON (SVInt i) = toJSON i
  toJSON (SVBool b) = toJSON b
  toJSON (SVStringList sl) = toJSON sl
  toJSON (SVEvent) = String "Event"

-- GuiElementId is the String identifying a GUI element
type GUIElementId = String
data GUIElementType = Button | CheckBox | TextBox | MultiSelect | NumberTextBox | RadioButton | Html deriving (Show, Read)
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
  
-- communication between GUI Frontend and webserver
  
-- internal data

type OneChannel = MVar [GUIMessage]

_readChannel :: OneChannel -> IO (Maybe GUIMessage)
_readChannel chan = do
  gsList <- takeMVar chan
  let (gsListNew, rval) = case gsList of
        (gs:gss) -> (gss, Just gs)
        [] -> ([], Nothing)
  putMVar chan gsListNew
  return rval
  
_writeChannel :: OneChannel -> GUIMessage -> IO ()
_writeChannel chan msg = do
  gsList <- takeMVar chan
  putMVar chan (gsList Prelude.++ [msg])
  return ()
  
  
data GSChannel = GSChannel {
  sendToGUI :: OneChannel,
  receiveFromGUI :: OneChannel,
  valueSetFlag :: MVar Bool 
  }
                 
_checkValueSetFlag :: GSChannel -> IO Bool
_checkValueSetFlag gsc = do
  flag <- takeMVar (valueSetFlag gsc)
  putMVar (valueSetFlag gsc) flag
  return flag
  
_setValueSetFlag :: GSChannel -> Bool -> IO ()
_setValueSetFlag gsc flag = do
  flag' <- takeMVar (valueSetFlag gsc)
  putMVar (valueSetFlag gsc) flag
  return ()
  
-- external interface for remaining code

createChannel :: IO GSChannel
createChannel = do
  sendChannel <- newMVar ([]::[GUIMessage])
  receiveChannel <- newMVar ([]::[GUIMessage])
  valueSetFlag <- newMVar False
  return $ GSChannel sendChannel receiveChannel valueSetFlag
  
-- two types of message routines, one used from Server side and one used from gui side

-- used in the netwire server code, to receive and send to and from gui
-- contains logic, to prevent a SetMessage to trigger a changed message
  
receiveGS :: GSChannel -> IO (Maybe GUIMessage)
receiveGS gsc = do
  msg <- _readChannel (receiveFromGUI gsc)
  case msg of
    Just gmsg -> do
      flag <- _checkValueSetFlag gsc
      if flag then do
        _setValueSetFlag gsc False
        return Nothing
        else do
          return $ Just gmsg
    Nothing -> do
      return Nothing
             

sendGS :: GSChannel -> GUIMessage -> IO ()
sendGS gsc msg = do
                 _writeChannel (sendToGUI gsc) msg
                 _setValueSetFlag gsc True
                 return ()

-- used in the handlers to send over the wire towards the gui with JSON format
-- plainly sends and receives, no additional logic

receiveGS' :: GSChannel -> IO (Maybe GUIMessage)
receiveGS' gsc = _readChannel (sendToGUI gsc)

sendGS' :: GSChannel -> GUIMessage -> IO ()
sendGS' gsc msg = _writeChannel (receiveFromGUI gsc) msg


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


-- | Yesod widget to initialize needed Javascript functionality in the HTML code of the GUI. Provides Dojokit inclusion and communication with Haskell Yesod server over websockets.
wInitGUI :: Int -- ^ port used to communicate with Haskell server
            -> Widget -- ^ resulting Yesod widget
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

            require(["dojo/ready", "dijit/form/Button", "dojo/dom", "dojo/json", "dijit/registry", "dojo/dom-construct"], function(ready, Button, dom, JSON, registry, domConstruct){

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
                                elem =  registry.byId(message.gmId);

                                if (message.gmType == "MultiSelect") 
                                {
                                   var sel = dijit.byId(message.gmId);
                                   sel.destroyDescendants();
                                   var dsel = dom.byId(message.gmId);
                                   for(var i in message.gmValue)
                                   {
                                       var c = domConstruct.create('option');
                                       var v = message.gmValue[i];
                                       c.innerHTML = v;
                                       c.value = v;
                                       dsel.appendChild(c);
                                   }
                                }
                                else if (message.gmType == "Html") 
                                {
                                   dom.byId(message.gmId).innerHTML = message.gmValue;
                                }
                                else if (message.gmType == "CheckBox") 
                                {
                                   elem.set("value", message.gmValue);
                                   elem.checked = message.gmValue;
                                }
                                else
                                {
                                   elem.set("value", message.gmValue);
                                }
                              } 
                            }
                       }
                       theGuiSocket.onopen = function(evt) {
                       }
             });  

           |]

-- | Yesod widget for the Button GUI Element
wButton :: String -- ^ Element Id 
           -> String -- ^ Label of the Button 
           -> Widget -- ^ resulting Yesod Widget
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

-- | Yesod widget for the CheckBox GUI Element
wCheckBox :: String -- ^ Element Id
             -> Widget -- ^ resulting Yesod Widget
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

-- | Yesod widget for the TextBox GUI element
wTextBox :: String -- ^ Element Id
            -> Widget -- ^ resulting Yesod widget
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

-- | Yesod widget for the MultiSelect GUI element
wMultiSelect :: String -- ^ Element Id
            -> Widget -- ^ resulting Yesod widget
wMultiSelect wid = do
  toWidget [julius|
            require(["dojo/ready", "dijit/form/MultiSelect", "dojo/dom", "dojo/json"], function(ready, MultiSelect, dom, JSON){
                       ready(function(){
                                // Create a text box programmatically:
                                var myMultiSelect = new MultiSelect({
                                                             onChange: function(val){
                                                               sendMessage("#{rawJS wid}", "OnChange", val, "MultiSelect");
                                                             },
                                                             name: '#{rawJS wid}'
                                                             }, dom.byId('#{rawJS wid}'));
                       });
             });  
           |]
  toWidget [hamlet|
           <select id="#{wid}">
            |]

-- | Yesod widget for the NumberTextBox GUI element
wNumberTextBox :: String -- ^ Element Id
                  -> Widget -- ^ resulting Yesod widget
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

-- | Yesod widget for the PlainHtml GUI element (an element which is used for dynamic HTML output
wHtml :: String -- ^ Element Id
         -> Widget -- ^ resulting Yesod widget
wHtml wid = do
  toWidget [hamlet|
           <div id="#{wid}">
                   |]

-- | Yesod widget for the RadioButtion GUI element
wRadioButton :: String -- ^ Element Id
                -> String -- ^ Name
                -> String -- ^ Value
                -> Bool -- ^ Checked
                -> Widget -- ^ resulting Yesod widget
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
                   return ()
                          
  liftIO $ threadDelay 1000
    
  -- read sockets
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
        HWebUI.forkFinally io (\_ -> putMVar mvar ())

forkFinally :: IO a -> (Either SomeException a -> IO ()) -> IO ThreadId
forkFinally action and_then =
  mask $ \restore ->
    forkIO $ try (restore action) >>= and_then
    


--------------------------------
-- netwire parts of GUI elements
--------------------------------

{- $wiremechanism

Obviously there are different possibilities to build a FRP based API for GUI elements with netwire. This implementation is in stage \"early prototype\" and might serve also as playground to test different designs in this area. The basic wires below all follow the same rules, with regards to notification of changes and setting of values. These rules make up a quite basic mechanism, which can be extended with additional netwire wrappers for more advanced purposes.

/Basic wire behaviour:/

Each GUI element, which carries a value of Type a (TextBox, CheckBox, ...) has an input type of (Maybe a) and an output type of a. To set a new value of the element as input a \"Just a\" needs to be given. A \"Nothing\" as input does not change the value from side of the program. As output the wire fires on each user caused change of the value, not on the case the value gets set by the program. If the value does not change, the wire inhibits (see netwire documentation for that). The reason to choose this model is simply that it is the most basic behaviour, which still contains all information needed and goes with one wire per element.

/How to use the basic wires:/

The following code from the example-arithmetic show how to create the wires, corresponding to the Yesod widgets.

>    -- create netwire gui elements
>    let gsmap = (fromList [])::(Map String GSChannel)
>        
>    (arg1, gsmap) <- textBoxW "arg1" gsmap
>    (arg2, gsmap) <- textBoxW "arg2" gsmap
>    (addB, gsmap) <- radioButtonW "rbadd" gsmap
>    (subB, gsmap) <- radioButtonW "rbsub" gsmap
>    (mulB, gsmap) <- radioButtonW "rbmul" gsmap
>    (divB, gsmap) <- radioButtonW "rbdiv" gsmap
>    (out1, gsmap) <- htmlW "out1" gsmap
        
The anchor between the Yesod widgets and the netwire wire is the \"Element Id\". After the wires have been created, they need to be wired in a way, which implements the wanted functionality. In case of the arithmetic example the wiring is done as follows:

>    -- build the FRP wire, arrow notation
>    
>    let result = proc _ -> do
>                              a1 <- hold "" arg1 -< Nothing
>                              a2 <- hold "" arg2 -< Nothing
> 			       badd <- hold True addB -< Nothing
>      			       bsub <- hold False subB -< Nothing
>                              bmul <- hold False mulB -< Nothing
>                              bdiv <- hold False divB -< Nothing
>                               
>                              let op = if badd then (+) else (if bsub then (-) else (if bmul then (*) else (if bdiv then (/) else (\ x y -> 0.0))))
>                              let res = op (atof a1) (atof a2)
>
>                              returnA -< res                             
>
>    let theWire = out1 .  ((Just . show) <$> result) . pure Nothing


-}


-- Netwire Types
--
type GUIWire a b = Wire () IO a b

-- generic function to create a value wire, a value wire has type "GUIWire (Maybe a) a"

valueWireGen :: String -- ^ Element Id 
                -> Map String GSChannel -- ^ Channel Map
                -> (a -> GUISignalValue) -- ^ svalue creation function
                -> (GUISignalValue -> a) -- ^ svalue extraction function
                -> GUIElementType -- ^ type of GUI Element
                -> IO (GUIWire (Maybe a) a , Map String GSChannel) -- ^ (wire, new Channel Map)
                
valueWireGen elid gsMap svalCreator svalExtractor guitype = do
  
  channel <- createChannel
  let gsMapNew = Data.Map.insert elid channel gsMap
  
  let wire = mkFixM  (\t inVal -> do
                                                case inVal of
                                                  Just bState -> do
                                                    sendGS channel (GUIMessage elid SetValue (svalCreator bState) guitype)
                                                    return $ Left ()
                                                  Nothing -> do
                                                    rcv <- receiveGS channel
                                                    case rcv of
                                                      Just guimsg -> do
                                                        if (gmSignal guimsg) == OnChange then do
                                                          let bState = svalExtractor (gmValue guimsg)
                                                          return $ Right bState
                                                          else return $ Left ()
                                                      Nothing -> do
                                                        return $ Left () 
                    )
                                         
  return (wire, gsMapNew)  

-- | Basic wire for CheckBox GUI element functionality
checkBoxW :: String -- ^ Element Id
             -> Map String GSChannel -- ^ Channel Map (Internal)
             -> IO (GUIWire (Maybe Bool) Bool, Map String GSChannel) -- ^ resulting Wire
checkBoxW elid gsMap = valueWireGen elid gsMap SVBool (\svval -> let (SVBool bstate) = svval in bstate) CheckBox

-- | Basic wire for RadioButton GUI element functionality
radioButtonW :: String -- ^ Element Id
             -> Map String GSChannel -- ^ Channel Map (Internal)
             -> IO (GUIWire (Maybe Bool) Bool, Map String GSChannel) -- ^ resulting Wire
radioButtonW elid gsMap = valueWireGen elid gsMap SVBool (\svval -> let (SVBool bstate) = svval in bstate) RadioButton

-- | Basic wire for TextBox GUI element functionality
textBoxW :: String -- ^ Element Id
             -> Map String GSChannel -- ^ Channel Map (Internal)
             -> IO (GUIWire (Maybe String) String, Map String GSChannel) -- ^ resulting Wire
textBoxW elid gsMap = valueWireGen elid gsMap SVString (\svval -> let (SVString bstate) = svval in bstate) TextBox

-- | Basic wire for MultiSelect GUI element functionality
multiSelectW :: String -- ^ Element Id
             -> Map String GSChannel -- ^ Channel Map (Internal)
             -> IO (GUIWire (Maybe [String]) [String], Map String GSChannel) -- ^ resulting Wire
multiSelectW elid gsMap = valueWireGen elid gsMap SVStringList (\svval -> let (SVStringList bstate) = svval in bstate) MultiSelect


guiWireGen :: String -> Map String GSChannel -> (String -> GSChannel -> IO (GUIWire a b)) -> IO (GUIWire a b, Map String GSChannel)
guiWireGen elid gsMap wireIn = do
  chan <- createChannel
  let gsMapNew = Data.Map.insert elid chan gsMap
  wireOut <- wireIn elid chan
  return (wireOut, gsMapNew)  

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
  
-- | Basic wire for NumberTextBox GUI element functionality
numberTextBoxW :: String -- ^ Element Id
             -> Map String GSChannel -- ^ Channel Map (Internal)
             -> IO (GUIWire (Maybe Double) Double, Map String GSChannel) -- ^ resulting Wire
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


-- | Basic wire for HTML GUI element functionality (output element with dynamic HTML)
htmlW :: String -- ^ Element Id
             -> Map String GSChannel -- ^ Channel Map (Internal)
             -> IO (GUIWire (Maybe String) String, Map String GSChannel) -- ^ resulting Wire
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
  
-- | Basic wire for Button GUI element functionality (output element with dynamic HTML)
buttonW :: String -- ^ Element Id
             -> Map String GSChannel -- ^ Channel Map (Internal)
             -> IO (GUIWire a a, Map String GSChannel) -- ^ resulting Wire
buttonW elid gsMap = guiWireGen elid gsMap buttonW'

-----------------------------------------------
-- functions to extend basic wire functionality
-----------------------------------------------

{- $advancedwire

to be done

-}

