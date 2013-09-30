{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, ExistentialQuantification, ScopedTypeVariables #-}
{- | Widgets is an internal implementation module of "HWebUI". "HWebUI" is providing FRP-based GUI functionality for Haskell by utilizing the Web-Browser. See module "HWebUI" for main documentation. 
-}
module Widgets (
  wInitGUI,
  wButton,
  wCheckBox,
  wHtml,
  wMultiSelect,
  wNumberTextBox,
  wRadioButton,
  wTextBox,
  
  Attribute (..),
  Property (..),
  style,
  height,
  width,
  label,
  name,
  value,
  checked
  
  
  ) where

import Yesod
import System.IO (hFlush, stdout)
import Text.Julius (rawJS, ToJavascript, toJavascript)
import Data.Text.Lazy.Builder (fromString, Builder)
import Prelude hiding ((.), id)
import Data.Text (pack) 
import Data.List (intersperse)

import Server

-- | Yesod widget to initialize needed Javascript functionality in the HTML code of the GUI. Provides Dojokit inclusion and communication with Haskell Yesod server over websockets.
wInitGUI :: Int -- ^ port used to communicate with Haskell server
            -> Widget -- ^ resulting Yesod widget
wInitGUI port = do
  let portStr = show port
  addScriptRemote("http://ajax.googleapis.com/ajax/libs/dojo/1.9.1/dojo/dojo.js")
  addStylesheetRemote("//ajax.googleapis.com/ajax/libs/dojo/1.9.1/dijit/themes/claro/claro.css")
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
                              if (message.gmSignal == "GUICommand SetValue")
                              {
                                elem =  registry.byId(message.gmId);

                                if (message.gmType == "MultiSelect") 
                                {
                                   var dsel = dom.byId(message.gmId);
                                   var childs = dsel.children;
                                   var lench = childs.length;

                                   var n = 0;
                                   for(var i in message.gmValue)
                                   {
                                       var opt;
                                       var val = message.gmValue[i][0];
                                       if (n >= lench) {
                                        opt = domConstruct.create('option');
                                        dsel.appendChild(opt);
                                       }
                                       else {
                                        opt = dsel.children[n];
                                       }
                                       opt.innerHTML = val;
                                       opt.value = n;
                                       n = n + 1;
                                   }
                                   for (i = n; i < lench; i++ )
                                   {
                                      dsel.removeChild(dsel.children[n]);
                                   }

                                   // set selection
                                   for (var i in message.gmValue)
                                   {
                                     dsel.children[i].selected = message.gmValue[i][1];
                                   }
                                } // MultiSelect
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
                              } // SetValue
                            }
                       }
                       theGuiSocket.onopen = function(evt) {
                       }
             });  

           |]


data  Attribute w a = Attr String (String -> a -> String)
data Property w = forall a. Attribute w a := a

propToJS :: Property w -> String
propToJS ( (:=) (Attr name toJS) value) = toJS name value

class HasLabel w where
  label :: Attribute w String
  label = Attr "label" stringPairToJS

class HasStyle w where
  style :: Attribute w String
  style = Attr "style" stringPairToJS

class HasConstraints w where
  constraints :: Attribute w JavaScript
  style = Attr "style" stringPairToJS

class HasName w where
  name :: Attribute w String
  name = Attr "name" stringPairToJS

class HasValue w where
  value :: Attribute w String
  value = Attr "value" stringPairToJS

class HasChecked w where
  checked :: Attribute w Bool
  checked = Attr "checked" boolPairToJS

class HasSize w where
  width :: Attribute w Int
  height :: Attribute w Int
  width = Attr "width" intPairToJS
  height = Attr "height" intPairToJS



intPairToJS :: String -> Int -> String
intPairToJS s i =  s ++ ": "  ++ (show i)
floatPairToJS :: String -> Float -> String
floatPairToJS s f = s ++ ": "  ++ (show f)
boolPairToJS :: String -> Bool -> String
boolPairToJS s b = s ++ ": " ++ (if b then "true" else "false")
stringPairToJS :: String -> String -> String
stringPairToJS s1 s2 = s1 ++ ": \"" ++ s2 ++ "\""
  
parasToJS :: [Property w] -> String
parasToJS pl = foldl (++) "" $ (\l -> if null l then l else l ++ [","])  $ intersperse "," $ map propToJS pl

data Button = Button ()
instance HasLabel Button
instance HasStyle Button
  
-- | Yesod widget for the Button GUI Elemhiding (intersperse)ent
wButton :: String -- ^ Element Id 
           -> [Property Button] -- ^ additional parameters
           -> Widget -- ^ resulting Yesod Widget
wButton wid paralist = do
  toWidget [julius|
            require(["dojo/ready", "dijit/form/Button", "dojo/dom", "dojo/json"], function(ready, Button, dom, JSON){
                       ready(function(){
                                // Create a button programmatically:
                                var myButton = new Button({
                                                             #{rawJS $ parasToJS paralist}
                                                             onClick: function(){
                                                               sendMessage("#{rawJS wid}", "GUIEvent OnChange", "Event", "Button");
                                                             }
                                                           }, '#{rawJS wid}');
                       });
             });  
           |]
  toWidget [hamlet|
           <button id="#{wid}" type="button">
            |]

data CheckBox = CheckBox ()
instance HasLabel CheckBox
instance HasChecked CheckBox
instance HasStyle CheckBox

-- | Yesod widget for the CheckBox GUI Element
wCheckBox :: String -- ^ Element Id
             -> [Property CheckBox] -- ^ additional parameters
             -> Widget -- ^ resulting Yesod Widget
wCheckBox wid paralist = do
  toWidget [julius|
            require(["dojo/ready", "dijit/form/CheckBox", "dojo/dom", "dojo/json"], function(ready, CheckBox, dom, JSON){
                       ready(function(){
                                // Create a checkbox programmatically:
                                var myCheckBox = new CheckBox({
                                                             #{rawJS $ parasToJS paralist}
                                                             onChange: function(val){
                                                               sendMessage("#{rawJS wid}", "GUIEvent OnChange", val, "CheckBox");
                                                             }
                                                           }, '#{rawJS wid}');
                       });
             });  
           |]
  toWidget [hamlet|
           <input id="#{wid}" type="checkbox">
            |]

data TextBox = TextBox ()
instance HasStyle TextBox
instance HasSize TextBox

-- | Yesod widget for the TextBox GUI element
wTextBox :: String -- ^ Element Id
            -> [Property TextBox] -- ^ additional parameters
            -> Widget -- ^ resulting Yesod widget
wTextBox wid paralist = do
  toWidget [julius|
            require(["dojo/ready", "dijit/form/TextBox", "dojo/dom", "dojo/json"], function(ready, TextBox, dom, JSON){
                       ready(function(){
                                // Create a text box programmatically:
                                var myTextBox = new TextBox({
                                                             #{rawJS $ parasToJS paralist}
                                                             onChange: function(val){
                                                               sendMessage("#{rawJS wid}", "GUIEvent OnChange", val, "TextBox");
                                                             },
                                                             intermediateChanges: true
                                                             }, '#{rawJS wid}');
                       });
             });  
           |]
  toWidget [hamlet|
           <input id="#{wid}" type="textbox">
            |]


data MultiSelect = MultiSelect ()
instance HasStyle MultiSelect
instance HasSize MultiSelect
  

-- | Yesod widget for the MultiSelect GUI element
wMultiSelect :: String -- ^ Element Id
             -> [Property MultiSelect] -- ^ additional parameters
             -> Widget -- ^ resulting Yesod widget
wMultiSelect wid paralist = do
  toWidget [julius|
            require(["dojo/ready", "dijit/form/MultiSelect", "dojo/dom", "dojo/json"], function(ready, MultiSelect, dom, JSON){
                       ready(function(){
                                // Create a text box programmatically:
                                var myMultiSelect = new MultiSelect({
                                                             #{rawJS $ parasToJS paralist}
                                                             onChange: function(val){
                                                                childs = this.containerNode.childNodes;
                                                                var valArr = new Array(childs.length);
                                                                for(var i=0; i<childs.length; i++) { valArr[i] = [childs[i].innerHTML, false]; }
                                                                for(var i=0; i<val.length; i++) { ind = parseInt(val[i], 10); valArr[ind][1] = true; }
                                                                sendMessage("#{rawJS wid}", "GUIEvent OnChange", valArr, "MultiSelect")
                                                             }
                                                             }, dom.byId('#{rawJS wid}'));
                       });
             });  
           |]
  toWidget [hamlet|
           <select id="#{wid}">
            |]

data NumberTextBox = NumberTextBox ()
instance HasStyle NumberTextBox
instance HasSize NumberTextBox
  

-- | Yesod widget for the NumberTextBox GUI element
wNumberTextBox :: String -- ^ Element Id
                  -> [Property NumberTextBox] -- ^ additional parameters
                  -> Widget -- ^ resulting Yesod widget
wNumberTextBox wid paralist = do
  toWidget [julius|
            require(["dojo/ready", "dijit/form/NumberTextBox", "dojo/dom", "dojo/json"], function(ready, NumberTextBox, dom, JSON){
                       ready(function(){
                                // Create a number text box programmatically:
                                var myTextBox = new NumberTextBox({
                                                             #{rawJS $ parasToJS paralist}
                                                             onChange: function(val){
                                                               sendMessage("#{rawJS wid}", "GUIEvent OnChange", val, "NumberTextBox");
                                                             },
                                                             intermediateChanges: true
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

data RadioButton = RadioButton ()
instance HasName RadioButton
instance HasValue RadioButton
instance HasChecked RadioButton
instance HasStyle RadioButton

-- | Yesod widget for the RadioButtion GUI element
wRadioButton :: String -- ^ Element Id
                -> [Property RadioButton] -- ^ additional parameters
                -> Widget -- ^ resulting Yesod widget
wRadioButton wid paralist = do
  toWidget [julius|
            require(["dojo/ready", "dijit/form/RadioButton", "dojo/dom", "dojo/json"], function(ready, RadioButton, dom, JSON){
                       ready(function(){
                                var myRadioButton = new RadioButton({
                                                             #{rawJS $ parasToJS paralist}
                                                             onChange: function(val){
                                                               sendMessage("#{rawJS wid}", "GUIEvent OnChange", val, "RadioButton");
                                                             }
                                                             }, '#{rawJS wid}');
                       });
             });  
           |]
  toWidget [hamlet|
           <input type="radio" id="#{wid}" >
            |]

