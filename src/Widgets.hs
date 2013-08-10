{-# LANGUAGE TemplateHaskell, QuasiQuotes, OverloadedStrings, TypeFamilies, MultiParamTypeClasses, Arrows #-}


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

import Server

-- | Yesod widget to initialize needed Javascript functionality in the HTML code of the GUI. Provides Dojokit inclusion and communication with Haskell Yesod server over websockets.
wInitGUI :: Int -- ^ port used to communicate with Haskell server
            -> HWebUIWidget -- ^ resulting Yesod widget
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

-- | Yesod widget for the Button GUI Element
wButton :: String -- ^ Element Id 
           -> String -- ^ Label of the Button 
           -> HWebUIWidget -- ^ resulting Yesod Widget
wButton wid label = do
  toWidget [julius|
            require(["dojo/ready", "dijit/form/Button", "dojo/dom", "dojo/json"], function(ready, Button, dom, JSON){
                       ready(function(){
                                // Create a button programmatically:
                                var myButton = new Button({
                                                             label: "#{rawJS label}",
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

-- | Yesod widget for the CheckBox GUI Element
wCheckBox :: String -- ^ Element Id
             -> HWebUIWidget -- ^ resulting Yesod Widget
wCheckBox wid = do
  toWidget [julius|
            require(["dojo/ready", "dijit/form/CheckBox", "dojo/dom", "dojo/json"], function(ready, CheckBox, dom, JSON){
                       ready(function(){
                                // Create a checkbox programmatically:
                                var myCheckBox = new CheckBox({
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

-- | Yesod widget for the TextBox GUI element
wTextBox :: String -- ^ Element Id
            -> HWebUIWidget -- ^ resulting Yesod widget
wTextBox wid = do
  toWidget [julius|
            require(["dojo/ready", "dijit/form/TextBox", "dojo/dom", "dojo/json"], function(ready, TextBox, dom, JSON){
                       ready(function(){
                                // Create a text box programmatically:
                                var myTextBox = new TextBox({
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

-- | Yesod widget for the MultiSelect GUI element
wMultiSelect :: String -- ^ Element Id
            -> Int -- ^ Width of widget
            -> HWebUIWidget -- ^ resulting Yesod widget
wMultiSelect wid width = do
  toWidget [julius|
            require(["dojo/ready", "dijit/form/MultiSelect", "dojo/dom", "dojo/json"], function(ready, MultiSelect, dom, JSON){
                       ready(function(){
                                // Create a text box programmatically:
                                var myMultiSelect = new MultiSelect({
                                                             onChange: function(val){
                                                                childs = this.containerNode.childNodes;
                                                                var valArr = new Array(childs.length);
                                                                for(var i=0; i<childs.length; i++) { valArr[i] = [childs[i].innerHTML, false]; }
                                                                for(var i=0; i<val.length; i++) { ind = parseInt(val[i], 10); valArr[ind][1] = true; }
                                                                sendMessage("#{rawJS wid}", "GUIEvent OnChange", valArr, "MultiSelect")
                                                             },
                                                             style: { "width" : "#{rawJS (show width)}px" },
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
                  -> HWebUIWidget -- ^ resulting Yesod widget
wNumberTextBox wid = do
  toWidget [julius|
            require(["dojo/ready", "dijit/form/NumberTextBox", "dojo/dom", "dojo/json"], function(ready, NumberTextBox, dom, JSON){
                       ready(function(){
                                // Create a number text box programmatically:
                                var myTextBox = new NumberTextBox({
                                                             onChange: function(val){
                                                               sendMessage("#{rawJS wid}", "GUIEvent OnChange", val, "NumberTextBox");
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
         -> HWebUIWidget -- ^ resulting Yesod widget
wHtml wid = do
  toWidget [hamlet|
           <div id="#{wid}">
                   |]

-- | Yesod widget for the RadioButtion GUI element
wRadioButton :: String -- ^ Element Id
                -> String -- ^ Name
                -> String -- ^ Value
                -> Bool -- ^ Checked
                -> HWebUIWidget -- ^ resulting Yesod widget
wRadioButton wid name value checked = do
  toWidget [julius|
            require(["dojo/ready", "dijit/form/RadioButton", "dojo/dom", "dojo/json"], function(ready, RadioButton, dom, JSON){
                       ready(function(){
                                var myRadioButton = new RadioButton({
                                                             onChange: function(val){
                                                               sendMessage("#{rawJS wid}", "GUIEvent OnChange", val, "RadioButton");
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

