{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, ExistentialQuantification, ScopedTypeVariables #-}
{- | Widgets is an internal implementation module of "HWebUI". "HWebUI" is providing FRP-based GUI functionality for Haskell by utilizing the Web-Browser. See module "HWebUI" for main documentation. 
-}
{-# OPTIONS_HADDOCK hide #-}

module Widgets (
  -- * Creating the GUI Functionality with FRP
  
  -- ** How HWebUI wires (based on netwire) work
  -- $wiremechanism
  
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
  Button,
  TextBox,
  RadioButton,
  MultiSelect,
  HtmlText
  
  -- ** Widget Implementation
  -- $widgets

  ) where

import Yesod
import Text.Julius (rawJS)
import Prelude hiding ((.), id)
import Data.List (intersperse)

import Properties
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


data HtmlText = HtmlText ()
instance HasStyle HtmlText

-- | Yesod widget for the PlainHtml GUI element (an element which is used for dynamic HTML output
wHtml :: String -- ^ Element Id
         -> [Property HtmlText] -- ^ additional parameters
         -> Widget -- ^ resulting Yesod widget
wHtml wid paralist = do
  toWidget [hamlet|
           <div id="#{wid}" style=#{parasToJS paralist}>
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
>                                badd <- hold True addB -< Nothing
>                                     bsub <- hold False subB -< Nothing
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

{- $widgets

The functionality of a widget is a complex combination of different implementation technologies, which all interwork. In the Browser a javascript Dojokit widget is sending and receiving Javascript events. Also in the Browser additional Javascript code transform those events and commands in a format which can be transferred to the backend. Then those events are transferred with the above described messaging mechanism towards the Haskell backend. Finally they are processed and steared towards the corresponding Haskell FRP widget implementation. Below a short description about how to implement a widget might shed some light on the interworking mechanisms.

/How to implement a widget:/

To implement a widget the following steps needs to be done:

- Think about the type of the widget. The most widgets which carry a value of type \"a\" understand commands of the format \"Maybe a\" to set the value of the widget and send changed values of simply type \"a\". In case of more complex widgets additional information might be included in this value, for example the MultiSelect widget encodes also the selection status in the value.

- Create a Yesod widget for the widget in the "Widgets" module. The Yesod widgets carry the Javascript code for the widget which sends messages upon a change in the widget. For this a \"onChange\" function needs to be coded in the Javascript of the Yesod widget. This function should send the changed value to the Haskell backend by calling the \"sendMessage\" function. In addition the Browser based widget needs to be updated in case a command message to do so is sent from the Haskell backend. To make this happen the 'wInitGUI' function needs to be updated to include widget specific behaviour for the new widget in the \"onMessage\" function.

- Create a netwire wire based widget in the "Wires" module. You might be able to use the generic implementation of the 'valueWireGen' function in case the widget is of a value type as described above. Sometimes the complete wire needs to be coded independently. The functionality needed for a wire representing a widget in FRP code is to receive messages from the channel and to update the state or to send messages over hte channel in case the value is updated by FRP mechanisms. Look at 'valueWireGen' to get a better understanding of that.

- Implement a test case to test your new widget.



-}
