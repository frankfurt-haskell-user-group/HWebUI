{- | HWebUI is providing FRP-based GUI functionality for Haskell by utilizing the Web-Browser. It is build on top of Yesod for the Web technologies and on netwire for the FRP interface. The status is \"early prototype\". The implementation uses a Javascript library (Dojo toolkit) for providing typical widgets, HTML for the layout of the widgets. With Javascript and websockets events are transferred between the Web and the Haskell world. This happens behind the scenes. The Haskell programmer is using a FRP based interface. See also: <http://www.github.com/frankfurt-haskell-user-group/HWebUI>.
-}
module HWebUI(
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
  Attribute (..),
  Property (..),
  style,
  height,
  width,
  label,
  name,
  value,
  checked,

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
  runHWebUI,
  
  -- ** Miscellaneous
  GSChannel,
  GUIWire,
  ChannelStateGUIWire,
  -- * Implementation Details
  
  -- ** Communication between Javascript/HTML Widget world and FRP Wire world
  -- $messaging
  
  -- ** The background server process (built on top of Yesod)
  -- $backgroundserver
  
  -- ** Widget Implementation
  -- $widgets
  
  
  
  
  ) where

import Yesod
import System.IO (hFlush, stdout)
import Prelude hiding ((.), id)
import Control.Wire
import Data.Map

import Messaging
import Widgets
import Server
import Wires
import Control.Monad.State
import Data.Map

-- | this function runs the HWebUI web server (the Yesod server), runs the netwire loop and wait for termination
runHWebUI port guiLayout channelStateWire = do
    -- create netwire gui elements
    let gsmap = Data.Map.fromList [] :: Map String GSChannel
    (theWire,gsmap') <- runStateT channelStateWire gsmap    
    runHWebUIServer port gsmap' guiLayout
    loopHWebUIWire theWire
    waitForHWebUIServer

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

{- $rungui
  
To run a GUI built with HWebUI, the background webserver needs to be started, which handels the websocket communication with the webbrowser. In addition a loop needs to be called, to run the netwire FRP based GUI api. All this is done by calling the runHWebUI function as shown below.

>    -- run the webserver, the netwire loop and wait for termination   
>    runHWebUI port gsmap guiLayout theWire
-}


{- $messaging

The communication between the Browser and the Haskell program running the HWebUI GUI is implemented over one websocket. The basic websocket code is realised in the "Server" module. Messages are passed back and forth through this one websocket and are stored in between in data structures called Channels. Each GUI element owns it's own channel and there is a channel map, which is indexed by the GUI Element ID. The basic code for the Channels can be found in the "Messaging" module. In this module also the data structure for the 'GUIMessage' data type is defined, which includes  for each message a 'GUISignal', a 'GUIValue', a 'GUIElementType' and a 'GUIElementId'. All those data types can be send over the Websocket as JSON data and are used only for internal purpose of this communication channel. The "Messaging" module exports functions to create Channels for messages and send and receive them. 

Those functions are used in the "Server" module, to receive messages from the websocket and push them to the channels of the single GUI elements. Additional functions are used in the FRP wire implementations in the "Wires" module to pop the messages from the channels during a receive action. The sending of messages towards the GUI elements works the other way around. Messages are pushed from the FRP code towards the channels and in the "Wires" module and in the "Server" module those messages are pushed through the websocket towards the Browswer.

In the Browser Messages are sent and received over JSON sending and receiving. The code for that is located in the "Widgets" module as Javascript implementation. The 'wInitGUI' widget comes with the Javascript code to reveive JSON messages from the Haskell based backend server and distribute it towards the single GUI elemens. The single widgets contain Javascript code to send JSON messages towards the Haskell based backend server.

-}


{- $advancedwire

to be done

-}

{- $backgroundserver

To run a web based GUI, we need a background Webserver. In the case of HWebUI this purpose is achieved by leveraging the Yesod webserver. In addition to serving the HTML pages, which build the static framework for the GUI, the Yesod server also provides the mechanism to implement a websockets based communication between the Browser and the Haskell process running the FRP based GUI. All this functionality is implemented in a pilot implementation mode in the "Server" module. According to this explanation functionality in the "Server" module is grouped into two parts, one is about providing the HTML/Javascript based landing page for the GUI and one is to accept a connection for a websocket communication and handle this specific communication channel.

The function which runs the background server is called 'runWebserver' and it includes both parts, it defines a Yesod settings data structure, which includes the websocket interception option. It also creates the base type of the Yesod server which includes the layout of the landing page. This is needed, since we want flexible layouts for the landing page, not linked to the function running the server. Then it starts the the Yesod server.

The functions which implement the websocket communication are the 'socketAcceptFunction', 'readSocketLoop' and the 'socketHandlingFunction', which spawns the write loop for writing towards the socket and then calls the 'readSocketLoop' function. The 'socketAcceptFunction' is called given as the anchor function in the websocket configuration inside the 'runWebserver' function.

The basic Yesod webserver is implemented the usual way, two annotation may be made: there is a specific base layout defined to include the Dojokit "claro" style in the body element of the HTML page. Also the layout of the one and only render page is not hard coded but given as a paramter to the configuration, so that it can be implemented later, as can be seen in the examples.

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
