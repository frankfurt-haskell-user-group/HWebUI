{-# LANGUAGE TemplateHaskell, QuasiQuotes, OverloadedStrings, TypeFamilies, MultiParamTypeClasses, Arrows #-}


{- | HWebUI is providing FRP-based GUI functionality for Haskell by utilizing the Web-Browser. It is build on top of Yesod for the Web technologies and on netwire for the FRP interface. The status is \"early prototype\". The implementation uses a Javascript library (Dojo toolkit) for providing typical widgets, HTML for the layout of the widgets. With Javascript and websockets events are transferred between the Web and the Haskell world. This happens behind the scenes. The Haskell programmer is using a FRP based interface. See also: <http://www.github.com/althainz/HWebUI>.
-}
module Wires (
  
  -- ** The netwire wires (basic functionality)
  buttonW,
  checkBoxW,
  htmlW,
  multiSelectW,
  numberTextBoxW,
  radioButtonW,
  textBoxW,
  
  GUIWire
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
import Widgets
import Server

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
                -> (a -> GUIValue) -- ^ svalue creation function
                -> (GUIValue -> b) -- ^ svalue extraction function
                -> GUIElementType -- ^ type of GUI Element
                -> IO (GUIWire (Maybe a) b , Map String GSChannel) -- ^ (wire, new Channel Map)
                
valueWireGen elid gsMap svalCreator svalExtractor guitype = do
  
  channel <- createChannel
  let gsMapNew = Data.Map.insert elid channel gsMap
  
  let wire = mkFixM  (\t inVal -> do
                                                case inVal of
                                                  Just bState -> do
                                                    sendGS channel (GUIMessage elid (GUICommand SetValue) (svalCreator bState) guitype)
                                                    return $ Left ()
                                                  Nothing -> do
                                                    rcv <- receiveGS channel
                                                    case rcv of
                                                      Just guimsg -> do
                                                        if (gmSignal guimsg) == (GUIEvent OnChange) then do
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
             -> IO (GUIWire (Maybe [String]) [Int], Map String GSChannel) -- ^ resulting Wire
multiSelectW elid gsMap = valueWireGen elid gsMap (\list -> SVList $ fmap SVString list) (\svlist -> let  
                                                                                             (SVList rval) = svlist 
                                                                                             rval' = fmap (\svval -> let (SVInt intval) = svval in intval) rval 
                                                                                             in rval')  MultiSelect


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
                                     sendGS channel (GUIMessage boxid (GUICommand SetValue) (SVDouble bState) NumberTextBox)
                                     return (Right bState, bState)
                                   Nothing -> do
                                     rcv <- receiveGS channel
                                     case rcv of
                                       Just guimsg -> do
                                         if (gmSignal guimsg) == (GUIEvent OnChange) then do
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
                                     sendGS channel (GUIMessage boxid (GUICommand SetValue) (SVString bState) Html)
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

