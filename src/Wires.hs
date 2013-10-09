{- | Wires is an internal implementation module of "HWebUI". "HWebUI" is providing FRP-based GUI functionality for Haskell by utilizing the Web-Browser. See module "HWebUI" for main documentation. 
-}
{-# OPTIONS_HADDOCK hide #-}

module Wires (
  
  -- ** The netwire wires (basic functionality)
  buttonW,
  checkBoxW,
  htmlW,
  multiSelectW,
  numberTextBoxW,
  radioButtonW,
  textBoxW,
  textareaW,

  -- ** functions to extend basic wires with additional functionality
  -- $advancedwire
  
  
  loopHWebUIWire,
  
  GUIWire,
  GSChannelMap (..)
  
  ) where

import Yesod                     (liftIO)
import Network.Wai.Handler.Warp  (defaultSettings, runSettings, Settings)
import System.IO                 (hFlush, IO, stdout)
import Control.Applicative       ((<$>))
import Control.Monad             (fmap, return)
import Text.Julius               (rawJS)
import Control.Concurrent        (threadDelay)
import Control.Exception         (mask, SomeException, try)
import Control.Monad.State       (fmap, liftIO, modify, return, StateT)

import Control.Wire              ((<$>), clockSession, delay, mkFixM, mkStateM, returnA, SomeException, stepSession, Wire, Session)
import Prelude                   (($), (.), (==), Bool, Double, Either (Left, Right), filter, fmap, fromInteger, fst, IO, Maybe (Just, Nothing), return, snd, String, zip)
import Data.Map                  (insert, Map)
import Data.Vector               (fromList, toList)

import GUIValue                  (GUIValue (SVBool, SVDouble, SVList, SVString))
import GUIEvent                  (GUIEvent (OnChange))
import GUICommand                (GUICommand (SetValue))
import GUISignal                 (GUISignal (GUICommand, GUIEvent))
import Messaging                 (createChannel, gmSignal, gmValue, GSChannel, GUIElementType (CheckBox, Html, MultiSelect, NumberTextBox, RadioButton, TextBox, Textarea), GUIMessage (GUIMessage), receiveGMReadChannel, sendGMWriteChannel)


-- Netwire Types
--
type GUIWire a b = Wire () IO a b
type GSChannelMap = Map String GSChannel

addChannel  :: String -> GSChannelMap -> IO (GSChannel, GSChannelMap) 
addChannel elid cmap = do
        channel <- createChannel         
        let newMap = Data.Map.insert elid channel cmap
        return (channel, newMap)

-- generic function to create a value wire, a value wire has type "GUIWire (Maybe a) a"

valueWireGen :: String -- ^ Element Id                 
                -> (a -> GUIValue) -- ^ svalue creation function
                -> (GUIValue -> b) -- ^ svalue extraction function
                -> GUIElementType -- ^ type of GUI Element
                -> GSChannelMap -- ^ Channels 
                -> IO (GUIWire (Maybe a) b, GSChannelMap)
                                
valueWireGen elid  svalCreator svalExtractor guitype cmap = do  
  (channel, nmap) <- addChannel elid cmap
  let wire = mkFixM $ \t inVal -> case inVal of
              Just bState -> do
                 sendGMWriteChannel channel (GUIMessage elid (GUICommand SetValue) (svalCreator bState) guitype)
                 return $ Left ()
              Nothing -> do
                 rcv <- receiveGMReadChannel channel
                 case rcv of
                      Just guimsg ->
                                 if gmSignal guimsg == GUIEvent OnChange then do
                                    let bState = svalExtractor (gmValue guimsg)
                                    return $ Right bState
                                 else return $ Left ()
                      Nothing -> return $ Left () 
  return (wire, nmap)                                                         
                                                     

-- | Basic wire for CheckBox GUI element functionality
checkBoxW :: String -- ^ Element Id
             -> GSChannelMap -- ^ Channels
             -> IO (GUIWire (Maybe Bool) Bool, GSChannelMap)
checkBoxW elid cmap = valueWireGen elid SVBool 
        (\svval -> let (SVBool bstate) = svval in bstate) CheckBox cmap

-- | Basic wire for RadioButton GUI element functionality
radioButtonW :: String -- ^ Element Id
                -> GSChannelMap -- ^ Channels
                -> IO (GUIWire (Maybe Bool) Bool, GSChannelMap)
radioButtonW elid cmap = valueWireGen elid SVBool (\svval -> let (SVBool bstate) = svval in bstate) RadioButton cmap

-- | Basic wire for TextBox GUI element functionality
textBoxW :: String -- ^ Element Id
             -> GSChannelMap -- ^ Channels
             -> IO (GUIWire (Maybe String) String, GSChannelMap)
textBoxW elid cmap = valueWireGen elid SVString (\svval -> let (SVString bstate) = svval in bstate) TextBox cmap

-- | Baisc wire for Textarea GUI element functionality
textareaW :: String -- ^ ElementId
               -> GSChannelMap -- ^ Channels
               -> IO (GUIWire (Maybe String) String, GSChannelMap)
textareaW elid cmap = valueWireGen elid SVString (\svval -> let (SVString bstate) = svval in bstate) Textarea cmap

-- | Basic wire for MultiSelect GUI element functionality 
_multiSelectW :: String -- ^ Element Id
             -> GSChannelMap -- ^ Channels
             -> IO (GUIWire (Maybe [(String, Bool)]) [(String, Bool)], GSChannelMap)
_multiSelectW elid cmap = valueWireGen elid  f1 f2 MultiSelect cmap where 
  f1 svlist = SVList $ fmap (\val -> SVList [SVString (fst val), SVBool (snd val)]) svlist      
  f2 (SVList svlist) = split <$> svlist
      where split (SVList [SVString aval, SVBool bval]) = (aval,bval)
       
multiSelectW :: String -- ^ Element Id
                -> GSChannelMap -- ^ Channels
                -> IO (GUIWire (Maybe [(String, Bool, a)]) [a], GSChannelMap)
multiSelectW elid cmap = do
  (w1, nmap) <- _multiSelectW elid cmap
  let w2 = proc inval -> do
        rec 
          thingies <- delay [] -< thingies'   -- thingies is the list of thingies of type a, which will get filtered by selections
    
          -- case there is a valid inval, thingies list changes
          let thingies' = case inval of        
                Just msIn -> fmap (\(s, b, a) -> a) msIn
                Nothing -> thingies
        
        -- rec ends here, following is without rec
  
        let inval' = case inval of
              Just msIn -> Just $ fmap (\(s, b, a) -> (s, b)) msIn
              Nothing -> Nothing
        

        -- run original multiSelectW, msOutVal is the selection
        msOut <- w1 -< inval'
        returnA -< fmap snd $ Prelude.filter (\((s, sel) , th) -> sel) (Prelude.zip msOut thingies')

  return (w2, nmap)

    

guiWireGen ::   String 
                -> (String -> GSChannel -> IO (GUIWire a b))
                -> GSChannelMap -- ^ Channels
                -> IO (GUIWire a b, GSChannelMap)
guiWireGen elid wireIn cmap = do
  (chan, nmap) <- addChannel elid cmap
  wireOut <- wireIn elid chan
  return (wireOut, nmap)

numberTextBoxW' :: String -> GSChannel -> IO (GUIWire (Maybe Double) Double)
numberTextBoxW' boxid channel = do
  let wire =  mkStateM 0 (\t (trigger, s) -> 
                                 case trigger of
                                   Just bState -> do
                                     sendGMWriteChannel channel (GUIMessage boxid (GUICommand SetValue) (SVDouble bState) NumberTextBox)
                                     return (Right bState, bState)
                                   Nothing -> do
                                     rcv <- receiveGMReadChannel channel
                                     case rcv of
                                       Just guimsg -> 
                                         if gmSignal guimsg == GUIEvent OnChange then do
                                           let (SVDouble bState) = gmValue guimsg
                                           return (Right bState, bState)
                                           else
                                             return (Right s, s)
                                       Nothing ->
                                         return (Right s, s)   )
  return wire                                                         
  
-- | Basic wire for NumberTextBox GUI element functionality
numberTextBoxW :: String -- ^ Element Id
                  -> GSChannelMap -- ^ Channels

                  -> IO (GUIWire (Maybe Double) Double, GSChannelMap)
numberTextBoxW elid cmap = guiWireGen elid numberTextBoxW' cmap

htmlW' :: String -> GSChannel -> IO (GUIWire (Maybe String) String)
htmlW' boxid channel = do
  let wire =  mkStateM "" (\t (trigger, s) -> 
                                 case trigger of
                                   Just bState -> do
                                     sendGMWriteChannel channel (GUIMessage boxid (GUICommand SetValue) (SVString bState) Html)
                                     return (Right bState, bState)
                                   Nothing -> 
                                     return (Right s, s) )
  return wire                                                         


-- | Basic wire for HTML GUI element functionality (output element with dynamic HTML)
htmlW :: String -- ^ Element Id
         -> GSChannelMap -- ^ Channels
         -> IO (GUIWire (Maybe String) String, GSChannelMap)
htmlW elid cmap = guiWireGen elid htmlW' cmap

-- button wire
--

buttonW' :: String -> GSChannel -> IO (GUIWire a a)
buttonW' boxid channel = do
  let wire =  mkFixM (\t var -> do
                                     rcv <- receiveGMReadChannel channel
                                     case rcv of
                                       Just gs -> return $ Right var
                                       _ -> return $ Left () )
  return wire                                                         
  
-- | Basic wire for Button GUI element functionality (output element with dynamic HTML)
buttonW :: String -- ^ Element Id
           -> GSChannelMap -- ^ Channels
           -> IO (GUIWire a a, GSChannelMap)
buttonW elid cmap = guiWireGen elid buttonW' cmap

_loopAWire :: Wire e IO () b -> Session IO -> IO ()
_loopAWire wire session = do
    (r, wire',  session') <- stepSession wire session ()
    threadDelay 10000
    _loopAWire wire' session'
    return ()

loopHWebUIWire :: Wire e IO () b -> IO ()
loopHWebUIWire theWire = _loopAWire theWire clockSession


{- $advancedwire

to be done

-}

