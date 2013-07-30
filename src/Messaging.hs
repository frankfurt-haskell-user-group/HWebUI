{-# LANGUAGE TemplateHaskell, QuasiQuotes, OverloadedStrings, TypeFamilies, MultiParamTypeClasses, Arrows #-}


module Messaging (
  
  GUIElementId,
  GUIElementType (..),
  GUIMessage (..),
  
  GSChannel,
  
  createChannel,
  receiveGS,
  sendGS,
  receiveGS',
  sendGS'
  
 
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

-------------------------------
-- communication infrastructure
-------------------------------

-- there are two communication channels, one from the web javascript to the backends yesod server, another one 
-- from the backend yesod server to the GUI FRP frontend process. For both different types of messages are used.
-- In the first case, messages are translated to JSON for sending over websockets.


-- GuiElementId is the String identifying a GUI element
type GUIElementId = String

-- GUIElementType is the type of GUI Element
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
  gmValue :: GUIValue,
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
  valueSetFlag :: MVar (Bool, GUIValue)
  }
                 
_checkValueSetFlag :: GSChannel -> GUIValue -> IO Bool
_checkValueSetFlag gsc valin = do
  (flag, val) <- takeMVar (valueSetFlag gsc)
  putMVar (valueSetFlag gsc) (flag, val)
--  print $ (show valin) ++ " " ++ (show val) ++ " " ++ (show (valin == val))
  return (flag && (valin == val))
  
_setValueSetFlag :: GSChannel -> Bool -> GUIValue -> IO ()
_setValueSetFlag gsc flag val = do
  (flag', val') <- takeMVar (valueSetFlag gsc)
  putMVar (valueSetFlag gsc) (flag, val)
  return ()
  
-- external interface for remaining code

createChannel :: IO GSChannel
createChannel = do
  sendChannel <- newMVar ([]::[GUIMessage])
  receiveChannel <- newMVar ([]::[GUIMessage])
  valueSetFlag <- newMVar (False, SVNone)
  return $ GSChannel sendChannel receiveChannel valueSetFlag
  
-- two types of message routines, one used from Server side and one used from gui side

-- used in the netwire server code, to receive and send to and from gui
-- contains logic, to prevent a SetMessage to trigger a changed message
  
receiveGS :: GSChannel -> IO (Maybe GUIMessage)
receiveGS gsc = do
  msg <- _readChannel (receiveFromGUI gsc)
  case msg of
    Just gmsg -> do
      flag <- _checkValueSetFlag gsc (gmValue gmsg)
      if flag then do
--        print $ "msg ignored: " ++ (show msg)
--        hFlush stdout
        _setValueSetFlag gsc False SVNone
        return Nothing
        else do
          return $ Just gmsg
    Nothing -> do
      return Nothing
             

sendGS :: GSChannel -> GUIMessage -> IO ()
sendGS gsc msg = do
--                 print $ "msg set ignore: " ++ (show msg)
--                 hFlush stdout                 
                 _setValueSetFlag gsc True (gmValue msg)
                 _writeChannel (sendToGUI gsc) msg
                 return ()

-- used in the handlers to send over the wire towards the gui with JSON format
-- plainly sends and receives, no additional logic

receiveGS' :: GSChannel -> IO (Maybe GUIMessage)
receiveGS' gsc = _readChannel (sendToGUI gsc)

sendGS' :: GSChannel -> GUIMessage -> IO ()
sendGS' gsc msg = _writeChannel (receiveFromGUI gsc) msg

