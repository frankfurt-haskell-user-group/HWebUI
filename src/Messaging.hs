{- | Messaging is an internal implementation module of "HWebUI". "HWebUI" is providing FRP-based GUI functionality for Haskell by utilizing the Web-Browser. See module "HWebUI" for main documentation. 
-}
module Messaging (
  
  -- * Data Types, used for messages and message buffers
  GUIElementId,
  GUIElementType (..),
  GUIMessage (..),
  GSChannel,
  
  -- * Functions to handle messages and message buffers
  createChannel,
  receiveGMReadChannel,  
  receiveGMWriteChannel,
  sendGMWriteChannel,
  sendGMReadChannel
 
  ) where

import Yesod
import qualified Data.Aeson                     as J
import System.IO (hFlush, stdout)
import Control.Applicative
import Control.Monad
import Control.Concurrent
import Prelude hiding ((.), id)
import Data.Text

import GUIValue
import GUISignal


-- | String identifying a GUI element
type GUIElementId = String

-- | The type of GUI Element
data GUIElementType = Button | CheckBox | TextBox | MultiSelect | NumberTextBox | RadioButton | Html deriving (Show, Read)
instance J.FromJSON GUIElementType where
  parseJSON (String sig) = return (read (unpack sig))
  parseJSON _ = mzero
instance J.ToJSON GUIElementType where
  toJSON sig = String (pack (show sig))
 

-- | A GUI message, used to communicate between Browser and Haskell server
data GUIMessage = GUIMessage {
  gmId :: GUIElementId, -- ^ the unique id of the GUI element
  gmSignal :: GUISignal, -- ^ the signal sent by this message, either a command towards the GUI element or an Event coming from it
  gmValue :: GUIValue, -- ^ the value associated with the command or event
  gmType :: GUIElementType -- ^ the type of the GUI element
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
  
-- | A two sided message buffer structure  
data GSChannel = GSChannel {
  sendToGUI :: MVar [GUIMessage],
  receiveFromGUI :: MVar [GUIMessage],
  valueSetFlag :: MVar (Bool, GUIValue)
  }
                 

-- internal impelementation 
  
_readChannel :: MVar [GUIMessage] -> IO (Maybe GUIMessage)
_readChannel chan = do
  gsList <- takeMVar chan
  let (gsListNew, rval) = case gsList of
        (gs:gss) -> (gss, Just gs)
        [] -> ([], Nothing)
  putMVar chan gsListNew
  return rval
  
_writeChannel :: MVar [GUIMessage] -> GUIMessage -> IO ()
_writeChannel chan msg = do
  gsList <- takeMVar chan
  putMVar chan (gsList Prelude.++ [msg])
  return ()
  
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

-- | creates a message buffer (Channel) with two way communication
createChannel :: IO GSChannel
createChannel = do
  sendChannel <- newMVar ([]::[GUIMessage])
  receiveChannel <- newMVar ([]::[GUIMessage])
  valueSetFlag <- newMVar (False, SVNone)
  return $ GSChannel sendChannel receiveChannel valueSetFlag
  
-- | read the message buffer (Channel) during reception of a GUI message from the Browser GUI element
receiveGMReadChannel :: GSChannel -> IO (Maybe GUIMessage)
receiveGMReadChannel gsc = do
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
             

-- | write the message buffer (Channel) during sending of a GUI message towards the Browser GUI element
sendGMWriteChannel :: GSChannel -> GUIMessage -> IO ()
sendGMWriteChannel gsc msg = do
--                 print $ "msg set ignore: " ++ (show msg)
--                 hFlush stdout                 
                 _setValueSetFlag gsc True (gmValue msg)
                 _writeChannel (sendToGUI gsc) msg
                 return ()

-- | read the message buffer (Channel) during sending of a GUI message towards the Browser GUI element, this is used by the "Server" module to pass all messages over the websocket to the Browser.
sendGMReadChannel :: GSChannel -> IO (Maybe GUIMessage)
sendGMReadChannel gsc = _readChannel (sendToGUI gsc)

-- | write the message buffer (Channel) during reception of a GUI message from the Browser GUI element, this is used by the "Server" module to provide the received data towards the Channels and the FRP GUI elements.
receiveGMWriteChannel :: GSChannel -> GUIMessage -> IO ()
receiveGMWriteChannel gsc msg = _writeChannel (receiveFromGUI gsc) msg

