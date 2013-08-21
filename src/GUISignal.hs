{- | GUISignal is an internal implementation module of "HWebUI". "HWebUI" is providing FRP-based GUI functionality for Haskell by utilizing the Web-Browser. See module "HWebUI" for main documentation. 
-}
module GUISignal (
  GUISignal (..)
  ) where

import Yesod
import qualified Data.Aeson                     as J
import System.IO (hFlush, stdout)
import Control.Monad
import Prelude hiding ((.), id)
import Data.Text

import GUICommand
import GUIEvent

-- | A GUI Signal is either an event coming from the GUI element upon user interaction or a command, send to the element
data GUISignal = GUIEvent GUIEvent 
               | GUICommand GUICommand
               deriving (Show, Read, Eq)

instance J.FromJSON GUISignal where
  parseJSON (String sig) = return (read (unpack sig))
  parseJSON _ = mzero
  
instance J.ToJSON GUISignal where
  toJSON sig = String (pack (show sig))
