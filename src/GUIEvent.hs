{- | GUIEvent is an internal implementation module of "HWebUI". "HWebUI" is providing FRP-based GUI functionality for Haskell by utilizing the Web-Browser. See module "HWebUI" for main documentation. 
-}
module GUIEvent (
  GUIEvent (..)
  ) where

import Yesod
import qualified Data.Aeson                     as J
import System.IO (hFlush, stdout)
import Control.Monad
import Prelude hiding ((.), id)
import Data.Text

-- | The GUIEvent is a GUISignal which is send from the GUI Element and which indicates a user interaction
data GUIEvent = OnChange  -- ^ indicate a change of the value of the GUI Element
              deriving (Show, Read, Eq)

instance J.FromJSON GUIEvent where
  parseJSON (String sig) = return (read (unpack sig))
  parseJSON _ = mzero
  
instance J.ToJSON GUIEvent where
  toJSON sig = String (pack (show sig))
