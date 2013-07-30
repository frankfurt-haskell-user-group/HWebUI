{-# LANGUAGE TemplateHaskell, QuasiQuotes, OverloadedStrings, TypeFamilies, MultiParamTypeClasses, Arrows #-}


{- | HWebUI is providing FRP-based GUI functionality for Haskell by utilizing the Web-Browser. It is build on top of Yesod for the Web technologies and on netwire for the FRP interface. The status is \"early prototype\". The implementation uses a Javascript library (Dojo toolkit) for providing typical widgets, HTML for the layout of the widgets. With Javascript and websockets events are transferred between the Web and the Haskell world. This happens behind the scenes. The Haskell programmer is using a FRP based interface. See also: <http://www.github.com/althainz/HWebUI>.
-}
module GUIEvent (
  GUIEvent (..)
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

-----------
-- GUIEvent
-----------

-- The GUIEvent is a GUISignal which get send from the GUI Element and which indicates a user interaction

-- purpose of signal, currently only value setting or change notification are used
data GUIEvent = OnChange deriving (Show, Read, Eq)

instance J.FromJSON GUIEvent where
  parseJSON (String sig) = return (read (unpack sig))
  parseJSON _ = mzero
instance J.ToJSON GUIEvent where
  toJSON sig = String (pack (show sig))

