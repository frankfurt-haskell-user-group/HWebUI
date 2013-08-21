{-# LANGUAGE TemplateHaskell, QuasiQuotes, OverloadedStrings, TypeFamilies, MultiParamTypeClasses, Arrows #-}


{- | GUICommand is an internal implementation module of "HWebUI". "HWebUI" is providing FRP-based GUI functionality for Haskell by utilizing the Web-Browser. See module "HWebUI" for main documentation. 
-}
module GUICommand (
  GUICommand (..)
  ) where

import Yesod
import qualified Data.Aeson                     as J
import System.IO (hFlush, stdout)
import Control.Monad
import Prelude hiding ((.), id)
import Data.Text

-- | The GUICommand is a GUISignal which is send towards the GUI element and which signals a command to the element
data GUICommand = SetValue -- ^ set the value of the GUI element
                deriving (Show, Read, Eq)

instance J.FromJSON GUICommand where
  parseJSON (String sig) = return (read (unpack sig))
  parseJSON _ = mzero

instance J.ToJSON GUICommand where
  toJSON sig = String (pack (show sig))
