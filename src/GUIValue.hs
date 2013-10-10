{- | GUIValue is an internal implementation module of "HWebUI". "HWebUI" is providing FRP-based GUI functionality for Haskell by utilizing the Web-Browser. See module "HWebUI" for main documentation. 
-}

{-# OPTIONS_HADDOCK hide #-}

module GUIValue (
  GUIValue (..)
  ) where

import Yesod
import qualified Data.Aeson                     as J
import System.IO (hFlush, stdout)
import Control.Monad
import Prelude hiding ((.), id)
import Data.Text
import Data.Vector (toList, fromList)
import Data.Attoparsec.Number as N

-- | A GUI value is the content of a GUI element, a String, a Bool a
-- Number or similar. Compound types are realised as lists of
-- values. This data type is used in 'GUIMessage' to encode different
-- values in a common data type.
data GUIValue = SVDouble Double
              | SVString String
              | SVList [GUIValue]
              | SVInt Int
              | SVBool Bool
              | SVEvent
              | SVNone
     deriving (Show, Read, Eq)

instance J.FromJSON GUIValue where
  parseJSON (String "Event") = return SVEvent
  parseJSON (String "None") = return SVNone
  parseJSON (Number (N.I i)) = return $ SVInt (fromIntegral i)
  parseJSON (Number (N.D d)) = return $ SVDouble d
  parseJSON (Bool b) = return $ SVBool b
  parseJSON (String s) = return $ SVString (unpack s)
  parseJSON (Array v) = 
    case Data.Vector.toList v of
        (s:ss) -> do
          (SVList sr) <- J.parseJSON (Array (Data.Vector.fromList ss))
          s1 <- J.parseJSON s
          return $ SVList (s1 : sr)
        [] -> return $ SVList []
  parseJSON _ = mzero
  
instance J.ToJSON GUIValue where
  toJSON (SVDouble d) = toJSON d
  toJSON (SVString s) = toJSON s
  toJSON (SVInt i) = toJSON i
  toJSON (SVBool b) = toJSON b
  toJSON (SVList sl) = toJSON sl
  toJSON (SVEvent) = String "Event"
  toJSON (SVNone) = String "None"

