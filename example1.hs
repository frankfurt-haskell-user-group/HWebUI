{-# LANGUAGE TemplateHaskell, QuasiQuotes, OverloadedStrings, TypeFamilies, MultiParamTypeClasses, Arrows #-}
module Main where

import Yesod
import Network.Wai.Handler.Warp (runSettings, Settings(..), defaultSettings)
import qualified Network.WebSockets             as WS
import qualified Network.Wai.Handler.WebSockets as WS
import qualified Data.Aeson                     as J
import           Data.Time.Clock (getCurrentTime)
import           Control.Monad (forever)
import           Control.Concurrent (forkIO, threadDelay)
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
import Data.Attoparsec.Number as N

import HWebUI
import Text.Printf

loop1 wire session = do
    (r, wire',  session') <- stepSession wire session ()
    threadDelay 10000
    loop1 wire' session'
    return ()

      
main :: IO ()
main = do
    -- settings 
    let port = 8080
        
    -- create gui elements and layout
    let guiLayout = do    
        wInitGUI port
        
        -- buttons
        toWidget [hamlet|
              <H1>Cool Webgui Test 
              A couple of buttons for input:
                    |]
        wButton "Button1"
        wButton "Button2"

        -- a text box
        toWidget [hamlet|
              <p>A textbox, to capture text input
                    |]
        wTextBox "TextBox1" 

        -- a checkbox
        toWidget [hamlet|
              <p>A checkbox for advanced examples
                    |]
        wCheckBox "CheckBox1" 

        -- finally the output text as html
        toWidget [hamlet|
              <p>And here the output text: 
              <p>
        |]
        wHtml "out1" 

    -- create netwire gui elements
    let gsmap = (fromList [])::(Map String GSChannel)
        
    (t, gsmap) <- textBoxW "TextBox1" gsmap
    (b1, gsmap) <- buttonW "Button1" gsmap
    (b2, gsmap) <- buttonW "Button2" gsmap
    (cb, gsmap) <- checkBoxW "CheckBox1" gsmap
    (output, gsmap) <- htmlW "out1" gsmap
        
    -- run the webserver   
    forkChild $ runWebserver port gsmap guiLayout
     
    -- build the FRP wire
--    let secondClock = hold (periodically 1 . second)
--    let v = mkFix (\t val -> Right $ Just (show val))
--    let j = mkFix (\t val -> Right $ Just val)

    
    let wif = proc a -> do
         b <- cb -< Nothing
         returnA -< if b then a else Nothing

    let w3 = (pure $ "Button 2 gefeuert") . cb . b2 . (pure $ Just False)
    let w2 = wif . b1 . (pure $ Just "Button 1 gefeuert")
    let w1 = periodically 5 . (pure $ Just "hallo ausgabe")

        
    let theWire = w3 <|> (output . (w2 <|> w1))
    
    -- loop netwire
    loop1 theWire clockSession
    
    -- wait for the webserver to terminate
    waitForChildren
    return ()
    


