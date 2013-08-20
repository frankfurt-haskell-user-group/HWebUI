{-# LANGUAGE TemplateHaskell, QuasiQuotes, OverloadedStrings, TypeFamilies, MultiParamTypeClasses, Arrows #-}
module Main where

import Yesod
import Control.Concurrent (threadDelay)
import Control.Wire
import Prelude hiding ((.), id)
import Data.Map

import HWebUI

main :: IO ()
main = do
    -- settings 
    let port = 8080
        
    -- create gui elements and layout
    let guiLayout = do    
        wInitGUI port
        
        -- buttons
        toWidget [hamlet|
              <H1>HWebUI - Counter Example
              The following buttons increase and decrease the counter:
                    |]
        wButton "Button1" "Up"
        wButton "Button2" "Down"

        -- finally the output text as html
        toWidget [hamlet|
              <p>And here the output value: 
              <p>
        |]
        wHtml "out1" 

    -- create netwire gui elements
    let theWire = do
        
        up <- buttonW "Button1" 
        down <- buttonW "Button2" 
        output <- htmlW "out1" 
        
        -- build the FRP wire, we need a counter, which increases a value at each up event and decreases it at each down event
    
        -- this wire counts from 0, part of prefab netwire Wires
        let cnt = countFrom (0::Int)

        -- this wire adds one on button up, substracts one on button down, return id on no button press
        let w1 = cnt . ( up . pure 1 <|> down . pure (-1)  <|> pure 0 )

        -- stringify the output result (applicative style)
        let strw1 = (Just . show ) <$> w1
        
        -- set the output on change only
        return $ output . changed . strw1 
    
    -- run the webserver, the netwire loop and wait for termination   
    runHWebUI port guiLayout theWire

    return ()
    

