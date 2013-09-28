{-# LANGUAGE TemplateHaskell, QuasiQuotes, OverloadedStrings, TypeFamilies, MultiParamTypeClasses, Arrows #-}
module Main where

import Yesod
import qualified Control.Wire as CW
import Control.Wire (pure, (<|>), (<$>), (.))
import Prelude hiding ((.), id)

import qualified HWebUI as HW

main :: IO ()
main = do
    -- settings 
    let port = 8080
        
    -- create gui elements and layout
    let guiLayout = do    
        HW.wInitGUI port
        
        -- buttons
        [whamlet|
              <H1>HWebUI - Counter Example
              The following buttons increase and decrease the counter:
                    |]
        HW.wButton "Button1" "Up"
        HW.wButton "Button2" "Down"

        -- finally the output text as html
        [whamlet|
              <p>And here the output value: 
              <p>
        |]
        HW.wHtml "out1" 

    -- create netwire gui elements
    let theWire = do
        
        up <- HW.buttonW "Button1" 
        down <- HW.buttonW "Button2" 
        output <- HW.htmlW "out1" 
        
        -- build the FRP wire, we need a counter, which increases a value at each up event and decreases it at each down event
    
        -- this wire counts from 0, part of prefab netwire Wires
        let cnt = CW.countFrom (0::Int)

        -- this wire adds one on button up, substracts one on button down, return id on no button press
        let w1 = cnt . ( up . pure 1 <|> down . pure (-1)  <|> pure 0 )

        -- stringify the output result (applicative style)
        let strw1 = (Just . show ) <$> w1
        
        -- set the output on change only
        return $ output . CW.changed . strw1 
    
    -- run the webserver, the netwire loop and wait for termination   
    HW.runHWebUI port guiLayout theWire

    return ()
    

