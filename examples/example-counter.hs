{-# LANGUAGE TemplateHaskell, QuasiQuotes, OverloadedStrings, TypeFamilies, MultiParamTypeClasses, Arrows #-}
module Main where

import Yesod
import qualified Control.Wire as CW
import Control.Wire (pure, (<|>), (<$>), (.))
import Prelude hiding ((.), id)

import qualified HWebUI as HW
import qualified WidgetWires as WW

namedWidgetWire = do
         WW.WidgetWire wButton1 button1W <- WW.wwButton
         WW.WidgetWire wButton2 button2W <- WW.wwButton    
         WW.WidgetWire wOut outW <- WW.wwHtml
     
         -- create gui elements and layout
         let guiLayout = do    
                      
             -- buttons
             [whamlet|
                   <H1>HWebUI - Counter Example
                   The following buttons increase and decrease the counter:
                         |]
             wButton1 [HW.label HW.:= "Up"]
             wButton2 [HW.label HW.:= "Down"]
     
             -- finally the output text as html
             [whamlet|
                   <p>And here the output value: 
                   <p>
             |]
             wOut 
     
         -- create netwire gui elements
         let theWire = do
             
             up <- button1W 
             down <- button2W 
             output <- outW 
             
             -- build the FRP wire, we need a counter, which increases a value at each up event and decreases it at each down event
         
             -- this wire counts from 0, part of prefab netwire Wires
             let cnt = CW.countFrom (0::Int)
     
             -- this wire adds one on button up, substracts one on button down, return id on no button press
             let w1 = cnt . ( up . pure 1 <|> down . pure (-1)  <|> pure 0 )
     
             -- stringify the output result (applicative style)
             let strw1 = (Just . show ) <$> w1
             
             -- set the output on change only
             return $ output . CW.changed . strw1 
        
         return (WW.WidgetWire guiLayout theWire) 
          
main :: IO ()
main = do
         -- settings 
         let port = 8080
         -- run the webserver, the netwire loop and wait for termination         
         HW.runHWebUIWW port namedWidgetWire 
