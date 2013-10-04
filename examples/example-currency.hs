{-# LANGUAGE TemplateHaskell, QuasiQuotes, OverloadedStrings, TypeFamilies, MultiParamTypeClasses, Arrows #-}
module Main where

import Yesod
import Control.Wire
import Prelude hiding ((.), id)


import HWebUI as HW
import qualified WidgetWires as WW


-- a double conversion function
atof :: String -> Double
atof instr = case reads instr of
     [] -> 0.0
     [(f, x)] -> f
    
namedWidgetWire = do
    WW.WidgetWire wTextBoxDollar textBoxDollarW <- WW.wwTextBox
    WW.WidgetWire wTextBoxEuro textBoxEuroW <- WW.wwTextBox

    -- create layout 
    ----------------
        
    let pl = []
    let guiLayout = do    
            
        -- a table with the entry fields (as text) the operator and the result
        [whamlet|
              <H1>HWebUI - Currency Example
              <p>
                    |]

        [whamlet|
           <table>
                   <tr>
                     <td> US Dollars
                     <td> ^{wTextBoxDollar pl}
                   <tr>
                     <td> Euros
                     <td> ^{wTextBoxEuro pl} 
                             |]



    -- create functionality 
    -----------------------
        
    let theWire = do
        
        dollars <- textBoxDollarW 
        euros <- textBoxEuroW 
        
        -- build the FRP wire, arrow notation, with recursion, using delay (!)
    
        -- get some double wire, sime like dollars, euros, just with doubles
        let dollD = (atof <$> dollars) . (fmap show <$> id)
        let euroD = (atof <$> euros) . (fmap show <$> id)
        
        -- output with Maybe in addition to input with maybe
        let dollD' = ((Just <$> id) . dollD) <|> pure Nothing
        let euroD' = ((Just <$> id) . euroD) <|> pure Nothing
        
        -- the rate
        let rate = 1.5
        
        -- need arrow, to bind recursively
        let runW = proc _ -> do
              rec
               (d', e') <- delay (Just 1.0, Just (1.0/rate)) -< (d, e)
               d <- fmap (* rate) <$> dollD' -< e'
               e <- fmap (/ rate) <$> euroD' -< d'                 
           
              returnA -< (d, e)
          
        return runW
    
    return (WW.WidgetWire guiLayout theWire)

main :: IO ()
main = do
         -- settings 
         let port = 8080
         -- run the webserver, the netwire loop and wait for termination   
         HW.runHWebUIWW port namedWidgetWire 
