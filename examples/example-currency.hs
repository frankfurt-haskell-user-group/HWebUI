{-# LANGUAGE TemplateHaskell, QuasiQuotes, OverloadedStrings, TypeFamilies, MultiParamTypeClasses, Arrows #-}
module Main where

import Yesod
import Control.Wire
import Prelude hiding ((.), id)
import Data.Map

import HWebUI

-- a double conversion function
atof :: String -> Double
atof instr = case reads instr of
     [] -> 0.0
     [(f, x)] -> f
      
main :: IO ()
main = do
    -- settings 
    -----------
  
    let port = 8080
        
    -- create layout 
    ----------------
        
    let guiLayout = do    
        wInitGUI port
        
        -- a table with the entry fields (as text) the operator and the result
        [whamlet|
              <H1>HWebUI - Currency Example
              <p>
                    |]

        [whamlet|
           <table>
                   <tr>
                     <td> US Dollars
                     <td> ^{wTextBox "tb-dollars"}
                   <tr>
                     <td> Euros
                     <td> ^{wTextBox "tb-euros"} 
                             |]



    -- create functionality 
    -----------------------
        
    let theWire = do
        
        dollars <- textBoxW "tb-dollars" 
        euros <- textBoxW "tb-euros" 
        
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
    
    -- run the webserver, the netwire loop and wait for termination   
    runHWebUI port guiLayout theWire
    
    return ()
    
    

