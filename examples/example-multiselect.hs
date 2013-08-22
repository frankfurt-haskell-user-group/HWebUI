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
              <H1>HWebUI - MultiSelect Example
              <p>
                    |]

        wMultiSelect "mselect" 200
        wHtml "output"

    -- create functionality 
    -----------------------
    let channelAndWire = do    
        mselect <- multiSelectW "mselect" 
        output<- htmlW "output" 
        
    -- build the FRP wire, arrow notation, with recursion, using delay (!)
        let w1 = mselect . (once . pure ( Just [("one", False, "data from one"), 
                                             ("two", False, "data from two"), 
                                             ("three", False, "data from three"), 
                                             ("four", False, "data from four")]) 
                         <|> pure Nothing) 
        let w2 = output . (Just . Prelude.foldl (\a b -> a ++ " " ++ show b) "Selected: " 
                       <$> id) 
                    . w1
        return w2
    
    -- run the webserver, the netwire loop and wait for termination   
    runHWebUI port guiLayout channelAndWire

    return ()
