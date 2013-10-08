{-# LANGUAGE TemplateHaskell, QuasiQuotes, OverloadedStrings, TypeFamilies, MultiParamTypeClasses, Arrows #-}
module Main where

import Yesod
import Control.Wire
import Prelude hiding ((.), id)

import HWebUI

-- a double conversion function
atof :: String -> Double
atof instr = case reads instr of
     [] -> 0.0
     [(f, x)] -> f
      
guiDefinition = do
    multiSelect <- hwuMultiSelect [width := 350]
    buttonStart <- hwuButton [label := "Start"]
    out <- hwuHtml []

    -- define layout 

    let guiLayout = do    
        
        -- a table with the entry fields (as text) the operator and the result
        [whamlet|
              <H1>HWebUI - MultiSelect Example
              <p>
                   |]
        [whamlet|
         <table>
           <tr>
             <td>^{hwuLayout buttonStart}
         |]
        
        hwuLayout multiSelect
        hwuLayout out

    -- define functionality 
        
    -- build the FRP wire, arrow notation, with recursion, using delay (!)
    let choices = pure ( Just [("one", False, "data from one"), 
                                             ("two", False, "data from two"), 
                                             ("three", False, "data from three"), 
                                             ("four", False, "data from four")])
    let w1 = (hwuWire multiSelect) . ( (hwuWire buttonStart) *> (once . choices <|> pure Nothing)
                             )
    let w2 = (hwuWire out) . (Just . Prelude.foldl (\a b -> a ++ " " ++ b) "Selected: " 
                       <$> id) 
                    . w1
    return w2

    return (guiLayout, w2)

main :: IO ()
main = do
         -- settings 
         let port = 8080
         -- run the webserver, the netwire loop and wait for termination   
         runHWebUI port guiDefinition     
