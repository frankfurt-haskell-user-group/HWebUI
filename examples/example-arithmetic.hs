{-# LANGUAGE TemplateHaskell, QuasiQuotes, OverloadedStrings, TypeFamilies, MultiParamTypeClasses, Arrows #-}
module Main where

import Yesod
import Control.Concurrent (threadDelay)
import Control.Wire
import Prelude hiding ((.), id)
import Data.Map

import HWebUI

-- a double conversion function
atof :: String -> Double
atof instr = case (reads instr) of
     [] -> 0.0
     [(f, x)] -> f
      
main :: IO ()
main = do
    -- settings 
    let port = 8080
        
    -- create gui elements and layout
    let guiLayout = do    
        wInitGUI port
        
        -- a table with the entry fields (as text) the operator and the result
        [whamlet|
              <H1>HWebUI - Arithmetics Example
              Insert Numbers in the both arguments and choose your operator:
              <p>
                    |]

        [whamlet|
           <table>
             <tr>
               <td> ^{wTextBox "arg1"}
               <td>
                 <table>
                   <tr>
                     <td> ^{wRadioButton "rbadd" "operator" "opea" True} add
                   <tr>
                     <td> ^{wRadioButton "rbsub" "operator" "opeb" False} sub
                   <tr>
                     <td> ^{wRadioButton "rbmul" "operator" "opec" False} mul
                   <tr>
                     <td> ^{wRadioButton "rbdiv" "operator" "oped" False} div
               <td> ^{wTextBox "arg2"}
                             |]


        -- finally the output text as html
        [whamlet|
              <p>And here the output value: 
              <p>
        |]
        wHtml "out1" 

    -- create netwire gui elements
    let gsmap = (fromList [])::(Map String GSChannel)
        
    (arg1, gsmap) <- textBoxW "arg1" gsmap
    (arg2, gsmap) <- textBoxW "arg2" gsmap
    (addB, gsmap) <- radioButtonW "rbadd" gsmap
    (subB, gsmap) <- radioButtonW "rbsub" gsmap
    (mulB, gsmap) <- radioButtonW "rbmul" gsmap
    (divB, gsmap) <- radioButtonW "rbdiv" gsmap
    (out1, gsmap) <- htmlW "out1" gsmap
        

    -- build the FRP wire, arrow notation
    
    let result = proc _ -> do
                               a1 <- hold "" arg1 -< Nothing
                               a2 <- hold "" arg2 -< Nothing
			       badd <- hold True addB -< Nothing
      			       bsub <- hold False subB -< Nothing
                               bmul <- hold False mulB -< Nothing
                               bdiv <- hold False divB -< Nothing
                               
                               let op = if badd then (+) else (if bsub then (-) else (if bmul then (*) else (if bdiv then (/) else (\ x y -> 0.0))))
                               let res = op (atof a1) (atof a2)

                               returnA -< res                             

    let theWire = out1 .  ((Just . show) <$> result) . pure Nothing
    
    -- run the webserver, the netwire loop and wait for termination   
    runHWebUI port gsmap guiLayout theWire

    return ()
    

