{-# LANGUAGE TemplateHaskell, QuasiQuotes, OverloadedStrings, TypeFamilies, MultiParamTypeClasses, Arrows #-}
module Main where

import Yesod
import Control.Concurrent (threadDelay)
import Control.Wire
import Prelude hiding ((.), id)
import Data.Map

import HWebUI

loop1 wire session = do
    (r, wire',  session') <- stepSession wire session ()
    threadDelay 100000
    loop1 wire' session'
    return ()

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
        toWidget [hamlet|
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
        toWidget [hamlet|
              <p>And here the output value: 
              <p>
        |]
        wHtml "out1" 

    -- create netwire gui elements
    let gsmap = (fromList [])::(Map String GSChannel)
        
    (arg1, gsmap1) <- textBoxW "arg1" gsmap
    (arg2, gsmap2) <- textBoxW "arg2" gsmap1
    (addB, gsmap3) <- radioButtonW "rbadd" gsmap2
    (subB, gsmap4) <- radioButtonW "rbsub" gsmap3
    (mulB, gsmap5) <- radioButtonW "rbmul" gsmap4
    (divB, gsmap6) <- radioButtonW "rbdiv" gsmap5
    (out1, gsmap7) <- htmlW "out1" gsmap6
        
    -- run the webserver   
    forkChild $ runWebserver port gsmap7 guiLayout
     

    -- build the FRP wire, we need a couple of elements
    
    -- get a double wire from a string wire
    let dbl = mkFix (\t s -> Right (atof s))
    let a1 = dbl . arg1 . pure Nothing
    let a2 = dbl . arg2 . pure Nothing

    --
    let result = proc _ -> do
                               a1 <- arg1 -< Nothing
                               a2 <- arg2 -< Nothing
			       badd <- addB -< Nothing
      			       bsub <- subB -< Nothing
                               bmul <- mulB -< Nothing
                               bdiv <- divB -< Nothing
                               
                               let op = if badd then (+) else (if bsub then (-) else (if bmul then (*) else (if bdiv then (/) else (\ x y -> 0.0))))
                               let res = op (atof a1) (atof a2)

                               returnA -< res                             

    let theWire = out1 .  ((Just . show) <$> result) . pure Nothing
    
    -- loop netwire
    loop1 theWire clockSession
    
    -- wait for the webserver to terminate
    waitForChildren
    return ()
    

