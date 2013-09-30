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
      
main :: IO ()
main = do
    -- settings 
    let port = 8080
        
    -- create gui elements and layout
    let rba = wRadioButton "rbadd" [name := "operator", value := "opea", checked := True]
    let rbs = wRadioButton "rbsub" [name := "operator", value := "opes", checked := False]
    let rbm = wRadioButton "rbmul" [name := "operator", value := "opem", checked := False]
    let rbd = wRadioButton "rbdiv" [name := "operator", value := "oped", checked := False]
    let pl = []
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
               <td> ^{wTextBox "arg1" pl}
               <td>
                 <table>
                   <tr>
                     <td> ^{rba} add
                   <tr>
                     <td> ^{rbs} sub
                   <tr>
                     <td> ^{rbm} mul
                   <tr>
                     <td> ^{rbd} div
               <td> ^{wTextBox "arg2" pl}
                             |]


        -- finally the output text as html
        [whamlet|
              <p>And here the output value: 
              <p>
        |]
        wHtml "out1" 

    -- create netwire gui elements
    let theWire = do
        
         arg1 <- textBoxW "arg1"
         arg2 <- textBoxW "arg2"
         addB <- radioButtonW "rbadd"
         subB <- radioButtonW "rbsub"
         mulB <- radioButtonW "rbmul"
         divB <- radioButtonW "rbdiv"
         out1 <- htmlW "out1"
             

       -- build the FRP wire, arrow notation
    
         let result = proc _ -> do
                                    a1 <- hold "" arg1 -< Nothing
                                    a2 <- hold "" arg2 -< Nothing
                                    badd <- hold True addB -< Nothing
                                    bsub <- hold False subB -< Nothing
                                    bmul <- hold False mulB -< Nothing
                                    bdiv <- hold False divB -< Nothing
                                    
                                    let op 
                                         | badd = (+)
                                         | bsub = (-)
                                         | bmul = (*) 
                                         | bdiv = (/) 
                                         | otherwise = \ x y -> 0.0
                                    
                                    let res = op (atof a1) (atof a2)
                                    returnA -< res                             

         return $ out1 .  ((Just . show) <$> result) . pure Nothing
    
    -- run the webserver, the netwire loop and wait for termination   
    runHWebUI port guiLayout theWire

    return ()
    

