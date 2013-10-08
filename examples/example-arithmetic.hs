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
    textBoxArg1 <- hwuTextBox []
    textBoxArg2 <- hwuTextBox []
    radioButtonAdd <- hwuRadioButton [name := "operator", value := "opea", checked := True]        
    radioButtonSub <- hwuRadioButton [name := "operator", value := "opes", checked := False]        
    radioButtonMul <- hwuRadioButton [name := "operator", value := "opem", checked := False]        
    radioButtonDiv <- hwuRadioButton [name := "operator", value := "oped", checked := False]       
    out <- hwuHtml []
    
    let guiLayout = do    
        
        -- a table with the entry fields (as text) the operator and the result
        [whamlet|
              <H1>HWebUI - Arithmetics Example
              Insert Numbers in the both arguments and choose your operator:
              <p>
                    |]

        [whamlet|
           <table>
             <tr>
               <td> ^{hwuLayout textBoxArg1}
               <td>
                 <table>
                   <tr>
                     <td>add ^{hwuLayout radioButtonAdd}
                   <tr>
                     <td>sub ^{hwuLayout radioButtonSub}
                   <tr>
                     <td>mul ^{hwuLayout radioButtonMul}
                   <tr>
                     <td>div ^{hwuLayout radioButtonDiv}
               <td> ^{hwuLayout textBoxArg2}
                             |]


        -- finally the output text as html
        [whamlet|
              <p>And here the output value: 
              <p>
        |]
        (hwuLayout out)

    
    let wireIn = proc _ -> do
            a1 <- hold "" (hwuWire textBoxArg1) -< Nothing
            a2 <- hold "" (hwuWire textBoxArg2) -< Nothing
            badd <- hold True (hwuWire radioButtonAdd) -< Nothing
            bsub <- hold False (hwuWire radioButtonSub) -< Nothing
            bmul <- hold False (hwuWire radioButtonMul) -< Nothing
            bdiv <- hold False (hwuWire radioButtonDiv) -< Nothing
            
            let op 
                 | badd = (+)
                 | bsub = (-)
                 | bmul = (*) 
                 | bdiv = (/) 
                 | otherwise = \ _ _ -> 0.0
            
            let res = op (atof a1) (atof a2)
            returnA -< res                             

    let theWire = (hwuWire out) .  ((Just . show) <$> wireIn) . pure Nothing
    
    return (guiLayout, theWire)
    
main :: IO ()
main = do
         -- settings 
         let port = 8080
         -- run the webserver, the netwire loop and wait for termination   
         runHWebUI port guiDefinition 