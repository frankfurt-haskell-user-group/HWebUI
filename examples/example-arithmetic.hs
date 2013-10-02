{-# LANGUAGE TemplateHaskell, QuasiQuotes, OverloadedStrings, TypeFamilies, MultiParamTypeClasses, Arrows #-}
module Main where

import Yesod
import Control.Wire
import Prelude hiding ((.), id)

import HWebUI
import qualified WidgetWires as WW

-- a double conversion function
atof :: String -> Double
atof instr = case reads instr of
     [] -> 0.0
     [(f, x)] -> f
      

namedWidgetWire = do
    let pl = []
    WW.WidgetWire wTextBoxArg1 textBoxArg1W <- WW.wwTextBox
    WW.WidgetWire wTextBoxArg2 textBoxArg2W <- WW.wwTextBox
    WW.WidgetWire wRadioButtonAdd radioButtonAddW <- WW.wwRadioButton        
    WW.WidgetWire wRadioButtonSub radioButtonSubW <- WW.wwRadioButton        
    WW.WidgetWire wRadioButtonMul radioButtonMulW <- WW.wwRadioButton        
    WW.WidgetWire wRadioButtonDiv radioButtonDivW <- WW.wwRadioButton       
    WW.WidgetWire wOut outW <- WW.wwHtml
    -- create gui elements and layout
    let rba = [name := "operator", value := "opea", checked := True]
    let rbs = [name := "operator", value := "opes", checked := False]
    let rbm = [name := "operator", value := "opem", checked := False]
    let rbd = [name := "operator", value := "oped", checked := False]
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
               <td> ^{wTextBoxArg1  pl}
               <td>
                 <table>
                   <tr>
                     <td> ^{wRadioButtonAdd rba}
                   <tr>
                     <td> ^{wRadioButtonSub rbs}
                   <tr>
                     <td> ^{wRadioButtonMul rbm}
                   <tr>
                     <td> ^{wRadioButtonDiv rbd}
               <td> ^{wTextBoxArg2  pl}
                             |]


        -- finally the output text as html
        [whamlet|
              <p>And here the output value: 
              <p>
        |]
        wOut

    -- create netwire gui elements
    let theWire = do
       
         arg1 <- textBoxArg1W
         arg2 <- textBoxArg2W
         addB <- radioButtonAddW
         subB <- radioButtonSubW
         mulB <- radioButtonMulW
         divB <- radioButtonDivW
         out1 <- outW
             

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
                                         | otherwise = \ _ _ -> 0.0
                                    
                                    let res = op (atof a1) (atof a2)
                                    returnA -< res                             

         return $ out1 .  ((Just . show) <$> result) . pure Nothing
    
    return (WW.WidgetWire guiLayout theWire)
    
main :: IO ()
main = do
         -- settings 
         let port = 8080
         -- run the webserver, the netwire loop and wait for termination   
         runHWebUIWW port namedWidgetWire 