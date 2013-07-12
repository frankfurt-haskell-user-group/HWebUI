{-# LANGUAGE TemplateHaskell, QuasiQuotes, OverloadedStrings, TypeFamilies, MultiParamTypeClasses, Arrows #-}
module Main where

import Yesod
import Control.Concurrent (threadDelay)
import Control.Wire
import Prelude hiding ((.), id)
import qualified Data.Map as M
import Data.Maybe
import Data.List
import Control.Lens

import HWebUI

loop1 wire session = do
    (r, wire',  session') <- stepSession wire session ()
    threadDelay 10000
    loop1 wire' session'
    return ()

-- a double conversion function
atof :: String -> Double
atof instr = case (reads instr) of
     [] -> 0.0
     [(f, x)] -> f
      
                 
-- data and functionality, to modify data
     
data Name = Name { _preName :: String, _surName :: String} deriving (Eq, Show)  -- Name Surname
makeLenses ''Name

data Entries = Entries { _namesEntries :: [Name], _indexEntries :: (Maybe Int)} deriving (Eq, Show)  -- List of names, maybe index of selected element
makeLenses ''Entries



initialEntries :: Entries
initialEntries = Entries [] Nothing



    
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
        toWidget [hamlet|
              <H1>HWebUI - CRUD Example
              <p>
                    |]
          
        [whamlet|
         <table>
           <tr>
             <td>Filter Prefix:
             <td>^{wTextBox "tb-filter-prefix"}
             <td>Name:
             <td>^{wTextBox "tb-prename"}
           <tr>        
             <td>
             <td>
             <td>Surname:
             <td>^{wTextBox "tb-surname"}
         ^{wMultiSelect "ms-entries" 350}
         <table>    
           <tr>    
             <td>^{wButton "bt-create" "Create Entry"}
             <td>^{wButton "bt-delete" "Delete Entry"}
                 
         
         |]


    -- create functionality 
    -----------------------
        
    let gsmap = (M.fromList [])::(M.Map String GSChannel)
        
    (prefix, gsmap) <- textBoxW "tb-filter-prefix" gsmap
    (prename, gsmap) <- textBoxW "tb-prename" gsmap
    (surname, gsmap) <- textBoxW "tb-surname" gsmap
    (create, gsmap) <- buttonW "bt-create" gsmap
    (entrieslist, gsmap) <- multiSelectW "ms-entries" gsmap
    (delete, gsmap) <- buttonW "bt-delete" gsmap
        
    -- build the FRP wires
    
    let addEntryW = mkFix (\t (Entries n i) -> Right (Entries (n ++ [Name "New Entry" "(edit me!)"])  i))
    let delEntryW = mkFix (\t ((Entries n i), sel) -> if null sel then 
                                                        Right (Entries n i) 
                                                        else 
                                                           Right $ Entries (fmap snd (filter (\(a,b) -> a /= head sel) (zip [0..] n) ) ) i 
                          )
        
    let ifThenJust b a = if b then Just a else Nothing
        
    let getDisplay = (\e -> (e^.preName) ++ " " ++ (e^.surName))
        
    let w1 = proc (entries, selection'', filtertxt) -> do
          
          fchange <- isJust <$> (event changed) -< filtertxt
          
          let lkup = (zip [0..] $ fmap fst $ filter (\(a,b) -> isInfixOf filtertxt b) (zip [0..] (fmap getDisplay (entries^.namesEntries))))::[(Int,Int)]
              
          let preFromI i = (namesEntries . element i . preName)
          let surFromI i = (namesEntries . element i . surName)
          let nameFromI i =  (namesEntries . element i )
          
          let selection = if fchange then ([]::[Int]) else selection''
         
          echange <- isJust <$> (event changed) -< entries 
          schange <- isJust <$> (event changed) -< selection
          
          
          
          
          
          (entries', selection', filtertxt') <-
                do
                         -- handle multiselect element
                         selection' <- entrieslist -<  ifThenJust (echange || fchange) (fmap (\(a,b) -> (fmap getDisplay (entries^.namesEntries))!!b ) lkup )
                         returnA -< (entries, selection', filtertxt)
                   <|> do
                         -- create a new entry
                         entries' <- addEntryW . create -< entries
                         returnA -< (entries', selection, filtertxt)
                   <|> do
                         -- delete entry
                         entries' <- delEntryW . delete -< (entries, selection)
                         returnA -< (entries', [], filtertxt)
                   <|> do
                         -- check filtertxt
                         filtertxt' <- prefix -< Nothing
                         returnA -< (entries, selection, filtertxt')
                   <|> do
                         -- check prename change
                         pn <- prename -< ifThenJust ((schange || fchange) && not (null selection)) (view (preFromI $ (fromJust (lookup (head selection) lkup))) entries)
                         let entries' = if length selection > 0 then
                                          (preFromI $ (fromJust (lookup (head selection) lkup))) .~ pn $ entries
                                          else
                                            entries
                         returnA -< (entries', selection, filtertxt)
                   <|> do
                         -- check surname change
                         sn <- surname  -< ifThenJust ((schange || fchange) && not (null selection)) (view (surFromI $  (fromJust (lookup (head selection) lkup)))  entries)
                         let entries' = if length selection > 0 then
                                          (surFromI $ (fromJust (lookup (head selection) lkup))) .~ sn $ entries
--                                          (surFromI (selection!!0)) .~ sn $ entries
                                          else
                                            entries
                         returnA -< (entries', selection, filtertxt)
                   <|> do
                         returnA -< (entries, selection, filtertxt)
          returnA -< (entries', selection', filtertxt')

    let w2 = proc _ -> do
                       rec
                         (entries, selection, filtertxt) <- delay (initialEntries, [], "") -< (entries', selection', filtertxt')
                         (entries', selection', filtertxt') <- w1 -< (entries, selection, filtertxt)
                       returnA -< ()
    
    
    let theWire = w2
    
    -- run the GUI
    --------------
        
    -- run the webserver   
    forkChild $ runWebserver port gsmap guiLayout
    -- loop netwire
    loop1 theWire clockSession
    -- wait for the webserver to terminate
    waitForChildren
    return ()
    
    

