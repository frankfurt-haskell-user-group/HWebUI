{-# LANGUAGE TemplateHaskell, QuasiQuotes, OverloadedStrings, TypeFamilies, MultiParamTypeClasses, Arrows #-}
module Main where

import Yesod
import Control.Wire
import Prelude hiding ((.), id)
import qualified Data.Map as M
import Data.Maybe
import Data.List hiding (delete)
import Control.Lens

import HWebUI

-- a double conversion function
atof :: String -> Double
atof instr = case (reads instr) of
     [] -> 0.0
     [(f, x)] -> f
      
                 
-- data and functionality, to modify data, also the filter feature needs to be considered
-- therfore, we need two pieces of data, one for keeping the global state, this is entries  
-- and one for keeping the filtered state, which is after applying the filter and adding index into allEntries (!), this is fiEntries
     

-- basic names, this is the main data
     
data Name = Name { _preName :: String, _surName :: String} deriving (Eq, Show)  -- Name Surname
makeLenses ''Name

type Names = [Name]

-- triple with data, selection, index for the gui multiselect handling

type Entry = (String, Bool, Int)

guiText :: Lens' Entry String
guiText = _1

guiSelected :: Lens' Entry Bool
guiSelected = _2

guiIndex :: Lens' Entry Int
guiIndex = _3

type Entries = [Entry]
type Selection = [Int]

-- helper functions for data

makeEntries :: Names -> Selection -> String -> Entries
makeEntries names selection filtertxt = filter ffilter $ fmap fentry (zip names [0..]) where
  ffilter = (\e -> isInfixOf filtertxt (e ^. guiText))
  fentry = (\(name, i) -> ( (name ^. preName) ++ " " ++ (name ^. surName), elem i selection, i))

onChangeJust :: Bool -> a -> Maybe a
onChangeJust b work = if b then Just work else Nothing
          

  
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
    (prenameTxt, gsmap) <- textBoxW "tb-prename" gsmap
    (surnameTxt, gsmap) <- textBoxW "tb-surname" gsmap
    (create, gsmap) <- buttonW "bt-create" gsmap
    (entrieslist, gsmap) <- multiSelectW "ms-entries" gsmap
    (delete, gsmap) <- buttonW "bt-delete" gsmap
    
    -- build the FRP wires
    
    let addNameW = mkFix (\t names -> Right (names ++ [Name "New Entry" "Edit me!"]))
    let delNamesW = mkFix (\t (names, selection) -> Right ( fmap fst $ filter (\(n, i) -> not (elem i selection)) (zip names [0..]) ))
        
    
    -- main wire to process crud element
        
    let w1 = proc (names, selection, filtertxt) -> do             -- all state is kept in entries and filtertext
          
          -- check for changes
          fchange <- isJust <$> (event changed) -< filtertxt
          nchange <- isJust <$> (event changed) -< names
          schange <- isJust <$> (event changed) -< selection 
          
          (names', selection', filtertxt') <-
                do
                         -- handle multiselect element
                         selection' <- entrieslist -<  onChangeJust (nchange || fchange || schange) (makeEntries names selection filtertxt)
                         returnA -< (names, selection', filtertxt)
                   <|> do
                         -- create a new entry
                         names' <- addNameW . create -< names
                         returnA -< (names', selection, filtertxt)
                   <|> do
                         -- delete entries which are selected
                         names' <- delNamesW . delete -< (names, selection)
                         returnA -< (names', [], filtertxt)
                   <|> do
                         -- check filtertxt
                         filtertxt' <- prefix -< Nothing
                         returnA -< (names, selection, filtertxt')
                   <|> do
                         -- check prename 
                         pn <- prenameTxt -< onChangeJust (nchange || fchange || schange) (if not (null selection) then head $ toListOf (element (head selection) . preName) names else "")
                         let names' = if not (null selection) then
                                            (element (head selection) . preName) .~ pn $ names
                                          else
                                            names
                         returnA -< (names', selection, filtertxt)
                   <|> do
                         -- check surname
                         sn <- surnameTxt -< onChangeJust (nchange || fchange || schange) (if not (null selection) then  head $ toListOf (element (head selection) . surName) names else "")
                         let names' = if not (null selection) then
                                            (element (head selection) . surName) .~ sn $ names
                                          else
                                            names
                         returnA -< (names', selection, filtertxt)
                   <|> do
                         returnA -< (names, selection, filtertxt)
          returnA -< (names', selection', filtertxt')

    let w2 = proc _ -> do
                       rec
                         (names, selection, filtertxt) <- delay ([]::Names, []::Selection, ""::String) -< (names', selection', filtertxt')
                         (names', selection', filtertxt') <- w1 -< (names, selection, filtertxt)
                       returnA -< ()
    
    
    let theWire = w2
    
    
    -- run the webserver, the netwire loop and wait for termination   
    runHWebUI port gsmap guiLayout theWire
    
    return ()
    
    

