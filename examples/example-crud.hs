{-# LANGUAGE TemplateHaskell, QuasiQuotes, OverloadedStrings, TypeFamilies, MultiParamTypeClasses, Arrows #-}
module Main where

import Yesod
import Control.Wire
import Prelude hiding ((.), id)

import HWebUI

import Data.Maybe
import Data.List hiding (delete)
import Control.Lens

-- a double conversion function
atof :: String -> Double
atof instr = case reads instr of
     [] -> 0.0
     [(f, x)] -> f
      
                 
-- data and functionality, to modify data, also the filter feature needs to be considered
-- therfore, we need two pieces of data, one for keeping the global state, this is entries  
-- and one for keeping the filtered state, which is after applying the filter and adding index into allEntries (!), this is fiEntries
     

-- basic names, this is the main data
     
data Person = Person { _preName :: String  -- Name
                     , _surName :: String  -- Surname
                     , _descr :: String    -- Description
                     }
    deriving (Eq, Show)

makeLenses ''Person

type Persons = [Person]

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

makeEntries :: Persons -> Selection -> String -> Entries
makeEntries persons selection filtertxt = filter ffilter $ fmap fentry (zip persons [0..]) where
  ffilter e =  filtertxt `isInfixOf` (e ^. guiText)
  fentry (person, i) = ((person ^. preName) ++ " " ++ (person ^. surName) ++ " " ++ (person ^. descr), i `elem` selection, i)

onChangeJust :: Bool -> a -> Maybe a
onChangeJust b work = if b then Just work else Nothing
          
guiDefinition = do

    -- define gui elements
    textBoxFilterPrefix <- hwuTextBox []
    textBoxPrename <- hwuTextBox []
    textBoxSurname <- hwuTextBox []
    textareaDescr <- hwuTextarea []
    
    multiSelectEntries <- hwuMultiSelect [width := 350]
    
    buttonCreate <- hwuButton [label := "Create Entry"]
    buttonDelete <- hwuButton [label := "Delete Entry"]
        
    -- define layout 
        
    let guiLayout = do    
        
        -- a table with the entry fields (as text) the operator and the result
        [whamlet|
              <H1>HWebUI - CRUD Example
              <p>
                    |]
        
        [whamlet|
         <table>
           <tr>
             <td>Filter Prefix:
             <td>^{hwuLayout textBoxFilterPrefix}
             <td>Name:
             <td>^{hwuLayout textBoxPrename}
           <tr>        
             <td>
             <td>
             <td>Surname:
             <td>^{hwuLayout textBoxSurname}
         ^{hwuLayout textareaDescr}
         ^{hwuLayout multiSelectEntries}
         <table>    
           <tr>    
             <td>^{hwuLayout buttonCreate}
             <td>^{hwuLayout buttonDelete}
         |]


    -- define functionality 
        
       
    let prefix = hwuWire textBoxFilterPrefix 
    let prenameTxt = hwuWire textBoxPrename 
    let surnameTxt = hwuWire textBoxSurname
    let descrTxt = hwuWire textareaDescr
    let create = hwuWire buttonCreate
    let entrieslist = hwuWire multiSelectEntries
    let delete = hwuWire buttonDelete
    
        -- build the FRP wires
    let addNameW = mkFix (\_ names -> Right (names ++ [Person "New Entry" "Edit me!" "For fun!"]))
    let delNamesW = mkFix (\_ (names, selection) -> Right (fst <$> filter (\(_, i) ->  i `notElem` selection) (zip names [0..]) ))

        -- main wire to process crud element
    let w1 = proc (names, selection, filtertxt) -> do             -- all state is kept in entries and filtertext
                -- check for changes
                fchange <- isJust <$> event changed -< filtertxt
                nchange <- isJust <$> event changed -< names
                schange <- isJust <$> event changed -< selection 
          
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
                                -- check description
                                description <- descrTxt -< onChangeJust (nchange || fchange || schange) (if not (null selection) then  head $ toListOf (element (head selection) . descr) names else "")
                                let names' = if not (null selection) then
                                            (element (head selection) . descr) .~ description $ names
                                          else
                                            names
                                returnA -< (names', selection, filtertxt)
                        <|> do
                                returnA -< (names, selection, filtertxt)
                returnA -< (names', selection', filtertxt')

    let w2 = proc _ -> do
                       rec
                         (names, selection, filtertxt) <- delay ([]::Persons, []::Selection, ""::String) -< (names', selection', filtertxt')
                         (names', selection', filtertxt') <- w1 -< (names, selection, filtertxt)
                       returnA -< ()
    
    return (guiLayout, w2)
    
main :: IO ()
main = do
         -- settings 
         let port = 8080
         -- run the webserver, the netwire loop and wait for termination         
         runHWebUI port guiDefinition 
