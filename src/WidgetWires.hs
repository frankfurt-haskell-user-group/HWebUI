{-# LANGUAGE Rank2Types, TypeSynonymInstances, FlexibleInstances, ExistentialQuantification, ScopedTypeVariables #-}
module WidgetWires where

import Server
import Wires
import Widgets
import Properties

import Control.Monad.State (StateT, put, get)
import Control.Monad.Trans (liftIO)
import Data.Map (fromList)


data WidgetWire a b = WidgetWire {
        hwuLayout :: Widget,
        hwuWire :: GUIWire a b 
        }

data GuiState = GuiState {
        channelMap :: GSChannelMap,
        idCounter :: Int }
        
type WWMonad a = StateT GuiState IO a

initGuiState :: GuiState
initGuiState = GuiState ( (fromList [])::GSChannelMap ) 0

freshId :: String -> WWMonad String
freshId prefix = do
        gs <- get
        let idc = idCounter gs
        let cm = channelMap gs
        put (GuiState cm (idc + 1))
        return $ prefix ++ (show idc)

getCMap :: WWMonad GSChannelMap
getCMap = do
        gs <- get
        return (channelMap gs)
        
putCMap :: GSChannelMap -> WWMonad ()
putCMap cmap = do
        gs <- get
        put $ GuiState cmap (idCounter gs)
        return ()

updateState :: (String -> props -> Widget)
               -> (String -> GSChannelMap -> IO (GUIWire a b, GSChannelMap))
               -> props
               -> WWMonad (WidgetWire a b)
updateState widget wire props = do
        elid <- freshId "hwuId"
        cmap <- getCMap
        let l = widget elid props
        (w, nmap) <- liftIO $ wire elid cmap
        putCMap nmap
        return $ WidgetWire l w

hwuButton :: [Property Button] -> WWMonad (WidgetWire a a)
hwuButton = updateState wButton buttonW

hwuHtml :: [Property HtmlText] -> WWMonad (WidgetWire (Maybe String) String)
hwuHtml = updateState wHtml htmlW

hwuTextBox :: [Property TextBox] -> WWMonad (WidgetWire (Maybe String) String)
hwuTextBox = updateState wTextBox textBoxW

hwuTextarea :: [Property Textarea] -> WWMonad (WidgetWire (Maybe String) String)
hwuTextarea = updateState wTextarea textareaW

hwuRadioButton :: [Property RadioButton] -> WWMonad (WidgetWire (Maybe Bool) Bool)
hwuRadioButton = updateState wRadioButton radioButtonW

hwuMultiSelect :: [Property MultiSelect] -> WWMonad (WidgetWire (Maybe [(String, Bool, a)]) [a])
hwuMultiSelect = updateState wMultiSelect multiSelectW
