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

hwuButton :: [Property Button] -> WWMonad (WidgetWire a a)
hwuButton props = do
        elid <- freshId "hwuId"
        cmap <- getCMap
        let l = wButton elid props
        (w, nmap) <- liftIO $ buttonW elid cmap
        putCMap nmap
        return $ WidgetWire l w

hwuHtml :: [Property HtmlText] -> WWMonad (WidgetWire (Maybe String) String)
hwuHtml props = do
        elid <- freshId "hwuId"
        cmap <- getCMap
        let l = wHtml elid props
        (w, nmap) <- liftIO $ htmlW elid cmap
        putCMap nmap
        return $ WidgetWire l w

hwuTextBox :: [Property TextBox] -> WWMonad (WidgetWire (Maybe String) String)
hwuTextBox props = do
        elid <- freshId "hwuId"
        cmap <- getCMap
        let l = wTextBox elid props
        (w, nmap) <- liftIO $ textBoxW elid cmap
        putCMap nmap
        return $ WidgetWire l w

hwuTextarea :: [Property Textarea] -> WWMonad (WidgetWire (Maybe String) String)
hwuTextarea props = do
        elid <- freshId "hwuId"
        cmap <- getCMap
        let l = wTextarea elid props
        (w, nmap) <- liftIO $ textareaW elid cmap
        putCMap nmap
        return $ WidgetWire l w

hwuRadioButton :: [Property RadioButton] -> WWMonad (WidgetWire (Maybe Bool) Bool)
hwuRadioButton props = do
        elid <- freshId "hwuId"
        cmap <- getCMap
        let l = wRadioButton elid props
        (w, nmap) <- liftIO $ radioButtonW elid cmap
        putCMap nmap
        return $ WidgetWire l w

hwuMultiSelect :: [Property MultiSelect] -> WWMonad (WidgetWire (Maybe [(String, Bool, a)]) [a])
hwuMultiSelect props = do
        elid <- freshId "hwuId"
        cmap <- getCMap
        let l = wMultiSelect elid props
        (w, nmap) <- liftIO $ multiSelectW elid cmap
        putCMap nmap
        return $ WidgetWire l w

