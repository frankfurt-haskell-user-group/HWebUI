{-# LANGUAGE Rank2Types, TypeSynonymInstances, FlexibleInstances, ExistentialQuantification, ScopedTypeVariables #-}
module WidgetWires where

import Server
import Wires
import Widgets
import FreshId
import Properties

class PropertyWidget t

instance PropertyWidget ([Property c] -> Widget)

instance PropertyWidget Widget

data WidgetWire a b t = PropertyWidget t => WidgetWire t (ChannelStateGUIWire a b)

type NamedWidgetWire a b t= SupplyVars (WidgetWire a b t)

giveAnId :: PropertyWidget t => (String -> t) 
        -> (String -> ChannelStateGUIWire a b) 
        -> NamedWidgetWire a b t 
giveAnId wWidget wireW = do
        ident <- supply
        return $ WidgetWire (wWidget ident) (wireW ident)

wwButton :: NamedWidgetWire a a ([Property Button] -> Widget)
wwButton = giveAnId wButton buttonW

wwHtml :: NamedWidgetWire (Maybe String) String Widget
wwHtml = giveAnId wHtml htmlW

wwTextBox :: NamedWidgetWire (Maybe String) String ([Property TextBox] -> Widget)
wwTextBox = giveAnId wTextBox textBoxW
 
wwRadioButton :: NamedWidgetWire (Maybe Bool) Bool ([Property RadioButton] -> Widget)
wwRadioButton = giveAnId wRadioButton radioButtonW
 
wwMultiSelect :: NamedWidgetWire (Maybe [(String, Bool, a)]) [a] ([Property MultiSelect] -> Widget)
wwMultiSelect = giveAnId wMultiSelect multiSelectW