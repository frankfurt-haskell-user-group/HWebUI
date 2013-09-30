{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, ExistentialQuantification, ScopedTypeVariables #-}

{- | Properties is an internal implementation module of "HWebUI". "HWebUI" is providing FRP-based GUI functionality for Haskell by utilizing the Web-Browser. See module "HWebUI" for main documentation. 
-}
{-# OPTIONS_HADDOCK hide #-}

module Properties (
  
  Attribute (..),
  Property (..),
  
  HasLabel,
  HasStyle,
  HasSize,
  HasName,
  HasValue,
  HasChecked,
  style,
  height,
  width,
  label,
  name,
  value,
  checked,
  
  parasToJS
  
  ) where

import Data.List

data  Attribute w a = Attr String (String -> a -> String)
data Property w = forall a. Attribute w a := a

propToJS :: Property w -> String
propToJS ( (:=) (Attr name toJS) value) = toJS name value

class HasLabel w where
  label :: Attribute w String
  label = Attr "label" stringPairToJS

class HasStyle w where
  style :: Attribute w String
  style = Attr "style" stringPairToJS

class HasName w where
  name :: Attribute w String
  name = Attr "name" stringPairToJS

class HasValue w where
  value :: Attribute w String
  value = Attr "value" stringPairToJS

class HasChecked w where
  checked :: Attribute w Bool
  checked = Attr "checked" boolPairToJS

class HasSize w where
  width :: Attribute w Int
  height :: Attribute w Int
  width = Attr "width" intPairToJS
  height = Attr "height" intPairToJS

intPairToJS :: String -> Int -> String
intPairToJS s i =  s ++ ": "  ++ (show i)
floatPairToJS :: String -> Float -> String
floatPairToJS s f = s ++ ": "  ++ (show f)
boolPairToJS :: String -> Bool -> String
boolPairToJS s b = s ++ ": " ++ (if b then "true" else "false")
stringPairToJS :: String -> String -> String
stringPairToJS s1 s2 = s1 ++ ": \"" ++ s2 ++ "\""
  
parasToJS :: [Property w] -> String
parasToJS pl = foldl (++) "" $ (\l -> if null l then l else l ++ [","])  $ intersperse "," $ map propToJS pl
