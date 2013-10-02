{-# LANGUAGE FunctionalDependencies, FlexibleInstances, GeneralizedNewtypeDeriving #-}

-- see http://www.haskell.org/haskellwiki/New_monads/MonadSupply
module FreshId 
    (MonadSupply,
     supply,
     SupplyVars,
     evalSupplyVars)
    where

import Control.Monad.State

type SupplyVars = Supply String

newtype Supply s a = Supply (State [s] a)
        deriving (Functor, Monad)
 
class Monad m => MonadSupply s m | m -> s where
    supply :: m s
         
instance MonadSupply s (Supply s) where
    supply = Supply $ do
                (x:xs) <- get
                put xs
                return x

evalSupply :: Supply t a -> [t] ->  a
evalSupply (Supply s) supp = evalState s supp
 
evalSupplyVars :: SupplyVars a -> a
evalSupplyVars x = evalSupply x vars
    where vars = [replicate k ['a'..'z'] | k <- [1..]] >>= sequence