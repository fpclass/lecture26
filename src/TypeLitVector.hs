--------------------------------------------------------------------------------
-- Functional Programming (CS141)                                             --
-- Lecture 24: Type promotion & GADTs                                         --
--------------------------------------------------------------------------------

{-# LANGUAGE DataKinds, GADTs, KindSignatures, TypeOperators #-}

-- | This module is the same as Vector, except we use GHC's own defintiion
-- of type-level natural numbers instead of our own so that we get nice
-- syntactic sugar for them.
module TypeLitVector where

--------------------------------------------------------------------------------

import GHC.TypeLits

--------------------------------------------------------------------------------
-- Vectors using Nat from GHC.TypeLits

data Vector (n :: Nat) a where
    Nil  :: Vector 0 a
    Cons :: a -> Vector n a -> Vector (n+1) a

vhead :: Vector (n+1) a -> a
vhead (Cons x xs) = x

cakemix :: Vector 2 String
cakemix = Cons "Fish-shaped rhubarb" (Cons "4 large eggs" Nil)

--------------------------------------------------------------------------------
