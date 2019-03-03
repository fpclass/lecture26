--------------------------------------------------------------------------------
-- Functional Programming (CS141)                                             --
-- Lecture 24: Type promotion & GADTs                                         --
--------------------------------------------------------------------------------

{-# LANGUAGE DataKinds, GADTs, KindSignatures, ScopedTypeVariables #-}

-- | This module implements natural numbers and vectors (lists which are
-- indexed by their length on the type-level)
module Vector where

--------------------------------------------------------------------------------
-- Natural numbers

data Nat = Zero | Succ Nat

--------------------------------------------------------------------------------
-- Vectors

data Vector (n :: Nat) a where
    Nil  :: Vector Zero a
    Cons :: a -> Vector n a -> Vector (Succ n) a

vhead :: Vector (Succ n) a -> a
vhead (Cons x xs) = x

vzip :: Vector n a -> Vector n b -> Vector n (a,b)
vzip Nil         Nil         = Nil
vzip (Cons x xs) (Cons y ys) = Cons (x,y) (vzip xs ys)

--------------------------------------------------------------------------------
-- Singletons

data SNat (n :: Nat) where
    SZero :: SNat Zero
    SSucc :: SNat n -> SNat (Succ n)

vreplicate :: SNat n -> a -> Vector n a
vreplicate SZero     x = Nil
vreplicate (SSucc n) x = Cons x (vreplicate n x)

vlength :: Vector n a -> Int
vlength Nil         = 0
vlength (Cons _ xs) = 1 + vlength xs

--------------------------------------------------------------------------------
-- Proxies & reification  

data NatProxy (a :: Nat) = MkProxy

zeroProxy :: NatProxy Zero
zeroProxy = MkProxy

oneProxy :: NatProxy (Succ Zero)
oneProxy = MkProxy

class FromNat (n :: Nat) where
    fromNat :: NatProxy n -> Int

instance FromNat Zero where
    fromNat _ = 0

instance FromNat n => FromNat (Succ n) where
    fromNat _ = 1 + fromNat (MkProxy :: NatProxy n)

vlength' :: forall n a . FromNat n => Vector n a -> Int
vlength' _ = fromNat (MkProxy :: NatProxy n)

--------------------------------------------------------------------------------
