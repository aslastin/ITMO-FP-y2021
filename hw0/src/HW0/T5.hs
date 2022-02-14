{-# LANGUAGE BangPatterns #-}

module HW0.T5
  ( Nat
  , nFromNatural
  , nToNum
  , nmult
  , nplus
  , ns
  , nz
  ) where

import GHC.Natural (Natural)


type Nat a = (a -> a) -> a -> a

nz :: Nat a
nz _ x = x

ns :: Nat a -> Nat a
ns n f = n f . f

nplus, nmult :: Nat a -> Nat a -> Nat a
nplus a b f = b f . a f
nmult a b f = a $ nplus b nz f

nFromNatural :: Natural -> Nat a
nFromNatural 0 _  = id
nFromNatural !n f = f . nFromNatural (n - 1) f

nToNum :: Num a => Nat a -> a
nToNum n = n (+1) 0
