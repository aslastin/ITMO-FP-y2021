{-# LANGUAGE BangPatterns #-}

module HW0.T4
  ( fac
  , fib
  , map'
  , repeat'
  ) where

import Data.Function (fix)
import GHC.Natural (Natural)


repeat' :: a -> [a]
repeat' x = fix $ \f -> x : f

map' :: (a -> b) -> [a] -> [b]
map' g = fix $ \f list ->
  case list of
    []     -> []
    (x:xs) -> g x : f xs

fib :: Natural -> Natural
fib = fix (\f !a b n -> if n == 0 then a else f b (a + b) (n - 1)) 0 1

fac :: Natural -> Natural
fac = fix (\f !acc n -> if n == 0 then acc else f (acc * n) (n - 1)) 1
