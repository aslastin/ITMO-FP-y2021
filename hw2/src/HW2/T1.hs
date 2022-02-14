module HW2.T1
  ( -- * Datatypes
    Annotated (..)
  , Except (..)
  , Fun (..)
  , List (..)
  , Option (..)
  , Pair (..)
  , Prioritised (..)
  , Quad (..)
  , Stream (..)
  , Tree (..)
    -- * map functions
  , mapAnnotated
  , mapExcept
  , mapFun
  , mapList
  , mapOption
  , mapPair
  , mapPrioritised
  , mapQuad
  , mapStream
  , mapTree
  ) where

import Prelude (($), (.))

data Option a = None | Some a

mapOption :: (a -> b) -> Option a -> Option b
mapOption _ None     = None
mapOption f (Some x) = Some $ f x


data Pair a = P a a

mapPair :: (a -> b) -> Pair a -> Pair b
mapPair f (P x1 x2) = P (f x1) (f x2)


data Quad a = Q a a a a

mapQuad :: (a -> b) -> Quad a -> Quad b
mapQuad f (Q x1 x2 x3 x4) = Q (f x1) (f x2) (f x3) (f x4)

data Annotated e a = a :# e
infix 0 :#

mapAnnotated :: (a -> b) -> Annotated e a -> Annotated e b
mapAnnotated f (x :# y) = f x :# y


data Except e a = Error e | Success a

mapExcept :: (a -> b) -> Except e a -> Except e b
mapExcept _ (Error x)   = Error x
mapExcept f (Success y) = Success $ f y


data Prioritised a = Low a | Medium a | High a

mapPrioritised :: (a -> b) -> Prioritised a -> Prioritised b
mapPrioritised f (Low x)    = Low $ f x
mapPrioritised f (Medium x) = Medium $ f x
mapPrioritised f (High x)   = High $ f x


data Stream a = a :> Stream a
infixr 5 :>

mapStream :: (a -> b) -> Stream a -> Stream b
mapStream f (h :> t) = f h :> mapStream f t


data List a = Nil | a :. List a
infixr 5 :.

mapList :: (a -> b) -> (List a -> List b)
mapList _ Nil      = Nil
mapList f (h :. t) = f h :. mapList f t


data Fun i a = F (i -> a)

mapFun :: (a -> b) -> (Fun i a -> Fun i b)
mapFun g (F f) = F $ g . f


data Tree a = Leaf | Branch (Tree a) a (Tree a)

mapTree :: (a -> b) -> (Tree a -> Tree b)
mapTree _ Leaf             = Leaf
mapTree f (Branch l val r) = Branch (mapTree f l) (f val) (mapTree f r)
