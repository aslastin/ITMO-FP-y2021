module HW1.T2
  ( N(..)
  , nToNum
  , nFromNatural
  , nplus
  , nmult
  , nsub
  , ncmp
  , nEven
  , nOdd
  , ndiv
  , nmod
  ) where

import Numeric.Natural as Natural    


data N = Z | S N

nToNum :: Num a => N -> a
nToNum n = nToNum' n 0

nToNum' :: Num a => N -> a -> a
nToNum' Z i = i 
nToNum' (S n) i = nToNum' n (i + 1)


nFromNatural :: Natural -> N
nFromNatural x = nFromNatural' x Z

nFromNatural' :: Natural -> N -> N
nFromNatural' 0 acc = acc
nFromNatural' x acc = nFromNatural' (x - 1) (S acc)


nplus :: N -> N -> N
nplus a Z = a
nplus a (S b) = nplus (S a) b


nmult :: N -> N -> N
nmult Z _ = Z
nmult a b = nmult' a b a

nmult' :: N -> N -> N -> N
nmult' _ Z _ = Z
nmult' _ (S Z) acc = acc
nmult' a (S b) acc = nmult' a b (nplus acc a) 


nsub :: N -> N -> Maybe N
nsub Z (S b) = Nothing
nsub a Z = Just a
nsub (S a) (S b) = nsub a b


ncmp :: N -> N -> Ordering
ncmp a b = case nsub a b of
  Nothing -> LT
  Just Z -> EQ
  Just _ -> GT

-- Advanced

nEven, nOdd :: N -> Bool
nEven Z = True
nEven (S Z) = False
nEven (S (S n)) = nEven n

nOdd = not . nEven


ndiv :: N -> N -> N
ndiv a b = ndiv' (nsub a b) b Z

ndiv' :: Maybe N -> N -> N -> N
ndiv' Nothing _ acc = acc
ndiv' (Just a) b acc = ndiv' (nsub a b) b (S acc)


nmod :: N -> N -> N
nmod a b = nmod' (nsub a b) b a

nmod' :: Maybe N -> N -> N -> N
nmod' Nothing _ prev = prev
nmod' (Just a) b _ = nmod' (nsub a b) b a
