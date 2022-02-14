module HW0.T6 where

import Data.Char (isSpace)
import HW0.T1


a = distrib (Left ("AB" ++ "CD" ++ "EF"))
b = map isSpace "Hello, World"
c = if 1 > 0 || error "X" then "Y" else "Z"

a_whnf = (Left ("AB" ++ "CD" ++ "EF"), Left ("AB" ++ "CD" ++ "EF"))
b_whnf = (isSpace 'H') : map isSpace "ello, World"
c_whnf = "Y"
