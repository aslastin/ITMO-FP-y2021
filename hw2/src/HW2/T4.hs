module HW2.T4
  ( -- * Datatypes
    Expr (..)
  , Prim (..)
  , State (..)
    -- * map functions
  , BinaryOperation
  , BinaryPrimConstructor
  , UnaryOperation
  , UnaryPrimConstructor
  , eval
  , evalState
  , joinState
  , mapState
  , modifyState
  , wrapState
  ) where

import qualified Control.Monad
import HW2.T1 (Annotated ((:#)))

type BinaryOperation = Double -> Double -> Double
type BinaryPrimConstructor = Double -> Double -> Prim Double

type UnaryOperation = Double -> Double
type UnaryPrimConstructor = Double -> Prim Double

data State s a = S { runS :: s -> Annotated s a }

mapState :: (a -> b) -> State s a -> State s b
mapState f state = S $ \s -> let (a :# s') = (runS state s) in (f a) :# s'

wrapState :: a -> State s a
wrapState a = S $ \s -> a :# s

joinState :: State s (State s a) -> State s a
joinState state = S $ \s -> let (state' :# s') = (runS state s) in (runS state' s')

modifyState :: (s -> s) -> State s ()
modifyState f = S $ \s -> () :# (f s)


instance Functor (State s) where
   fmap = mapState

instance Applicative (State s) where
   pure = wrapState
   p <*> q = Control.Monad.ap p q

instance Monad (State s) where
   m >>= f = joinState (fmap f m)


data Prim a =
     Add a a      -- ^ (+)
   | Sub a a      -- ^ (-)
   | Mul a a      -- ^ (*)
   | Div a a      -- ^ (/)
   | Abs a        -- ^ abs
   | Sgn a        -- ^ signum

data Expr =
   Val Double
   | Op (Prim Expr)

instance Num Expr where
   x + y = Op (Add x y)
   x - y = Op (Sub x y)
   x * y = Op (Mul x y)
   abs x = Op (Abs x)
   signum x = Op (Sgn x)

   fromInteger x = Val (fromInteger x)

instance Fractional Expr where
   x / y = Op (Div x y)

   fromRational x = Val (fromRational x)


eval :: Expr -> State [Prim Double] Double
eval (Val x) = wrapState x
eval (Op op) = evalOp op

evalOp :: Prim Expr -> State [Prim Double] Double
evalOp (Add x y) = binaryEval (+) Add x y
evalOp (Sub x y) = binaryEval (-) Sub x y
evalOp (Mul x y) = binaryEval (*) Mul x y
evalOp (Div x y) = binaryEval (/) Div x y
evalOp (Abs x)   = unaryEval abs Abs x
evalOp (Sgn x)   = unaryEval signum Sgn x

-- | For evaluation

binaryEval :: BinaryOperation -> BinaryPrimConstructor -> Expr -> Expr -> State [Prim Double] Double
binaryEval f prim expr1 expr2 = do
  x <- eval expr1
  y <- eval expr2
  modifyState ((prim x y):)
  wrapState $ f x y

unaryEval ::UnaryOperation -> UnaryPrimConstructor -> Expr -> State [Prim Double] Double
unaryEval f prim expr = do
  x <- eval expr
  modifyState ((prim x):)
  wrapState $ f x

-- | For import

evalState :: Expr -> State [Prim Double] Double
evalState = eval
