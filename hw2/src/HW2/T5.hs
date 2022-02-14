module HW2.T5
  ( -- * Datatypes
    EvaluationError (..)
  , ExceptState (..)
    -- * map functions
  , eval
  , evalExceptState
  , getExceptState
  , joinExceptState
  , mapExceptState
  , modifyExceptState
  , throwExceptState
  , wrapExceptState
  ) where

import qualified Control.Monad
import HW2.T1 (Annotated ((:#)), Except (Error, Success), mapExcept)
import HW2.T2 (wrapExcept)
import HW2.T3 (joinExcept)
import HW2.T4 (BinaryOperation, BinaryPrimConstructor, Expr (Op, Val),
               Prim (Abs, Add, Div, Mul, Sgn, Sub), UnaryOperation, UnaryPrimConstructor, joinState,
               mapState, modifyState, runS, wrapState)

data ExceptState e s a = ES { runES :: s -> Except e (Annotated s a) }

mapExceptState :: (a -> b) -> ExceptState e s a -> ExceptState e s b
mapExceptState f eState = ES $ \s -> mapExcept (\(a :# s') -> (f a) :# s') (runES eState s)

wrapExceptState :: a -> ExceptState e s a
wrapExceptState a = ES $ \s -> Success (a :# s)

joinExceptState :: ExceptState e s (ExceptState e s a) -> ExceptState e s a
joinExceptState eState = ES $ \s -> joinExcept $ mapExcept (\(eState' :# s') -> runES eState' s') $ runES eState s

modifyExceptState :: (s -> s) -> ExceptState e s ()
modifyExceptState f = ES $ \s -> Success (() :# (f s))

throwExceptState :: e -> ExceptState e s a
throwExceptState e = ES $ \_ -> Error e

-- | For header function

getExceptState :: ExceptState e s s
getExceptState = ES $ \s -> Success $ (s :# s)

instance Functor (ExceptState e s) where
  fmap = mapExceptState

instance Applicative (ExceptState e s) where
  pure = wrapExceptState
  p <*> q = Control.Monad.ap p q

instance Monad (ExceptState e s) where
   m >>= f = joinExceptState (fmap f m)

data EvaluationError = DivideByZero

eval :: Expr -> ExceptState EvaluationError [Prim Double] Double
eval (Val x) = wrapExceptState x
eval (Op (Div x y)) = do
  xVal <- eval x
  yVal <- eval y
  if (yVal == 0)
  then throwExceptState DivideByZero
  else do
    modifyExceptState ((Div xVal yVal):)
    wrapExceptState $ xVal / yVal
eval (Op op) = evalOp op

evalOp :: Prim Expr -> ExceptState EvaluationError [Prim Double] Double
evalOp (Add x y) = binaryEval (+) Add x y
evalOp (Sub x y) = binaryEval (-) Sub x y
evalOp (Mul x y) = binaryEval (*) Mul x y
evalOp (Div x y) = binaryEval (/) Div x y
evalOp (Abs x)   = unaryEval abs Abs x
evalOp (Sgn x)   = unaryEval signum Sgn x

-- | For evaluation

binaryEval :: BinaryOperation -> BinaryPrimConstructor -> Expr -> Expr -> ExceptState EvaluationError [Prim Double] Double
binaryEval f prim expr1 expr2 = do
  x <- eval expr1
  y <- eval expr2
  modifyExceptState ((prim x y):)
  wrapExceptState $ f x y

unaryEval ::UnaryOperation -> UnaryPrimConstructor -> Expr -> ExceptState EvaluationError [Prim Double] Double
unaryEval f prim expr = do
  x <- eval expr
  modifyExceptState ((prim x):)
  wrapExceptState $ f x

-- | For import

evalExceptState :: Expr -> ExceptState EvaluationError [Prim Double] Double
evalExceptState = eval
