{-# LANGUAGE BlockArguments             #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module HW2.T6
   ( -- * Datatypes
     ParseError (..)
   , Parser (..)
     -- * functions
   , pChar
   , pEof
   , parseError
   , parseExpr
   , runP
   )
where

import Control.Applicative (Alternative (..), Applicative (..))
import Control.Monad (MonadPlus, mfilter, msum, void)
import GHC.Natural (Natural)
import HW2.T1 (Annotated ((:#)), Except (Error, Success), mapExcept)
import HW2.T3 (joinExcept)
import HW2.T4 (Expr (Op, Val), Prim (Add, Div, Mul, Sub))
import HW2.T5 (ExceptState (ES, runES), getExceptState, wrapExceptState, throwExceptState)

newtype ParseError = ErrorAtPos Natural

newtype Parser a = P (ExceptState ParseError (Natural, String) a)
   deriving newtype (Functor, Applicative, Monad)


runP :: Parser a -> String -> Except ParseError a
runP (P eState) input = mapExcept (\(res :# _) -> res) $ runES (f eState) (0, input)
  where
    f :: ExceptState ParseError (Natural, String) a -> ExceptState ParseError (Natural, String) a
    f m = do
      res <- m
      (_, rest) <- getExceptState
      if rest == ""
      then wrapExceptState res
      else do f m


pChar :: Parser Char
pChar = P $ ES \(pos, s) ->
  case s of
    []     -> Error (ErrorAtPos pos)
    (c:cs) -> Success (c :# (pos + 1, cs))

-- | What happens when the string is empty?
-- We return an error because there is no valid expression to reveice from runP

-- | How does the parser state change when a character is consumed?
-- a :# (pos, rest):
-- a - contains parsed and transformed expression
-- pos - amount of read symbols
-- res - rest of line to parse

parseError :: Parser a
parseError = undefined

instance Alternative Parser where
  empty = parseError
  (<|>) = undefined

instance MonadPlus Parser   -- No methods.

pEof :: Parser ()
pEof = undefined

parseExpr :: String -> Except ParseError Expr
parseExpr = undefined
