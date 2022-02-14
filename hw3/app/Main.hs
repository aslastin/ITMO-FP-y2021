{-# LANGUAGE ViewPatterns #-}

module Main where

import System.Console.Haskeline

import Control.Monad (forever)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Maybe (MaybeT, runMaybeT)

import HW3.Action (HiPermission (..), runHIO)
import HW3.Base (HiExpr)
import HW3.Evaluator (eval)
import HW3.Parser (parse)
import HW3.Pretty (addColor, prettyValue)

import Prettyprinter (Doc, line, pretty)
import Prettyprinter.Render.Terminal.Internal (AnsiStyle, Color (Red), putDoc)

import Text.Megaparsec (errorBundlePretty)

import Data.Char (isSpace)

import qualified Data.Set as Set

-- | @'RIO'@ stands for Repl IO - all computations occur inside it.
-- Why I am using MaybeT here? We will see quite soon.
type RIO = InputT (MaybeT IO) ()

defaultPermissions :: Set.Set HiPermission
defaultPermissions = Set.fromList [AllowRead, AllowWrite, AllowTime]

main :: IO (Maybe ())
main = runRepl repl

runRepl :: RIO -> IO (Maybe ())
runRepl = (runMaybeT . runInputT defaultSettings)

-- | MaybeT comes into play here and now - it helps to avoid multiple nesting
-- inside @'repl'@ and nicely combines with ViewPatterns.
--
-- Moreover when pattern matching fails @'InputT'@ behave like normal Monad transformer,
-- i.e. call @'fail'@ implementation of it nested monad - MaybeT in our case - it returns Nothing,
-- and finally we escape from @'repl'@ and return to @'main'@.
--
-- Good job!
repl :: RIO
repl = forever $ do
  (Just (trim -> input)) <- getInputLine "hi> "
  if null input
  then return ()
  else either (printlnError . errorBundlePretty) processExpr $ parse input
  where
    trim :: String -> String
    trim = let g = reverse . dropWhile isSpace in g . g

processExpr :: HiExpr -> RIO
processExpr expr = do
  value <- liftIO $ runHIO (eval expr) defaultPermissions
  either (printlnError . show) (println . prettyValue) value

printlnError :: String -> RIO
printlnError = println . (addColor Red) . pretty

println :: Doc AnsiStyle -> RIO
println = liftIO . putDoc . (<> line <> line)
