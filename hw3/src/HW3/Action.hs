{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DerivingVia                #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs               #-}

module HW3.Action
  ( HIO (..)
  , HiPermission (..)
  , PermissionException (..)
  ) where

import Control.Exception.Base (Exception, throwIO)
import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (MonadReader, ReaderT (..), ask)

import HW3.Base (HiAction (..), HiMonad (..), HiValue (..))
import HW3.Evaluator (returnHiList, toHiNumber)

import qualified Data.ByteString as BS
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Text.Encoding as Encoding
import qualified Data.Time as Time
import qualified System.Directory as Dir
import qualified System.Random as Rand


data HiPermission
  = AllowRead
  | AllowWrite
  | AllowTime
  deriving (Show, Eq, Ord)


data PermissionException
  = PermissionRequired HiPermission
  deriving (Show, Exception)


-- | Extension @'deriving via'@ helps to:
-- 1. Inherit implementation from ReaderT (this is HIO, in fact)
-- 2. Become MonadReader so we can use it's base functions like ask
newtype HIO a = HIO { runHIO :: Set.Set HiPermission -> IO a }
  deriving (Functor, Applicative, Monad, MonadIO)
    via (ReaderT (Set.Set HiPermission) IO)
  deriving (MonadReader (Set.Set HiPermission))
    via (ReaderT (Set.Set HiPermission) IO)


instance HiMonad HIO where
  runAction :: HiAction -> HIO HiValue

  runAction (HiActionRead fp) = do
    checkPermission AllowRead
    isFile <- liftIO $ Dir.doesFileExist fp
    liftIO $ if isFile
    then do
      bs <- BS.readFile fp
      return $ either (const $ HiValueBytes bs) HiValueString (Encoding.decodeUtf8' bs)
    else do
      entries <- Dir.listDirectory fp
      return $ returnHiList $ fmap filePathToHiString entries

  runAction (HiActionWrite fp bs) = simpleVoidAction AllowWrite (BS.writeFile fp bs)

  runAction (HiActionMkDir fp) = simpleVoidAction AllowWrite (Dir.createDirectoryIfMissing True fp)

  runAction (HiActionChDir fp) = simpleVoidAction AllowRead (Dir.setCurrentDirectory fp)

  runAction HiActionCwd = simpleAction filePathToHiString AllowRead Dir.getCurrentDirectory

  runAction HiActionNow = simpleAction HiValueTime AllowTime Time.getCurrentTime

  runAction (HiActionRand l r) = do
    g <- Rand.getStdGen
    let (n, g') = Rand.uniformR (l, r) g
    Rand.setStdGen g'
    return $ toHiNumber n

  runAction (HiActionEcho s) = simpleVoidAction AllowWrite (putStrLn $ T.unpack s)


-- | Helpful methods for working with actions


filePathToHiString :: FilePath -> HiValue
filePathToHiString = HiValueString . T.pack


-- | @'simpleAction'@ just checks permission, perform single action and then wraps the result
simpleAction :: (a -> HiValue) -> HiPermission -> IO a -> HIO HiValue
simpleAction wrapper perm action = checkPermission perm >> liftIO action >>= (return . wrapper)


-- | Version of @'simpleAction'@ when we don't need any result
simpleVoidAction :: HiPermission -> IO () -> HIO HiValue
simpleVoidAction = simpleAction (const HiValueNull)


checkPermission :: HiPermission -> HIO ()
checkPermission p = do { ps <- ask; liftIO $ when (Set.notMember p ps) (throwIO $ PermissionRequired p) }
