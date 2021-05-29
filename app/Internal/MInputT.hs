{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

module Internal.MInputT
  ( MInputT(..)
  , mRunInputT
  , mWithInterrupt
  , mHandleInterrupt
  , mGetInputLine
  , mGetInputChar
  , mOutputStr
  , mOutputStrLn
  , HL.defaultSettings
  ) where

import           Control.Monad.Catch            ( MonadCatch
                                                , MonadMask
                                                , MonadThrow
                                                , catchIOError
                                                )
import           Control.Monad.IO.Class         ( MonadIO(..) )
import           Control.Monad.State            ( MonadState(..) )
import           Control.Monad.Trans.Class      ( MonadTrans(..) )
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import           System.Console.Haskeline      as HL
                                                ( InputT
                                                , Settings
                                                , defaultSettings
                                                , getInputChar
                                                , getInputLine
                                                , handleInterrupt
                                                , outputStr
                                                , outputStrLn
                                                , runInputT
                                                , withInterrupt
                                                )

-- InputT doesn't provide instances of MTL classes, so we need to do it ourselves
-- The approach is taken from the (no longer working) haskeline-class package, which
-- can be found at https://hackage.haskell.org/package/haskeline-class
newtype MInputT m a = MInputT {unMInputT :: HL.InputT m a}
  deriving (Functor, Applicative, Monad, MonadTrans, MonadIO, MonadThrow, MonadCatch)

instance MonadState s m => MonadState s (MInputT m) where
  get   = lift get
  put   = lift . put
  state = lift . state

mRunInputT :: (MonadIO m, MonadMask m) => HL.Settings m -> MInputT m a -> m a
mRunInputT s m = HL.runInputT s (unMInputT m)

mWithInterrupt :: (MonadIO m, MonadMask m) => MInputT m a -> MInputT m a
mWithInterrupt = MInputT . HL.withInterrupt . unMInputT

mHandleInterrupt
  :: (MonadIO m, MonadMask m) => MInputT m a -> MInputT m a -> MInputT m a
mHandleInterrupt catchA tryA =
  MInputT $ HL.handleInterrupt (unMInputT catchA) (unMInputT tryA)

mGetInputLine :: (MonadIO m, MonadMask m) => Text -> MInputT m (Maybe Text)
mGetInputLine = (fmap . fmap $ T.pack) . MInputT . HL.getInputLine . T.unpack

mGetInputChar :: (MonadIO m, MonadMask m) => Text -> MInputT m (Maybe Char)
mGetInputChar = MInputT . HL.getInputChar . T.unpack

mOutputStr :: MonadIO m => Text -> MInputT m ()
mOutputStr = MInputT . HL.outputStr . T.unpack

mOutputStrLn :: MonadIO m => Text -> MInputT m ()
mOutputStrLn = MInputT . HL.outputStrLn . T.unpack
