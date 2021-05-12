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

mGetInputLine :: (MonadIO m, MonadMask m) => String -> MInputT m (Maybe String)
mGetInputLine = MInputT . HL.getInputLine

mGetInputChar :: (MonadIO m, MonadMask m) => String -> MInputT m (Maybe Char)
mGetInputChar = MInputT . HL.getInputChar

mOutputStr :: MonadIO m => String -> MInputT m ()
mOutputStr = MInputT . HL.outputStr

mOutputStrLn :: MonadIO m => String -> MInputT m ()
mOutputStrLn = MInputT . HL.outputStrLn
