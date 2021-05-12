-- | This module simply re-exports a bunch of Monad (and Applicative) stuff that
-- we want to use.

module Internal.Monad
  ( Control.Monad.IO.Class.liftIO
  , Control.Monad.Except.ExceptT(..)
  , Control.Monad.Except.runExceptT
  , Control.Monad.Except.MonadError(..)
  , Control.Monad.when
  , Control.Monad.unless
  , Control.Monad.forM
  , Control.Monad.forM_
  , Control.Monad.void
  , Control.Monad.replicateM
  , Control.Monad.replicateM_
  , Control.Monad.filterM
  , (<|>)
  ) where

-- base
import           Control.Applicative
import           Control.Monad
import           Control.Monad.Except
-- mtl
import           Control.Monad.IO.Class
