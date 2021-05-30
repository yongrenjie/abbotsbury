-- | This module simply re-exports a bunch of Monad (and Applicative) stuff that
-- we want to use.

module Internal.Monad
  ( Control.Monad.IO.Class.MonadIO(..)
  , Control.Monad.Except.ExceptT(..)
  , Control.Monad.Except.runExceptT
  , Control.Monad.Except.MonadError(..)
  , Control.Monad.Trans.Class.lift
  , Control.Monad.guard
  , Control.Monad.when
  , Control.Monad.unless
  , Control.Monad.forM
  , Control.Monad.forM_
  , Control.Monad.void
  , Control.Monad.replicateM
  , Control.Monad.replicateM_
  , Control.Monad.filterM
  , Data.Either.isLeft
  , Data.Either.isRight
  , Data.Maybe.isJust
  , Data.Maybe.isNothing
  , (<|>)
  ) where

-- base
import           Control.Applicative
import           Control.Monad
import           Control.Monad.Except
import           Data.Either
import           Data.Maybe
-- mtl
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Class
