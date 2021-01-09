{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Abbot.Monad
  ( module Abbot.Monad
  , module Control.Monad.State
  , Control.Monad.Catch.catchIOError
  ) where

-- Note that Control.Monad.State (from mtl) re-exports Control.Monad.Trans
-- (which provides both lift and liftIO).

import           Control.Monad.Catch
import           Control.Monad.IO.Class
import           Control.Monad.State
import           Control.Monad.Trans.Class      ( lift )
import           System.Console.Haskeline       ( InputT )

instance MonadState s m => MonadState s (InputT m) where
  get = lift get
  put = lift . put
