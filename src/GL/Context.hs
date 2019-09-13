{-# LANGUAGE FlexibleContexts, RankNTypes #-}

module GL.Context
  ( module GL.Context
  )
where

import           Control.Monad.State
import           Control.Monad.Reader
import           Control.Monad.Except
import           GL.Utils

ctxRaise :: MonadState [[c]] m => m a -> m a
ctxRaise = ctxRaiseAdd []

ctxAdd :: MonadState [[c]] m => c -> m ()
ctxAdd c = modify (\(x : xs) -> (c : x) : xs)

ctxRaiseAdd :: MonadState [[c]] m => [c] -> m a -> m a
ctxRaiseAdd xs m = modify (xs :) *> m <* modify tail

single :: (MonadError String m) => m [t] -> m t
single m = liftEither =<< (onlyEither "A search failed" <$> m)
