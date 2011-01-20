{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances, UndecidableInstances #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Monad.ST.Class
-- Copyright   :  (C) 2011 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  MPTCs, fundeps
--
----------------------------------------------------------------------------
module Control.Monad.ST.Class (MonadST(..)) where

import Control.Monad.Trans.Class
import Control.Monad.ST

class Monad m => MonadST s m | m -> s where
  liftST :: ST s a -> m a
  
instance MonadST RealWorld IO where
  liftST = stToIO

instance MonadST s (ST s) where
  liftST = id

instance (MonadTrans t, MonadST s m, Monad (t m)) => MonadST s (t m) where
  liftST = lift . liftST
