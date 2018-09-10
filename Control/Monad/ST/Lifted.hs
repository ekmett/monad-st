-- | Lifted version of functions from "Data.STRef".
{-# OPTIONS_GHC -Wall #-}
module Control.Monad.ST.Lifted
  ( newSTRef
  , readSTRef
  , writeSTRef
  , modifySTRef
  , modifySTRef'
  ) where

import Control.Monad.ST.Class (MonadST(..))
import Data.STRef (STRef)
import qualified Data.STRef as ST

-- | Lifted version of 'ST.newSTRef'.
newSTRef :: MonadST m => a -> m (STRef (World m) a)
newSTRef = liftST . ST.newSTRef

-- | Lifted version of 'ST.readSTRef'.
readSTRef :: MonadST m => STRef (World m) a -> m a
readSTRef = liftST . ST.readSTRef

-- | Lifted version of 'ST.writeSTRef'.
writeSTRef :: MonadST m => STRef (World m) a -> a -> m ()
writeSTRef r = liftST . ST.writeSTRef r

-- | Lifted version of 'ST.modifySTRef'.
modifySTRef :: MonadST m => STRef (World m) a -> (a -> a) -> m ()
modifySTRef r = liftST . ST.modifySTRef r

-- | Lifted version of 'ST.modifySTRef'.
modifySTRef' :: MonadST m => STRef (World m) a -> (a -> a) -> m ()
modifySTRef' r = liftST . ST.modifySTRef' r
