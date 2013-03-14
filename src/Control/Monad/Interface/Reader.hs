{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

{-|

This module exports:

    1. The 'MonadReader' type class and its operations 'reader', 'ask' and
    'local'.

    2. An instance of 'MonadReader' for the @->@ type.

    3. Instances of 'MonadReader' for the relevant monad transformers from the
    @transformers@ package (ReaderT', lazy 'L.RWST' and strict 'RWST').

    4. A universal pass-through instance of 'MonadReader' for any existing
    @MonadReader@ wrapped by a 'MonadLayer'.

    5. The utility operations 'asks'.

-}

module Control.Monad.Interface.Reader
    ( MonadReader (reader, ask, local)
    , asks
    )
where

-- base ----------------------------------------------------------------------
import           Control.Monad (liftM)
import           Control.Monad.Instances ()
import           Data.Monoid (Monoid (mempty))


-- transformers --------------------------------------------------------------
import           Control.Monad.Trans.Reader (ReaderT (ReaderT))
import qualified Control.Monad.Trans.RWS.Lazy as L (RWST (RWST))
import           Control.Monad.Trans.RWS.Strict (RWST (RWST))
import           Data.Functor.Product (Product (Pair))


-- layers --------------------------------------------------------------------
import           Control.Monad.Layer
                     ( MonadLayer (type Inner, layer, layerInvmap)
                     )


------------------------------------------------------------------------------
-- | The 'MonadReader interface monad represents computations which can read
-- values from a shared environment, pass values from function to function
-- and execute sub-computations in a modified environment. Using the 
-- @MonadReader@ interface for such computations is often clearer and easier
-- than using the 'Control.Monad.Interface.State.MonadState' interface.
--
-- Minimal complete definition: 'local' and one of either 'reader' or 'ask'.
class Monad m => MonadReader r m | m -> r where
    -- | Embed a simple reader action into the monad.
    reader :: (r -> a) -> m a

    -- | Retrieves the monad environment.
    ask :: m r

    -- | Executes a computation in a modified environment.
    local :: (r -> r) -> m a -> m a

    reader f = liftM f ask
    {-# INLINE reader #-}

    ask = reader id
    {-# INLINE ask #-}


------------------------------------------------------------------------------
instance MonadReader r ((->) r) where
    reader = ($)
    {-# INLINE reader #-}
    ask = id
    {-# INLINE ask #-}
    local = flip (.)
    {-# INLINE local #-}


------------------------------------------------------------------------------
instance Monad m => MonadReader r (ReaderT r m) where
    reader = ReaderT . (return .)
    {-# INLINE reader #-}
    ask = ReaderT return
    {-# INLINE ask #-}
    local f (ReaderT m) = ReaderT $ m . f
    {-# INLINE local #-}


------------------------------------------------------------------------------
instance (Monad m, Monoid w) => MonadReader r (L.RWST r w s m) where
    reader f = L.RWST $ \r s -> return (f r, s, mempty)
    {-# INLINE reader #-}
    ask = L.RWST $ \r s -> return (r, s, mempty)
    {-# INLINE ask #-}
    local f (L.RWST m) = L.RWST $ \r s -> m (f r) s
    {-# INLINE local #-}


------------------------------------------------------------------------------
instance (Monad m, Monoid w) => MonadReader r (RWST r w s m) where
    reader f = RWST $ \r s -> return (f r, s, mempty)
    {-# INLINE reader #-}
    ask = RWST $ \r s -> return (r, s, mempty)
    {-# INLINE ask #-}
    local f (RWST m) = RWST $ \r s -> m (f r) s
    {-# INLINE local #-}


------------------------------------------------------------------------------
instance (MonadReader r f, MonadReader r g) =>
    MonadReader r (Product f g)
  where
    reader f = Pair (reader f) (reader f)
    {-# INLINE reader #-}
    ask = Pair ask ask
    {-# INLINE ask #-}
    local t (Pair f g) = Pair (local t f) (local t g)
    {-# INLINE local #-}


------------------------------------------------------------------------------
instance (MonadLayer m, MonadReader r (Inner m)) => MonadReader r m where
    reader = layer . reader
    {-# INLINE reader #-}
    ask = layer ask
    {-# INLINE ask #-}
    local f m = layer ask >>= \r -> layerInvmap (local f) (local (const r)) m
    {-# INLINE local #-}


------------------------------------------------------------------------------
-- | Retrieves a function of the current environment.
asks :: MonadReader r m => (r -> a) -> m a
asks = reader
{-# INLINE asks #-}