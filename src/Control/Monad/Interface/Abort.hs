{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE UndecidableInstances #-}

{-|
-}

module Control.Monad.Interface.Abort
    ( MonadAbort (abort)
    )
where

-- base ----------------------------------------------------------------------
import          Control.Exception (SomeException, throwIO)
import          Control.Monad (mzero)
import          GHC.Conc.Sync (STM, throwSTM)


-- transformers --------------------------------------------------------------
import          Control.Monad.Trans.Error (ErrorT (ErrorT), Error)
import          Control.Monad.Trans.Maybe (MaybeT)
import          Control.Monad.Trans.List (ListT)
import          Data.Functor.Product (Product (Pair))


-- layers --------------------------------------------------------------------
import          Control.Monad.Layer (MonadLayer (type Inner, layer))


------------------------------------------------------------------------------
-- | The 'MonadAbort' type class represents the class of monads which can
-- fail, and, if possible, store a value of type @e@ in doing so.
--
-- Minimal complete definition: abort.
class Monad m => MonadAbort e m where
    -- | Fail with a given exception.
    abort :: e -> m a


------------------------------------------------------------------------------
instance MonadAbort e ([]) where
    abort = const mzero
    {-# INLINE abort #-}


------------------------------------------------------------------------------
instance MonadAbort e Maybe where
    abort = const mzero
    {-# INLINE abort #-}


------------------------------------------------------------------------------
instance MonadAbort e (Either e) where
    abort = Left
    {-# INLINE abort #-}


------------------------------------------------------------------------------
instance MonadAbort SomeException IO where
    abort = throwIO
    {-# INLINE abort #-}


------------------------------------------------------------------------------
instance MonadAbort SomeException STM where
    abort = throwSTM
    {-# INLINE abort #-}


------------------------------------------------------------------------------
instance Monad m => MonadAbort e (ListT m) where
    abort = const mzero
    {-# INLINE abort #-}


------------------------------------------------------------------------------
instance Monad m => MonadAbort e (MaybeT m) where
    abort = const mzero
    {-# INLINE abort #-}


------------------------------------------------------------------------------
instance (Error e, Monad m) => MonadAbort e (ErrorT e m) where
    abort = ErrorT . return . Left
    {-# INLINE abort #-}


------------------------------------------------------------------------------
instance (MonadAbort e f, MonadAbort e g) => MonadAbort e (Product f g) where
    abort e = Pair (abort e) (abort e)


------------------------------------------------------------------------------
instance (MonadLayer m, MonadAbort e (Inner m)) => MonadAbort e m where
    abort = layer . abort
    {-# INLINE abort #-}
