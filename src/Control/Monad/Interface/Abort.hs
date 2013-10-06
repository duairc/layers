{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE UndecidableInstances #-}

{-|

This module exports:

    1. The type class 'MonadAbort' and its operation 'abort'.

    2. Instances of 'MonadAbort' for every 'MonadPlus' and 'Either'-like monad
    from the @base@ and @transformers@ packages.
    
    3. A universal pass-through instance of 'MonadAbort' for any existing
    'MonadAbort' wrapped by any 'MonadLayer'.

    4. An orphan instance of the 'Error' class from @transformers@ for the
    'SomeException' type: this is a necessary hack in order to force 'ErrorT'
    to permit instances of 
    an instance of 'MonadCatch'.

-}

module Control.Monad.Interface.Abort
    ( MonadAbort (abort)
    )
where

-- base ----------------------------------------------------------------------
import           Control.Exception (SomeException, throwIO)
import           Control.Monad (mzero)
import           GHC.Conc.Sync (STM, throwSTM)


-- transformers --------------------------------------------------------------
import           Control.Monad.Trans.Error (ErrorT (ErrorT), Error)
import           Control.Monad.Trans.Maybe (MaybeT)
import           Control.Monad.Trans.List (ListT)
import           Data.Functor.Product (Product (Pair))


-- layers --------------------------------------------------------------------
import          Control.Monad.Lift (MonadTrans, lift)


------------------------------------------------------------------------------
-- | The 'MonadAbort' type class represents the class of monads which can
-- fail, and, if possible, store a value of type @e@ in doing so. This
-- includes every monad permitting a 'Control.Monad.MonadPlus' instance (in
-- which case the value of type @e@ is simply discarded) as well as
-- 'Either'-like monads (including 'IO'). The latter class of monads generally
-- permit a 'Control.Monad.Interface.MonadRecover' for resuming failed
-- computations.
--
-- Minimal complete definition: abort.
class Monad m => MonadAbort e m where
    -- | The following law holds for valid instances of 'MonadAbort';
    --
    --     [Zero] @abort e >>= f = abort e@
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
    {-# INLINE abort #-}


------------------------------------------------------------------------------
instance (MonadTrans t, MonadAbort e m, Monad (t m)) => MonadAbort e (t m)
  where
    abort = lift . abort
    {-# INLINE abort #-}
