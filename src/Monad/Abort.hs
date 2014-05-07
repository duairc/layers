{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE UndecidableInstances #-}
#ifdef LANGUAGE_ConstraintKinds
{-# LANGUAGE ConstraintKinds #-}
#endif

{-# OPTIONS_GHC -fno-warn-warnings-deprecations #-}

{-|

This module defines the 'MonadAbort' interface, which consists of:

    * 'MonadAbort' :: @* -> (* -> *) -> Constraint@

    * 'abort' :: @MonadAbort e m => e -> m a@

The 'MonadAbort' interface is the basis of both the 'Monad.Throw.MonadThrow'
and 'Monad.Error.MonadError' interfaces.

-}

module Monad.Abort
    ( MonadAbort (abort)
    )
where

-- base ----------------------------------------------------------------------
import           Control.Exception (SomeException, throwIO)
import           Control.Monad (mzero)
#if MIN_VERSION_base(4, 3, 0)
import           GHC.Conc.Sync (STM, throwSTM)
#else
import           GHC.Conc (STM, unsafeIOToSTM)
#endif


#if MIN_VERSION_mmorph(1, 0, 1)
-- mmorph --------------------------------------------------------------------
import           Control.Monad.Trans.Compose (ComposeT (ComposeT))
#endif


-- transformers --------------------------------------------------------------
#if !MIN_VERSION_transformers(0, 5, 0)
import           Control.Monad.Trans.Error (ErrorT (ErrorT), Error)
#endif
#if MIN_VERSION_transformers(0, 4, 0)
import           Control.Monad.Trans.Except (ExceptT (ExceptT))
#endif
import           Control.Monad.Trans.Maybe (MaybeT)
import           Control.Monad.Trans.List (ListT)
#if MIN_VERSION_transformers(0, 3, 0)
import           Data.Functor.Product (Product (Pair))
#endif


-- layers --------------------------------------------------------------------
import          Control.Monad.Lift.Top (MonadTop, liftT)


------------------------------------------------------------------------------
-- | The @'MonadAbort' e@ constraint matches monads whose computations can
-- \"fail\" (be aborted), and, if possible, store a value of type @e@
-- containing information about the nature of the failure.
--
-- Every monad which permits an instance 'Control.Monad.MonadPlus' trivially
-- permits an instance of @MonadFlexibleInstancesFlexibleInstancesAbort@: for these monads, the @e@ paramater to
-- 'abort' is discarded, and @abort@ is implemented as @const mzero@.
--
-- The other class of monads that permit a @MonadAbort@ instance are the
-- 'Either'-like monads (including 'IO'): these monads actually store the @e@
-- parameter passed to the @abort@ operation on failure. These monads also
-- generally permit a 'Monad.Recover.MonadRecover' instance.
class Monad m => MonadAbort e m where
    -- | The following law holds for valid instances of 'MonadAbort';
    --
    --     [Zero] @abort e >>= f = abort e@
    abort :: e -> m a


------------------------------------------------------------------------------
instance MonadAbort e ([]) where
    abort = const mzero


------------------------------------------------------------------------------
instance MonadAbort e Maybe where
    abort = const mzero


------------------------------------------------------------------------------
instance MonadAbort e (Either e) where
    abort = Left


------------------------------------------------------------------------------
instance MonadAbort SomeException IO where
    abort = throwIO


------------------------------------------------------------------------------
instance MonadAbort SomeException STM where
#if MIN_VERSION_base(4, 3, 0)
    abort = throwSTM
#else
    abort = unsafeIOToSTM . throwIO
#endif


------------------------------------------------------------------------------
instance Monad m => MonadAbort e (ListT m) where
    abort = const mzero


------------------------------------------------------------------------------
instance Monad m => MonadAbort e (MaybeT m) where
    abort = const mzero


#if !MIN_VERSION_transformers(0, 5, 0)
------------------------------------------------------------------------------
instance (Error e, Monad m) => MonadAbort e (ErrorT e m) where
    abort = ErrorT . return . Left
#endif


#if MIN_VERSION_transformers(0, 4, 0)
------------------------------------------------------------------------------
instance Monad m => MonadAbort e (ExceptT e m) where
    abort = ExceptT . return . Left
#endif


#if MIN_VERSION_transformers(0, 3, 0)
------------------------------------------------------------------------------
instance (MonadAbort e f, MonadAbort e g) => MonadAbort e (Product f g) where
    abort e = Pair (abort e) (abort e)
#endif


#if MIN_VERSION_mmorph(1, 0, 1)
------------------------------------------------------------------------------
instance MonadAbort e (f (g m)) => MonadAbort e (ComposeT f g m) where
    abort = ComposeT . abort
#endif


------------------------------------------------------------------------------
instance (MonadTop t m, MonadAbort e m) => MonadAbort e (t m)
  where
    abort = liftT . abort
    {-# INLINABLE abort #-}
