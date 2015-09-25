{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE UndecidableInstances #-}
#ifdef LANGUAGE_ConstraintKinds
{-# LANGUAGE ConstraintKinds #-}
#endif

{-# OPTIONS_GHC -fno-warn-warnings-deprecations #-}

#include <macros.h>

{-|

This module defines the 'MonadAbort' G(monadinterface,interface). It, along
with its sister G(monadinterface,interface) 'MonadRecover', is inspired by
the
<M(monad-abort-fd,Control-Monad-Abort-Class)#t:MonadAbort eponymous interfaces>
from the
<M(monad-abort-fd,Control-Monad-Abort-Class) Control.Monad.Abort.Class> module
module of the H(monad-abort-fd) package. It consists of:

  * The 'MonadAbort' constraint.
  * The 'abort' operation.
  * Instances of 'MonadAbort':

      * For the following G(basemonad,base monads):

          * 'Either'
          * @[@@]@
          * 'Maybe'
          * 'IO'
          * 'STM'

      * For arbitrary G(innermonad,inner monads) wrapped by one of the
      following G(monadlayer,monad layers):

          * 'ErrorT'
          * 'ExceptT'
          * 'ListT'
          * 'MaybeT'

      * G(universalpassthroughinstance,Pass-through instances) for:

          * Any G(innermonad,inner monad) with an existing 'MonadAbort'
          instance wrapped by any G(monadlayer,monad layer) implementing
          'Control.Monad.Lift.MonadTrans'.
          * The 'Product' of any two G(monadictype,monadic types) which both
          have existing 'MonadAbort' instances.
          * The <M(mmorph,Control-Monad-Trans-Compose)#t:ComposeT composition>
          of two G(monadlayer,monad layers) wrapped around an
          G(innermonad,inner monad), where either the
          G(innermonad,inner monad) or one or more of the composed
          G(monadlayer,monad layers) has an existing instance for
          'MonadAbort'.

The 'Monad.Throw.MonadThrow', 'Monad.Catch.MonadCatch' and
'Monad.Error.MonadError' G(monadinterface,interfaces) are all built on top of
'MonadAbort'.

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
-- | The @'MonadAbort' e@ constraint matches monads whose
-- G(computation, computations) can \"G(shortcircuit,fail)\" (be aborted),
-- and, if possible, store a value of type @e@ containing information about
-- the nature of the failure.
--
-- Every monad which permits an instance 'Control.Monad.MonadPlus' trivially
-- permits an instance of 'MonadAbort': for these monads, the @e@ paramater to
-- 'abort' is discarded, and 'abort' is implemented as @'const' 'mzero'@.
--
-- The other class of monads that permit a 'MonadAbort' instance are the
-- 'Either'-like monads (including 'IO'): these monads actually store the @e@
-- parameter passed to the 'abort' operation on failure. These monads also
-- generally permit a 'Monad.Recover.MonadRecover' instance that allows the
-- @e@ value to be recovered using the 'Monad.Recover.recover' operation.
--
-- Minimal complete definition: 'abort'.
class Monad m => MonadAbort e m where
    -- | The following law holds for valid instances of 'MonadAbort':
    --
    --     [Zero] @'abort' e '>>=' f ≡ 'abort' e@
    --
    -- In other words, 'abort' causes the computation to
    -- G(shortcircuit, short-circuit).
    abort :: e -> m a


------------------------------------------------------------------------------
instance MonadAbort e (Either e) where
    abort = Left


------------------------------------------------------------------------------
instance MonadAbort e ([]) where
    abort = const mzero


------------------------------------------------------------------------------
instance MonadAbort e Maybe where
    abort = const mzero


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
instance _OVERLAPPABLE (MonadTop t m, MonadAbort e m, Monad (t m)) =>
    MonadAbort e (t m)
  where
    abort = liftT . abort
    {-# INLINABLE abort #-}
