{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE UndecidableInstances #-}
#ifdef LANGUAGE_ConstraintKinds
{-# LANGUAGE ConstraintKinds #-}
#endif

{-# OPTIONS_GHC -fno-warn-warnings-deprecations #-}

#include <macros.h>

{-|

This module defines the 'MonadRecover' G(monadinterface,interface). It, along
with its sister G(monadinterface,interface) 'MonadAbort', is inspired by the
<M(monad-abort-fd,Control-Monad-Abort-Class)#t:MonadRecover eponymous interfaces>
from the
<M(monad-abort-fd,Control-Monad-Abort-Class) Control.Monad.Abort.Class> module
of the H(monad-abort-fd) package. It consists of:

  * The 'MonadRecover' constraint.
  * The 'recover' operation.
  * Instances of 'MonadRecover':

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

          * Any G(innermonad,inner monad) with an existing 'MonadRecover'
          instance wrapped by any G(monadlayer,monad layer) implementing
          'Control.Monad.Lift.MonadTransControl'.
          * The 'Product' of any two G(monadictype,monadic types) which both
          have existing 'MonadRecover' instances.
          * The <M(mmorph,Control-Monad-Trans-Compose)#t:ComposeT composition>
          of two G(monadlayer,monad layers) wrapped around an
          G(innermonad,inner monad), where either the
          G(innermonad,inner monad) or one or more of the composed
          G(monadlayer,monad layers) has an existing instance for
          'MonadRecover'.

The 'Monad.Catch.MonadCatch' and 'Monad.Error.MonadError'
G(monadinterface,interfaces) are both built on top of 'MonadRecover'.

-}

module Monad.Recover
    ( MonadRecover (recover)
    )
where

-- base ----------------------------------------------------------------------
import           Control.Exception (SomeException, catch)
import           Control.Monad (mplus)
#if MIN_VERSION_base(4, 3, 0)
import           GHC.Conc.Sync (STM, catchSTM)
#else
import           GHC.Conc (STM, catchSTM)
#endif
#if !MIN_VERSION_base(4, 6, 0)
import           Prelude hiding (catch)
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
import           Control.Monad.Lift.Top (MonadTopControl, controlT)
import           Monad.Abort (MonadAbort)


------------------------------------------------------------------------------
-- | The @'MonadRecover' e@ constraint matches monads whose
-- G(computation,computations) can 'recover' from a failure caused by a call
-- to 'Monad.Abort.abort'.
--
-- Every monad which permits an instance 'Control.Monad.MonadPlus' (of the
-- <HW(MonadPlus) left catch> variety) trivially permits an instance of
-- 'MonadRecover': for these instances, the @e@ parameter is fixed to @()@, as
-- there is no @e@ value which can be recovered from a
-- \"G(shortcircuit,zero)\".
--
-- The other class of monads that permit a 'MonadRecover' instance are the
-- 'Either'-like monads (including 'IO'): these monads actually store the @e@
-- parameter passed to the 'abort' operation on failure, hence it can later be
-- retrieved using the 'recover' operation.
--
-- Minimal complete definition: 'recover'.
class MonadAbort e m => MonadRecover e m | m -> e where
    -- | In addition to the 'MonadAbort' \"G(shortcircuit,zero)\" law, the
    -- following laws hold for valid instances of 'MonadRecover':
    --
    --     [Left Identity] @'recover' ('Monad.Abort.abort' e) (\\_ -> m) ≡ m@
    --     [Right Identity] @'recover' m 'Monad.Abort.abort' ≡ m@
    --     [Associativity] @'recover' m (\\_ -> 'recover' n (\\_ -> o)) ≡ 'recover' ('recover' m (\\_ -> n)) (\\_ -> o)@
    --     [Left Catch] @'recover' ('return' a) _ ≡ 'return' a@
    --     [Recoverability] @'recover' ('Monad.Abort.abort' e) 'return' ≡ 'return' e@
    recover :: m a -> (e -> m a) -> m a


------------------------------------------------------------------------------
instance MonadRecover e (Either e) where
    recover m h = either h Right m


------------------------------------------------------------------------------
instance MonadRecover () ([]) where
    recover m h = mplus m (h ())


------------------------------------------------------------------------------
instance MonadRecover () Maybe where
    recover m h = mplus m (h ())


------------------------------------------------------------------------------
instance MonadRecover SomeException IO where
    recover = catch


------------------------------------------------------------------------------
instance MonadRecover SomeException STM where
    recover = catchSTM


#if !MIN_VERSION_transformers(0, 5, 0)
------------------------------------------------------------------------------
instance (Error e, Monad m) => MonadRecover e (ErrorT e m) where
    recover (ErrorT m) h = ErrorT $ m >>= either
        (\e -> let ErrorT m' = h e in m')
        (return . Right)
    {-# INLINABLE recover #-}
#endif


#if MIN_VERSION_transformers(0, 4, 0)
------------------------------------------------------------------------------
instance Monad m => MonadRecover e (ExceptT e m) where
    recover (ExceptT m) h = ExceptT $ m >>= either
        (\e -> let ExceptT m' = h e in m')
        (return . Right)
    {-# INLINABLE recover #-}
#endif


------------------------------------------------------------------------------
instance Monad m => MonadRecover () (ListT m) where
    recover m h = mplus m (h ())


------------------------------------------------------------------------------
instance Monad m => MonadRecover () (MaybeT m) where
    recover m h = mplus m (h ())


#if MIN_VERSION_transformers(0, 3, 0)
------------------------------------------------------------------------------
instance (MonadRecover e f, MonadRecover e g) => MonadRecover e (Product f g)
  where
    recover (Pair f g) h = Pair
        (recover f (\e -> let Pair f' _ = h e in f'))
        (recover g (\e -> let Pair _ g' = h e in g'))
#endif


#if MIN_VERSION_mmorph(1, 0, 1)
------------------------------------------------------------------------------
instance MonadRecover e (f (g m)) => MonadRecover e (ComposeT f g m) where
    recover (ComposeT m) h = ComposeT
        (recover m (\e -> let ComposeT m' = h e in m'))
#endif


------------------------------------------------------------------------------
instance (MonadTopControl t m, MonadRecover e m, MonadAbort e (t m)) =>
    MonadRecover e (t m)
  where
    recover m h = controlT (\peel -> recover (peel m) (peel . h))
    {-# INLINABLE recover #-}
