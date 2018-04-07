{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

#ifdef LANGUAGE_ConstraintKinds
{-# LANGUAGE ConstraintKinds #-}
#endif

#ifdef LANGUAGE_SafeHaskell
#if __GLASGOW_HASKELL__ >= 704
{-# LANGUAGE Safe #-}
#else
{-# LANGUAGE Trustworthy #-}
#endif
#endif

#include <docmacros.h>
#include <overlap.h>

{-|

This module defines the 'MonadReader' G(monadinterface,interface). It is
designed to be compatible with the with the
T(mtl,Control-Monad-Reader-Class,MonadReader) interface from the H(mtl)
package. It consists of:

  * The 'MonadReader' constraint.
  * The 'ask' and 'local' operations.

  * Instances of 'MonadReader':

      * For the base monad @-@@>@.

      * For arbitrary G(innermonad,inner monads) wrapped by one of the
      following G(monadlayer,monad layers):

          * 'ReaderT'
          * Lazy 'L.RWST'
          * Strict 'RWST'

      * G(universalpassthroughinstance,Pass-through instances) for:

          * Any G(innermonad,inner monad) with an existing 'MonadReader'
          instance wrapped by any G(monadlayer,monad layer) implementing
          'Control.Monad.Lift.MonadTrans' and 'Control.Monad.Lift.MInvariant'.
          * The 'Product' of any two G(monadictype,monadic types) which both
          have existing 'MonadReader' instances.
          * The <M(mmorph,Control-Monad-Trans-Compose)#t:ComposeT composition>
          of two G(monadlayer,monad layers) wrapped around an
          G(innermonad,inner monad), where either the
          G(innermonad,inner monad) or one or more of the composed
          G(monadlayer,monad layers) has an existing instance for
          'MonadReader'.

  * The 'asks' and 'reader' utility operations.

-}

module Monad.Reader
    ( MonadReader (reader, ask, local)
    , asks
    )
where

-- base ----------------------------------------------------------------------
import           Control.Monad (liftM)
#if !MIN_VERSION_base(4, 8, 0)
import           Data.Monoid (Monoid (mempty))
#endif


-- layers --------------------------------------------------------------------
import           Control.Monad.Lift (MonadTrans, lift, MInvariant, hoistiso)


#if MIN_VERSION_mmorph(1, 0, 1)
-- mmorph --------------------------------------------------------------------
import           Control.Monad.Trans.Compose (ComposeT (ComposeT))


#endif
-- transformers --------------------------------------------------------------
import           Control.Monad.Trans.Reader (ReaderT (ReaderT))
import qualified Control.Monad.Trans.RWS.Lazy as L (RWST (RWST))
import           Control.Monad.Trans.RWS.Strict (RWST (RWST))
#if MIN_VERSION_transformers(0, 3, 0)
import           Data.Functor.Product (Product (Pair))
#endif


------------------------------------------------------------------------------
-- | The 'MonadReader' G(monadinterface,interface) represents
-- G(computation,computations) which can read values from a shared
-- environment, pass values from function to function and execute
-- G(computation,sub-computations) in a modified environment. Using the
-- 'MonadReader' G(monadinterface,interface) for such
-- G(computation,computations) is often clearer and easier
-- than using the 'Monad.State.MonadState' G(monadinterface,interface).
--
-- Minimal complete definition: 'local' and one of either 'reader' or 'ask'.
class Monad m => MonadReader r m | m -> r where
    -- | Embed a simple reader G(computation,action) into the monad.
    reader :: (r -> a) -> m a

    -- | Retrieves the monad environment.
    ask :: m r

    -- | Executes a G(computation,computation) in a modified environment.
    local :: (r -> r) -> m a -> m a

    reader f = liftM f ask
    {-# INLINABLE reader #-}

    ask = reader id
    {-# INLINABLE ask #-}

#ifdef MinimalPragma
    {-# MINIMAL local, (reader | ask) #-}

#endif

------------------------------------------------------------------------------
instance MonadReader r ((->) r) where
    reader = ($)
    ask = id
    local = flip (.)


------------------------------------------------------------------------------
instance Monad m => MonadReader r (ReaderT r m) where
    reader = ReaderT . (return .)
    ask = ReaderT return
    local f (ReaderT m) = ReaderT $ m . f


------------------------------------------------------------------------------
instance (Monad m, Monoid w) => MonadReader r (L.RWST r w s m) where
    reader f = L.RWST $ \r s -> return (f r, s, mempty)
    ask = L.RWST $ \r s -> return (r, s, mempty)
    local f (L.RWST m) = L.RWST $ \r s -> m (f r) s


------------------------------------------------------------------------------
instance (Monad m, Monoid w) => MonadReader r (RWST r w s m) where
    reader f = RWST $ \r s -> return (f r, s, mempty)
    ask = RWST $ \r s -> return (r, s, mempty)
    local f (RWST m) = RWST $ \r s -> m (f r) s


#if MIN_VERSION_transformers(0, 3, 0)
------------------------------------------------------------------------------
instance (MonadReader r f, MonadReader r g) =>
    MonadReader r (Product f g)
  where
    reader f = Pair (reader f) (reader f)
    ask = Pair ask ask
    local t (Pair f g) = Pair (local t f) (local t g)


#endif
#if MIN_VERSION_mmorph(1, 0, 1)
------------------------------------------------------------------------------
instance MonadReader r (f (g m)) => MonadReader r (ComposeT f g m) where
    reader f = ComposeT (reader f)
    ask = ComposeT ask
    local t (ComposeT m) = ComposeT (local t m)


#endif
------------------------------------------------------------------------------
instance __OVERLAPPABLE__
    (MonadTrans t, MInvariant t, MonadReader r m, Monad (t m))
  =>
    MonadReader r (t m)
  where
    reader = lift . reader
    {-# INLINABLE reader #-}
    ask = lift ask
    {-# INLINABLE ask #-}
    local f m = lift ask >>= \r -> hoistiso (local f) (local (const r)) m
    {-# INLINABLE local #-}


------------------------------------------------------------------------------
-- | Retrieves a function of the current environment.
asks :: MonadReader r m => (r -> a) -> m a
asks = reader
