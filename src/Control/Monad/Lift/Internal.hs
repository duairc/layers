{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE IncoherentInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -fno-warn-warnings-deprecations #-}

#ifdef LANGUAGE_DefaultSignatures
{-# LANGUAGE DefaultSignatures #-}
#endif

#if defined(LANGUAGE_SafeHaskell) && __GLASGOW_HASKELL__ >= 704
{-# LANGUAGE Unsafe #-}
#endif

#include "docmacros.h"
#include "overlap.h"

module Control.Monad.Lift.Internal
    ( LayerEffects, LayerResult, LayerState, coercePeel
    , OuterEffects, OuterResult, OuterState, coercePeelI
    , ComposeResult (ComposeResult), fromR, toR, fromS, toS
    , Iso1, Codomain1, from1, to1
    )
where

-- base ----------------------------------------------------------------------
import           Control.Arrow (first)
#if MIN_VERSION_base(4, 7, 0) && __GLASGOW_HASKELL__ >= 710
import           Data.Coerce (Coercible, coerce)
#endif
import           Data.Functor.Identity (Identity)
#ifndef ClosedTypeFamilies
import           GHC.Exts (Any)
import           Unsafe.Coerce (unsafeCoerce)
#endif


#if MIN_VERSION_mmorph(1, 0, 1)
-- mmorph --------------------------------------------------------------------
import           Control.Monad.Trans.Compose (ComposeT (ComposeT))


#endif
-- transformers --------------------------------------------------------------
#if !MIN_VERSION_transformers(0, 6, 0)
import           Control.Monad.Trans.Error (ErrorT)
#endif
#if MIN_VERSION_transformers(0, 4, 0)
import           Control.Monad.Trans.Except (ExceptT)
#endif
import           Control.Monad.Trans.Identity (IdentityT)
import           Control.Monad.Trans.List (ListT)
import           Control.Monad.Trans.Maybe (MaybeT)
import           Control.Monad.Trans.Reader (ReaderT)
import qualified Control.Monad.Trans.RWS.Lazy as L (RWST)
import           Control.Monad.Trans.RWS.Strict (RWST)
import qualified Control.Monad.Trans.State.Lazy as L (StateT)
import           Control.Monad.Trans.State.Strict (StateT)
import qualified Control.Monad.Trans.Writer.Lazy as L (WriterT)
import           Control.Monad.Trans.Writer.Strict (WriterT)


------------------------------------------------------------------------------
-- | The G(layereffect,layer effects) of the @t@ G(monadlayer,layer) of the
-- monad @t m@.
type LayerEffects t m a = (LayerResult t a, LayerState t m)


------------------------------------------------------------------------------
-- | The G(layerresult,layer result) of @t@.
type family LayerResult (t :: (* -> *) -> * -> *) :: * -> *
#if !MIN_VERSION_transformers(0, 6, 0)
type instance LayerResult (ErrorT e) = Either e
#endif
#if MIN_VERSION_transformers(0, 4, 0)
type instance LayerResult (ExceptT e) = Either e
#endif
type instance LayerResult IdentityT = Identity
type instance LayerResult ListT = []
type instance LayerResult MaybeT = Maybe
type instance LayerResult (ReaderT r) = Identity
type instance LayerResult (StateT s) = Identity
type instance LayerResult (L.StateT s) = Identity
type instance LayerResult (RWST r w s) = (,) w
type instance LayerResult (L.RWST r w s) = (,) w
type instance LayerResult (WriterT w) = (,) w
type instance LayerResult (L.WriterT w) = (,) w


------------------------------------------------------------------------------
-- | The G(layerstate,layer state) of @t@.
type family LayerState (t :: (* -> *) -> * -> *) (m :: * -> *) :: *
#if !MIN_VERSION_transformers(0, 6, 0)
type instance LayerState (ErrorT e) m = ()
#endif
#if MIN_VERSION_transformers(0, 4, 0)
type instance LayerState (ExceptT e) m = ()
#endif
type instance LayerState IdentityT m = ()
type instance LayerState ListT m = ()
type instance LayerState MaybeT m = ()
type instance LayerState (ReaderT r) m = r
type instance LayerState (StateT s) m = s
type instance LayerState (L.StateT s) m = s
type instance LayerState (RWST r w s) m = (r, s)
type instance LayerState (L.RWST r w s) m = (r, s)
type instance LayerState (WriterT w) m = ()
type instance LayerState (L.WriterT w) m = ()


------------------------------------------------------------------------------
coercePeel :: forall t m. ()
    => (forall a. t m a -> m (LayerEffects t m a))
    -> (forall a. t m a -> m (LayerEffects t m a))
#if __GLASGOW_HASKELL__ >= 704
coercePeel f = f
#else
coercePeel = unsafeCoerce
#endif
{-# INLINE coercePeel #-}


------------------------------------------------------------------------------
-- | The combined G(layereffect,layer effects) of all the
-- G(outerlayer,outer layers) around @i@ of the monad @m@.
type OuterEffects i m a = (OuterResult i m a, OuterState i m)


------------------------------------------------------------------------------
-- | The combined G(layerresult,layer results) of all the
-- G(outerlayer,outer layers) around @i@ of the monad @m@.
--
-- Note: On GHC 7.8 and up, this is implemented as a
-- BWT(NewAxioms/ClosedTypeFamilies, closed type family). Older versions of
-- GHC do not support closed type families, but we use various hacks involving
-- 'Any' and 'unsafeCoerce' to provide the same interface. You should not need
-- to worry about this; I am pretty sure it is safe.
#ifdef ClosedTypeFamilies
type family OuterResult (i :: * -> *) (m :: * -> *) :: * -> *
  where
    OuterResult m m = Identity
    OuterResult i (t m) = ComposeResult i t m
    OuterResult i m = OuterResult i (Codomain1 m)
#else
type OuterResult i m = OuterResult_ i m
#endif


------------------------------------------------------------------------------
-- | The combined G(layerstate,layer states) of all the
-- G(outerlayer,outer layers) around @i@ of the monad @m@.
--
-- Note: On GHC 7.8 and up, this is implemented as a
-- BWT(NewAxioms/ClosedTypeFamilies, closed type family). Older versions of
-- GHC do not support closed type families, but we use various hacks involving
-- 'GHC.Exts.Any' and 'Unsafe.Coerce.unsafeCoerce' to provide the same
-- interface. You should not need to worry about this; I am pretty sure it is
-- safe.
#ifdef ClosedTypeFamilies
type family OuterState (i :: * -> *) (m :: * -> *) :: *
  where
    OuterState m m = ()
    OuterState i (t m) = (LayerState t m, OuterState i m)
    OuterState i m = OuterState i (Codomain1 m)
#else
type OuterState i m = OuterState_ i m
#endif


------------------------------------------------------------------------------
newtype ComposeResult i t m a = ComposeResult
    (OuterResult i m (LayerResult t a, LayerState t m))


------------------------------------------------------------------------------
instance (Functor (OuterResult i m), Functor (LayerResult t)) =>
    Functor (ComposeResult i t m)
  where
    fmap f (ComposeResult or_) = ComposeResult (fmap (first (fmap f)) or_)


#ifndef ClosedTypeFamilies
------------------------------------------------------------------------------
newtype OuterResult_ (i :: * -> *) (m :: * -> *) (a :: *) = OuterResult_ Any


------------------------------------------------------------------------------
instance Functor (OuterResult_ m m) where
    fmap f (r :: OuterResult_ m m a) = toR (fmap f a)
      where
        a = fromR r :: Identity a


------------------------------------------------------------------------------
instance __OVERLAPPABLE__
    ( Functor (OuterResult_ i m), Functor (LayerResult t)
    )
  =>
    Functor (OuterResult_ i (t m))
  where
    fmap f (r :: OuterResult_ i (t m) a) = toR (fmap f a)
      where
        a = fromR r :: ComposeResult i t m a


------------------------------------------------------------------------------
instance __OVERLAPPABLE__
    ( Functor (OuterResult_ i (Codomain1 m))
    )
  =>
    Functor (OuterResult_ i m)
  where
    fmap f (r :: OuterResult_ i m a) = toR (fmap f a)
      where
        a = fromR r :: OuterResult_ i (Codomain1 m) a


------------------------------------------------------------------------------
newtype OuterState_ (i :: * -> *) (m :: * -> *) = OuterState_ Any


#endif
------------------------------------------------------------------------------
#ifdef ClosedTypeFamilies
toS, fromS, toR, fromR :: a -> a
toS = id; fromS = id; toR = id; fromR = id
#else
toS :: x -> OuterState_ i m; toR :: x -> OuterResult_ i m a
toS = OuterState_ . unsafeCoerce; toR = OuterResult_ . unsafeCoerce
fromS :: OuterState_ i m -> x; fromR :: OuterResult_ i m a -> x
fromS (OuterState_ x) = unsafeCoerce x; fromR (OuterResult_ x) = unsafeCoerce x
#endif
{-# INLINE toS #-}
{-# INLINE toR #-}
{-# INLINE fromS #-}
{-# INLINE fromR #-}


------------------------------------------------------------------------------
coercePeelI :: forall i m. ()
    => (forall a. m a -> i (OuterEffects i m a))
    -> (forall a. m a -> i (OuterEffects i m a))
#if __GLASGOW_HASKELL__ >= 704
coercePeelI f = f
#else
coercePeelI = unsafeCoerce
#endif
{-# INLINE coercePeelI #-}


------------------------------------------------------------------------------
class Iso1 t where
    type Codomain1 (t :: * -> *) :: * -> *
    to1 :: forall a. t a -> Codomain1 t a
    from1 :: forall a. Codomain1 t a -> t a
#if MIN_VERSION_base(4, 7, 0)
-- fails on GHC 7.8 for some reason
#if __GLASGOW_HASKELL__ >= 710

    default to1 :: Coercible t (Codomain1 t) => forall a. t a -> Codomain1 t a
    to1 = coerce

    default from1
        :: Coercible (Codomain1 t) t => forall a. Codomain1 t a -> t a
    from1 = coerce
#endif
#endif


#if MIN_VERSION_mmorph(1, 0, 1)
------------------------------------------------------------------------------
instance Iso1 (ComposeT f g m) where
    type Codomain1 (ComposeT f g m) = f (g m)
    to1 (ComposeT m) = m
    from1 = ComposeT


#endif
