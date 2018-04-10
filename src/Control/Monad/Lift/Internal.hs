{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
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

module Control.Monad.Lift.Internal
    ( LayerEffects, LayerResult, LayerState, coercePeel
    , ComposeResult2 (ComposeResult2), ComposeResult3 (ComposeResult3)
    , ComposeResult4 (ComposeResult4)
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
type LayerEffects t a = (LayerResult t a, LayerState t)


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
#if MIN_VERSION_mmorph(1, 0, 1)
type instance LayerResult (ComposeT f g) = ComposeResult2 f g
#endif


------------------------------------------------------------------------------
-- | The G(layerstate,layer state) of @t@.
type family LayerState (t :: (* -> *) -> * -> *) :: *
#if !MIN_VERSION_transformers(0, 6, 0)
type instance LayerState (ErrorT e) = ()
#endif
#if MIN_VERSION_transformers(0, 4, 0)
type instance LayerState (ExceptT e) = ()
#endif
type instance LayerState IdentityT = ()
type instance LayerState ListT = ()
type instance LayerState MaybeT = ()
type instance LayerState (ReaderT r) = r
type instance LayerState (StateT s) = s
type instance LayerState (L.StateT s) = s
type instance LayerState (RWST r w s) = (r, s)
type instance LayerState (L.RWST r w s) = (r, s)
type instance LayerState (WriterT w) = ()
type instance LayerState (L.WriterT w) = ()
#if MIN_VERSION_mmorph(1, 0, 1)
type instance LayerState (ComposeT f g) = (LayerState f, LayerState g)
#endif


------------------------------------------------------------------------------
newtype ComposeResult2 u v a = ComposeResult2
    (LayerResult v (LayerEffects u a))


------------------------------------------------------------------------------
instance (Functor (LayerResult u), Functor (LayerResult v)) =>
    Functor (ComposeResult2 u v)
  where
    fmap f (ComposeResult2 a) = ComposeResult2 (fmap (first (fmap f)) a)
    {-# INLINE fmap #-}


------------------------------------------------------------------------------
newtype ComposeResult3 u v w a = ComposeResult3
    (LayerResult w (LayerEffects v (LayerEffects u a)))


------------------------------------------------------------------------------
instance
    ( Functor (LayerResult u), Functor (LayerResult v)
    , Functor (LayerResult w)
    )
  =>
    Functor (ComposeResult3 u v w)
  where
    fmap f (ComposeResult3 a) = ComposeResult3
        (fmap (first (fmap (first (fmap f)))) a)
    {-# INLINE fmap #-}


------------------------------------------------------------------------------
newtype ComposeResult4 u v w x a = ComposeResult4
    (LayerResult x (LayerEffects w (LayerEffects v (LayerEffects u a))))


------------------------------------------------------------------------------
instance
    ( Functor (LayerResult u), Functor (LayerResult v)
    , Functor (LayerResult w), Functor (LayerResult x)
    )
  =>
    Functor (ComposeResult4 u v w x)
  where
    fmap f (ComposeResult4 a) = ComposeResult4
        (fmap (first (fmap (first (fmap (first (fmap f)))))) a)
    {-# INLINE fmap #-}


------------------------------------------------------------------------------
coercePeel :: forall t m. ()
    => (forall a. t m a -> m (LayerEffects t a))
    -> (forall a. t m a -> m (LayerEffects t a))
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
    OuterState i (t m) = (LayerState t, OuterState i m)
    OuterState i m = OuterState i (Codomain1 m)
#else
type OuterState i m = OuterState_ i m
#endif


------------------------------------------------------------------------------
newtype ComposeResult i t m a = ComposeResult
    (OuterResult i m (LayerResult t a, LayerState t))


------------------------------------------------------------------------------
instance (Functor (OuterResult i m), Functor (LayerResult t)) =>
    Functor (ComposeResult i t m)
  where
    fmap f (ComposeResult a) = ComposeResult (fmap (first (fmap f)) a)


#ifndef ClosedTypeFamilies
------------------------------------------------------------------------------
newtype OuterResult_ (i :: * -> *) (m :: * -> *) (a :: *) = OuterResult_ Any


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
