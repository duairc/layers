{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}

#ifdef LANGUAGE_ConstraintKinds
{-# LANGUAGE ConstraintKinds #-}
#endif

#include <overlap.h>

{-|



-}

module Control.Monad.Lift.Layer
    ( MonadLayer
    , liftL
    , MonadLayerControl
    , suspendL
    , resumeL
    , captureL
    , extractL
    , liftControlL
    , controlL
    , liftOpL
    , liftOpL_
    , liftDiscardL
    , MonadLayerInvariant
    , hoistisoL
    , MonadLayerFunctor
    , hoistL
    )
where

-- layers --------------------------------------------------------------------
import           Control.Monad.Lift
                     ( MonadInner
                     , liftI
                     , MonadInnerControl
                     , OuterEffects
                     , OuterResult
                     , OuterState
                     , suspendI
                     , resumeI
                     , captureI
                     , extractI
                     , liftControlI
                     , controlI
                     , liftOpI
                     , liftOpI_
                     , liftDiscardI
                     , MonadInnerInvariant
                     , hoistisoI
                     , MonadInnerFunctor
                     , hoistI
                     )


------------------------------------------------------------------------------
class (MonadInner (t i) m, MonadInner i (t i)) => MonadLayer i t m
    | t m -> i, i m -> t


------------------------------------------------------------------------------
instance MonadInner i (t i) => MonadLayer i t (t i)


------------------------------------------------------------------------------
instance __OVERLAPPABLE__ (MonadLayer i s m, MonadInner (s i) (t m))
    => MonadLayer i s (t m)


------------------------------------------------------------------------------
liftL :: MonadLayer i t m => t i a -> m a
liftL = liftI


------------------------------------------------------------------------------
#if LANGUAGE_ConstraintKinds
type MonadLayerControl i t m = (MonadInnerControl (t i) m, MonadLayer i t m)
#else
class (MonadInnerControl (t i) m, MonadLayer i t m) => MonadLayerControl i t m
instance (MonadInnerControl (t i) m, MonadLayer i t m) =>
    MonadLayerControl i t m
#endif


------------------------------------------------------------------------------
data Pm (m :: * -> *) = Pm


------------------------------------------------------------------------------
suspendL :: MonadLayerControl i t m
    => m a
    -> OuterState (t i) m
    -> t i (OuterEffects (t i) m a)
suspendL = suspendI


------------------------------------------------------------------------------
resumeL :: forall proxy i t m a. MonadLayerControl i t m
    => proxy t
    -> OuterEffects (t i) m a
    -> m a
resumeL _ = resumeI (Pm :: Pm (t i))


------------------------------------------------------------------------------
captureL :: forall proxy i t m. MonadLayerControl i t m
    => proxy t
    -> m (OuterState (t i) m)
captureL _ = captureI (Pm :: Pm (t i))


------------------------------------------------------------------------------
extractL :: forall proxy proxy' i t m a. MonadLayerControl i t m
    => proxy t
    -> proxy' m
    -> OuterResult (t i) m a
    -> Maybe a
extractL _ = extractI (Pm :: Pm (t i))


------------------------------------------------------------------------------
liftControlL :: MonadLayerControl i t m
    => ((forall b. m b -> t i (OuterEffects (t i) m b)) -> t i a)
    -> m a
liftControlL = liftControlI


------------------------------------------------------------------------------
controlL :: MonadLayerControl i t m
    => ((forall b. m b -> t i (OuterEffects (t i) m b))
        -> t i (OuterEffects (t i) m a))
    -> m a
controlL = controlI


------------------------------------------------------------------------------
liftOpL :: MonadLayerControl i t m
    => ((a -> t i (OuterEffects (t i) m b)) -> t i (OuterEffects (t i) m c))
    -> (a -> m b)
    -> m c
liftOpL = liftOpI


------------------------------------------------------------------------------
liftOpL_ :: MonadLayerControl i t m
    => (t i (OuterEffects (t i) m a) -> t i (OuterEffects (t i) m b))
    -> m a
    -> m b
liftOpL_ = liftOpI_


------------------------------------------------------------------------------
liftDiscardL :: MonadLayerControl i t m => (t i () -> t i a) -> m () -> m a
liftDiscardL = liftDiscardI


------------------------------------------------------------------------------
#if LANGUAGE_ConstraintKinds
type MonadLayerInvariant j n i t m =
    (MonadInnerInvariant j n (t i) m, MonadLayer i t m)
#else
class (MonadInnerInvariant j n (t i) m, MonadLayer i t m) =>
    MonadLayerInvariant j n i t m
        | t m -> i
        , i m -> t
        , i j m -> n
        , t i j m -> n
        , j n m -> i
        , j n m -> t
        , i n m -> j
        , t n m -> j
instance (MonadInnerInvariant j n (t i) m, MonadLayer i t m) =>
    MonadLayerInvariant j n i t m
#endif


------------------------------------------------------------------------------
hoistisoL :: MonadLayerInvariant j n i t m
    => (forall b. t i b -> j b)
    -> (forall b. j b -> t i b)
    -> m a
    -> n a
hoistisoL = hoistisoI


------------------------------------------------------------------------------
#if LANGUAGE_ConstraintKinds
type MonadLayerFunctor j n i t m =
    ( MonadInnerFunctor j n (t i) m
    , MonadLayer i t m
    , MonadLayerInvariant j n i t m
    )
#else
class
    ( MonadInnerFunctor j n (t i) m
    , MonadLayer i t m
    , MonadLayerInvariant j n i t m
    )
  =>
    MonadLayerFunctor j n i t m
        | t m -> i
        , i m -> t
        , i j m -> n
        , t j m -> n
        , t i j n -> m
        , j n m -> i
        , j n m -> t
        , i n m -> j
        , t n m -> j
instance
    ( MonadInnerFunctor j n (t i) m
    , MonadLayer i t m
    , MonadLayerInvariant j n i t m
    )
  =>
    MonadLayerFunctor j n i t m
#endif


------------------------------------------------------------------------------
hoistL :: MonadLayerFunctor j n i t m
    => (forall b. t i b -> j b)
    -> m a
    -> n a
hoistL = hoistI
