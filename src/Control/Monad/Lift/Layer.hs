{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
#ifdef LANGUAGE_ConstraintKinds
{-# LANGUAGE ConstraintKinds #-}
#endif

{-|



-}

module Control.Monad.Lift.Layer
    ( MonadLayer
    , liftT
    , MonadLayerControl
    , suspendT
    , resumeT
    , captureT
    , extractT
    , liftControlT
    , controlT
    , liftOpT
    , liftOpT_
    , liftDiscardT
    , MonadLayerInvariant
    , hoistisoT
    , MonadLayerFunctor
    , hoistT
    )
where

-- layers --------------------------------------------------------------------
import           Control.Monad.Lift
                     ( MonadTrans
                     , MonadTransControl
                     , MonadInner
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
class (MonadInner (t i) m, MonadTrans t) => MonadLayer i t m
    | t m -> i, i m -> t


------------------------------------------------------------------------------
instance (Monad i, Monad (t i), MonadTrans t) => MonadLayer i t (t i)


------------------------------------------------------------------------------
instance (MonadLayer i s m, MonadInner (s i) (t m), Monad (t m), MonadTrans t)
    => MonadLayer i s (t m)


------------------------------------------------------------------------------
liftT :: MonadLayer i t m => t i a -> m a
liftT = liftI


------------------------------------------------------------------------------
#if LANGUAGE_ConstraintKinds
type MonadLayerControl i t m =
    (MonadInnerControl (t i) m, MonadTransControl t, MonadLayer i t m)
#else
class (MonadInnerControl (t i) m, MonadTransControl t, MonadLayer i t m) =>
    MonadLayerControl i t m
instance (MonadInnerControl (t i) m, MonadTransControl t, MonadLayer i t m) =>
    MonadLayerControl i t m
#endif


------------------------------------------------------------------------------
data Pm (m :: * -> *) = Pm


------------------------------------------------------------------------------
suspendT :: MonadLayerControl i t m
    => m a
    -> OuterState (t i) m
    -> t i (OuterEffects (t i) m a)
suspendT = suspendI


------------------------------------------------------------------------------
resumeT :: forall proxy i t m a. MonadLayerControl i t m
    => proxy t
    -> OuterEffects (t i) m a
    -> m a
resumeT _ = resumeI (Pm :: Pm (t i))


------------------------------------------------------------------------------
captureT :: forall proxy i t m. MonadLayerControl i t m
    => proxy t
    -> m (OuterState (t i) m)
captureT _ = captureI (Pm :: Pm (t i))


------------------------------------------------------------------------------
extractT :: forall proxy proxy' i t m a. MonadLayerControl i t m
    => proxy t
    -> proxy' m
    -> OuterResult (t i) m a
    -> Maybe a
extractT _ = extractI (Pm :: Pm (t i))


------------------------------------------------------------------------------
liftControlT :: MonadLayerControl i t m
    => ((forall b. m b -> t i (OuterEffects (t i) m b)) -> t i a)
    -> m a
liftControlT = liftControlI


------------------------------------------------------------------------------
controlT :: MonadLayerControl i t m
    => ((forall b. m b -> t i (OuterEffects (t i) m b))
        -> t i (OuterEffects (t i) m a))
    -> m a
controlT = controlI


------------------------------------------------------------------------------
liftOpT :: MonadLayerControl i t m
    => ((a -> t i (OuterEffects (t i) m b)) -> t i (OuterEffects (t i) m c))
    -> (a -> m b)
    -> m c
liftOpT = liftOpI


------------------------------------------------------------------------------
liftOpT_ :: MonadLayerControl i t m
    => (t i (OuterEffects (t i) m a) -> t i (OuterEffects (t i) m c))
    -> m a
    -> m c
liftOpT_ = liftOpI_


------------------------------------------------------------------------------
liftDiscardT :: MonadLayerControl i t m => (t i () -> t i a) -> m () -> m a
liftDiscardT = liftDiscardI


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
hoistisoT :: MonadLayerInvariant j n i t m
    => (forall b. t i b -> j b)
    -> (forall b. j b -> t i b)
    -> m a
    -> n a
hoistisoT = hoistisoI


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
hoistT :: MonadLayerFunctor j n i t m
    => (forall b. t i b -> j b)
    -> m a
    -> n a
hoistT = hoistI
