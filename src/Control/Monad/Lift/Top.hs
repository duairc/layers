{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}

#ifdef LANGUAGE_ConstraintKinds
{-# LANGUAGE ConstraintKinds #-}
#endif

#ifdef LANGUAGE_SafeHaskell
{-# LANGUAGE Safe #-}
#endif

{-|



-}

module Control.Monad.Lift.Top
    ( MonadTop
    , liftT
    , MonadTopControl
    , suspendT
    , resumeT
    , captureT
    , extractT
    , liftControlT
    , controlT
    , liftOpT
    , liftOpT_
    , liftDiscardT
    , MonadTopInvariant
    , hoistisoT
    , MonadTopFunctor
    , hoistT
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
#if LANGUAGE_ConstraintKinds
type MonadTop t m = MonadInner m (t m)
#else
class MonadInner m (t m) => MonadTop t m
instance MonadInner m (t m) => MonadTop t m
#endif


------------------------------------------------------------------------------
liftT :: MonadTop t m => m a -> t m a
liftT = liftI


------------------------------------------------------------------------------
#if LANGUAGE_ConstraintKinds
type MonadTopControl t m = (MonadInnerControl m (t m), MonadTop t m)
#else
class (MonadInnerControl m (t m), MonadTop t m) => MonadTopControl t m
instance (MonadInnerControl m (t m), MonadTop t m) => MonadTopControl t m
#endif


------------------------------------------------------------------------------
data Pm (m :: * -> *) = Pm


------------------------------------------------------------------------------
suspendT :: MonadTopControl t m
    => t m a
    -> OuterState m (t m)
    -> m (OuterEffects m (t m) a)
suspendT = suspendI


------------------------------------------------------------------------------
resumeT :: forall t m a. MonadTopControl t m
    => OuterEffects m (t m) a
    -> t m a
resumeT = resumeI (Pm :: Pm m)


------------------------------------------------------------------------------
captureT :: forall t m. MonadTopControl t m => t m (OuterState m (t m))
captureT = captureI (Pm :: Pm m)


------------------------------------------------------------------------------
extractT :: forall proxy proxy' t m a. MonadTopControl t m
    => proxy t
    -> proxy' m
    -> OuterResult m (t m) a
    -> Maybe a
extractT _ proxy = extractI proxy (Pm :: Pm (t m))


------------------------------------------------------------------------------
liftControlT :: MonadTopControl t m
    => ((forall b. t m b -> m (OuterEffects m (t m) b)) -> m a)
    -> t m a
liftControlT = liftControlI


------------------------------------------------------------------------------
controlT :: MonadTopControl t m
    => ((forall b. t m b -> m (OuterEffects m (t m) b))
        -> m (OuterEffects m (t m) a))
    -> t m a
controlT = controlI


------------------------------------------------------------------------------
liftOpT :: MonadTopControl t m
    => ((a -> m (OuterEffects m (t m) b)) -> m (OuterEffects m (t m) c))
    -> (a -> t m b)
    -> t m c
liftOpT = liftOpI


------------------------------------------------------------------------------
liftOpT_ :: MonadTopControl t m
    => (m (OuterEffects m (t m) a) -> m (OuterEffects m (t m) b))
    -> t m a
    -> t m b
liftOpT_ = liftOpI_


------------------------------------------------------------------------------
liftDiscardT :: MonadTopControl t m => (m () -> m a) -> t m () -> t m a
liftDiscardT = liftDiscardI


------------------------------------------------------------------------------
#if LANGUAGE_ConstraintKinds
type MonadTopInvariant n t m =
    (MonadInnerInvariant n (t n) m (t m), MonadTop t m, MonadTop t n)
#else
class (MonadInnerInvariant n (t n) m (t m), MonadTop t m, MonadTop t n) =>
    MonadTopInvariant n t m
instance (MonadInnerInvariant n (t n) m (t m), MonadTop t m, MonadTop t n) =>
    MonadTopInvariant n t m
#endif


------------------------------------------------------------------------------
hoistisoT :: MonadTopInvariant n t m
    => (forall b. m b -> n b)
    -> (forall b. n b -> m b)
    -> t m a
    -> t n a
hoistisoT = hoistisoI


------------------------------------------------------------------------------
#if LANGUAGE_ConstraintKinds
type MonadTopFunctor n t m =
    ( MonadInnerFunctor n (t n) m (t m)
    , MonadTop t m
    , MonadTop t n
    , MonadTopInvariant n t m
    )
#else
class
    ( MonadInnerFunctor n (t n) m (t m)
    , MonadTop t m
    , MonadTop t n
    , MonadTopInvariant n t m
    )
  =>
    MonadTopFunctor n t m
instance
    ( MonadInnerFunctor n (t n) m (t m)
    , MonadTop t m
    , MonadTop t n
    , MonadTopInvariant n t m
    )
  =>
    MonadTopFunctor n t m
#endif


------------------------------------------------------------------------------
hoistT :: MonadTopFunctor n t m
    => (forall b. m b -> n b)
    -> t m a
    -> t n a
hoistT = hoistI
