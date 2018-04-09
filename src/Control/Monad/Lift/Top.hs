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

#ifdef LANGUAGE_SafeHaskell
{-# LANGUAGE Trustworthy #-}
#endif

#include "newtypec.h"

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
import           Control.Monad.Lift.Internal (coercePeelI)


------------------------------------------------------------------------------
newtypeC(MonadTop t m, MonadInner m (t m))


------------------------------------------------------------------------------
liftT :: MonadTop t m => m a -> t m a
liftT = liftI


------------------------------------------------------------------------------
newtypeC(MonadTopControl t m, (MonadInnerControl m (t m), MonadTop t m))


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
extractT :: forall t m a b proxy proxy'. MonadTopControl t m
    => proxy t
    -> proxy' m
    -> OuterResult m (t m) a
    -> Either (OuterResult m (t m) b) a
extractT _ proxy = extractI proxy (Pm :: Pm (t m))


------------------------------------------------------------------------------
liftControlT :: MonadTopControl t m
    => ((forall b. t m b -> m (OuterEffects m (t m) b)) -> m a)
    -> t m a
liftControlT f = liftControlI (\peel -> f (coercePeelI peel))


------------------------------------------------------------------------------
controlT :: MonadTopControl t m
    => ((forall b. t m b -> m (OuterEffects m (t m) b))
        -> m (OuterEffects m (t m) a))
    -> t m a
controlT f = controlI (\peel -> f (coercePeelI peel))


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
newtypeC(MonadTopInvariant n t m,
    ( MonadInnerInvariant n (t n) m (t m)
    , MonadTop t m
    , MonadTop t n
    ))


------------------------------------------------------------------------------
hoistisoT :: MonadTopInvariant n t m
    => (forall b. m b -> n b)
    -> (forall b. n b -> m b)
    -> t m a
    -> t n a
hoistisoT = hoistisoI


------------------------------------------------------------------------------
newtypeC(MonadTopFunctor n t m,
    ( MonadInnerFunctor n (t n) m (t m)
    , MonadTop t m
    , MonadTop t n
    , MonadTopInvariant n t m
    ))


------------------------------------------------------------------------------
hoistT :: MonadTopFunctor n t m
    => (forall b. m b -> n b)
    -> t m a
    -> t n a
hoistT = hoistI
