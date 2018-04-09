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

#ifdef LANGUAGE_SafeHaskell
{-# LANGUAGE Trustworthy #-}
#endif

#include "newtypec.h"
#include "overlap.h"

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
import           Control.Monad.Lift.Internal (coercePeelI)


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
newtypeC(MonadLayerControl i t m,
    ( MonadInnerControl (t i) m
    , MonadLayer i t m
    ))


------------------------------------------------------------------------------
data Pm (m :: * -> *) = Pm


------------------------------------------------------------------------------
suspendL :: MonadLayerControl i t m
    => m a
    -> OuterState (t i) m
    -> t i (OuterEffects (t i) m a)
suspendL = suspendI


------------------------------------------------------------------------------
resumeL :: forall i t m a proxy. MonadLayerControl i t m
    => proxy t
    -> OuterEffects (t i) m a
    -> m a
resumeL _ = resumeI (Pm :: Pm (t i))


------------------------------------------------------------------------------
captureL :: forall i t m proxy. MonadLayerControl i t m
    => proxy t
    -> m (OuterState (t i) m)
captureL _ = captureI (Pm :: Pm (t i))


------------------------------------------------------------------------------
extractL :: forall i t m a b proxy proxy'. MonadLayerControl i t m
    => proxy t
    -> proxy' m
    -> OuterResult (t i) m a
    -> Either (OuterResult (t i) m b) a
extractL _ = extractI (Pm :: Pm (t i))


------------------------------------------------------------------------------
liftControlL :: MonadLayerControl i t m
    => ((forall b. m b -> t i (OuterEffects (t i) m b)) -> t i a)
    -> m a
liftControlL f = liftControlI (\peel -> f (coercePeelI peel))


------------------------------------------------------------------------------
controlL :: MonadLayerControl i t m
    => ((forall b. m b -> t i (OuterEffects (t i) m b))
        -> t i (OuterEffects (t i) m a))
    -> m a
controlL f = controlI (\peel -> f (coercePeelI peel))


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
newtypeC(MonadLayerInvariant j n i t m,
    ( MonadInnerInvariant j n (t i) m
    , MonadLayer i t m
    ))


------------------------------------------------------------------------------
hoistisoL :: MonadLayerInvariant j n i t m
    => (forall b. t i b -> j b)
    -> (forall b. j b -> t i b)
    -> m a
    -> n a
hoistisoL = hoistisoI


------------------------------------------------------------------------------
newtypeC(MonadLayerFunctor j n i t m,
    ( MonadInnerFunctor j n (t i) m
    , MonadLayer i t m
    , MonadLayerInvariant j n i t m
    ))


------------------------------------------------------------------------------
hoistL :: MonadLayerFunctor j n i t m
    => (forall b. t i b -> j b)
    -> m a
    -> n a
hoistL = hoistI
