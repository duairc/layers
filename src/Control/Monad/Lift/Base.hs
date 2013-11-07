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

module Control.Monad.Lift.Base
    ( MonadBase
    , liftBase
    , MonadBaseControl
    , suspendBase
    , resumeBase
    , captureBase
    , extractBase
    , liftBaseControl
    , controlBase
    , liftBaseOp
    , liftBaseOp_
    , liftBaseDiscard
    , MonadBaseInvariant
    , hoistisoBase
    , MonadBaseFunctor
    , hoistBase
    )
where

-- base ----------------------------------------------------------------------
import           Control.Monad.ST (ST)
import qualified Control.Monad.ST.Lazy as L (ST)
#if MIN_VERSION_base(4, 7, 0)
import           Data.Proxy (Proxy)
#endif
#if MIN_VERSION_base(4, 3, 0)
import           GHC.Conc.Sync (STM)
#else
import           GHC.Conc (STM)
#endif


-- transformers --------------------------------------------------------------
import           Data.Functor.Identity (Identity)


-- layers --------------------------------------------------------------------
import           Control.Monad.Lift
                     ( MonadTrans
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
class MonadInner b m => MonadBase b m | m -> b


------------------------------------------------------------------------------
instance MonadBase Identity Identity


------------------------------------------------------------------------------
instance MonadBase Maybe Maybe


------------------------------------------------------------------------------
instance MonadBase (Either e) (Either e)


------------------------------------------------------------------------------
instance MonadBase [] []


------------------------------------------------------------------------------
instance MonadBase ((->) r) ((->) r)


------------------------------------------------------------------------------
instance MonadBase IO IO


------------------------------------------------------------------------------
instance MonadBase (ST s) (ST s)


------------------------------------------------------------------------------
instance MonadBase (L.ST s) (L.ST s)


------------------------------------------------------------------------------
instance MonadBase STM STM


#if MIN_VERSION_base(4, 7, 0)
------------------------------------------------------------------------------
instance MonadBase Proxy Proxy


#endif
------------------------------------------------------------------------------
instance (MonadTrans t, MonadBase b m, MonadInner b (t m), Monad (t m)) =>
    MonadBase b (t m)


------------------------------------------------------------------------------
liftBase :: MonadBase b m => b a -> m a
liftBase = liftI


------------------------------------------------------------------------------
#if LANGUAGE_ConstraintKinds
type MonadBaseControl b m = (MonadInnerControl b m, MonadBase b m)
#else
class (MonadInnerControl b m, MonadBase b m) => MonadBaseControl b m | m -> b
instance (MonadInnerControl b m, MonadBase b m) => MonadBaseControl b m
#endif


------------------------------------------------------------------------------
data Pm (m :: * -> *) = Pm


------------------------------------------------------------------------------
suspendBase :: MonadBaseControl b m => m a -> OuterState b m -> b (OuterEffects b m a)
suspendBase = suspendI


------------------------------------------------------------------------------
resumeBase :: forall b m a. MonadBaseControl b m => OuterEffects b m a -> m a
resumeBase = resumeI (Pm :: Pm b)


------------------------------------------------------------------------------
captureBase :: forall b m. MonadBaseControl b m => m (OuterState b m)
captureBase = captureI (Pm :: Pm b)


------------------------------------------------------------------------------
extractBase :: forall proxy b m a. MonadBaseControl b m => proxy m -> OuterResult b m a -> Maybe a
extractBase = extractI (Pm :: Pm b)


------------------------------------------------------------------------------
liftBaseControl :: MonadBaseControl b m => ((forall c. m c -> b (OuterEffects b m c)) -> b a) -> m a
liftBaseControl = liftControlI


------------------------------------------------------------------------------
controlBase :: MonadBaseControl b m => ((forall c. m c -> b (OuterEffects b m c)) -> b (OuterEffects b m a)) -> m a
controlBase = controlI


------------------------------------------------------------------------------
liftBaseOp :: MonadBaseControl b m => ((a -> b (OuterEffects b m c)) -> b (OuterEffects b m d)) -> (a -> m c) -> m d
liftBaseOp = liftOpI


------------------------------------------------------------------------------
liftBaseOp_ :: MonadBaseControl b m => (b (OuterEffects b m a) -> b (OuterEffects b m c)) -> m a -> m c
liftBaseOp_ = liftOpI_


------------------------------------------------------------------------------
liftBaseDiscard :: MonadBaseControl b m => (b () -> b a) -> m () -> m a
liftBaseDiscard = liftDiscardI


------------------------------------------------------------------------------
#if LANGUAGE_ConstraintKinds
type MonadBaseInvariant b m = (MonadInnerInvariant b m, MonadBase b m)
#else
class (MonadInnerInvariant b m, MonadBase b m) => MonadBaseInvariant b m | m -> b
instance (MonadInnerInvariant b m, MonadBase b m) => MonadBaseInvariant b m
#endif


------------------------------------------------------------------------------
hoistisoBase :: MonadBaseInvariant b m => (forall c. b c -> b c) -> (forall c. b c -> b c) -> m a -> m a
hoistisoBase = hoistisoI


------------------------------------------------------------------------------
#if LANGUAGE_ConstraintKinds
type MonadBaseFunctor b m = (MonadInnerFunctor b m, MonadBase b m)
#else
class (MonadInnerFunctor b m, MonadBase b m) => MonadBaseFunctor b m | m -> b
instance (MonadInnerFunctor b m, MonadBase b m) => MonadBaseFunctor b m
#endif


------------------------------------------------------------------------------
hoistBase :: MonadBaseFunctor b m => (forall c. b c -> b c) -> m a -> m a
hoistBase = hoistI
