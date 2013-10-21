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
    , peelBase
    , restoreBase
    , suspendBase
    , extractBase
    , resultBase
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
                     , MonadLift
                     , lift'
                     , MonadLiftControl
                     , Lift
                     , LiftResult
                     , LiftState
                     , peel'
                     , restore'
                     , suspend'
                     , extract'
                     , result'
                     , liftControl'
                     , control'
                     , liftOp'
                     , liftOp_'
                     , liftDiscard'
                     , MonadLiftInvariant
                     , hoistiso'
                     , MonadLiftFunctor
                     , hoist'
                     )


------------------------------------------------------------------------------
class MonadLift b m => MonadBase b m | m -> b


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
instance (MonadTrans t, MonadBase b m, MonadLift b (t m), Monad (t m)) =>
    MonadBase b (t m)


------------------------------------------------------------------------------
liftBase :: MonadBase b m => b a -> m a
liftBase = lift'


------------------------------------------------------------------------------
#if LANGUAGE_ConstraintKinds
type MonadBaseControl b m = (MonadLiftControl b m, MonadBase b m)
#else
class (MonadLiftControl b m, MonadBase b m) => MonadBaseControl b m | m -> b
instance (MonadLiftControl b m, MonadBase b m) => MonadBaseControl b m
#endif


------------------------------------------------------------------------------
data Pm (m :: * -> *) = Pm


------------------------------------------------------------------------------
peelBase :: MonadBaseControl b m => m a -> LiftState b m -> b (Lift b m a)
peelBase = peel'


------------------------------------------------------------------------------
restoreBase :: forall b m a. MonadBaseControl b m => Lift b m a -> m a
restoreBase = restore' (Pm :: Pm b)


------------------------------------------------------------------------------
suspendBase :: forall b m. MonadBaseControl b m => m (LiftState b m)
suspendBase = suspend' (Pm :: Pm b)


------------------------------------------------------------------------------
extractBase :: forall proxy b m a. MonadBaseControl b m => proxy m -> LiftResult b m a -> Maybe a
extractBase = extract' (Pm :: Pm b)


------------------------------------------------------------------------------
resultBase :: forall b m a. MonadBaseControl b m => m a -> m (LiftResult b m a)
resultBase = result' (Pm :: Pm b)


------------------------------------------------------------------------------
liftBaseControl :: MonadBaseControl b m => ((forall c. m c -> b (Lift b m c)) -> b a) -> m a
liftBaseControl = liftControl'


------------------------------------------------------------------------------
controlBase :: MonadBaseControl b m => ((forall c. m c -> b (Lift b m c)) -> b (Lift b m a)) -> m a
controlBase = control'


------------------------------------------------------------------------------
liftBaseOp :: MonadBaseControl b m => ((a -> b (Lift b m c)) -> b (Lift b m d)) -> (a -> m c) -> m d
liftBaseOp = liftOp'


------------------------------------------------------------------------------
liftBaseOp_ :: MonadBaseControl b m => (b (Lift b m a) -> b (Lift b m c)) -> m a -> m c
liftBaseOp_ = liftOp_'


------------------------------------------------------------------------------
liftBaseDiscard :: MonadBaseControl b m => (b () -> b a) -> m () -> m a
liftBaseDiscard = liftDiscard'


------------------------------------------------------------------------------
#if LANGUAGE_ConstraintKinds
type MonadBaseInvariant b m = (MonadLiftInvariant b m, MonadBase b m)
#else
class (MonadLiftInvariant b m, MonadBase b m) => MonadBaseInvariant b m | m -> b
instance (MonadLiftInvariant b m, MonadBase b m) => MonadBaseInvariant b m
#endif


------------------------------------------------------------------------------
hoistisoBase :: MonadBaseInvariant b m => (forall c. b c -> b c) -> (forall c. b c -> b c) -> m a -> m a
hoistisoBase = hoistiso'


------------------------------------------------------------------------------
#if LANGUAGE_ConstraintKinds
type MonadBaseFunctor b m = (MonadLiftFunctor b m, MonadBase b m)
#else
class (MonadLiftFunctor b m, MonadBase b m) => MonadBaseFunctor b m | m -> b
instance (MonadLiftFunctor b m, MonadBase b m) => MonadBaseFunctor b m
#endif


------------------------------------------------------------------------------
hoistBase :: MonadBaseFunctor b m => (forall c. b c -> b c) -> m a -> m a
hoistBase = hoist'
