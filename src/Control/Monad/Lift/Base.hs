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

{-|



-}

module Control.Monad.Lift.Base
    ( MonadBase
    , liftB
    , MonadBaseControl
    , suspendB
    , resumeB
    , captureB
    , extractB
    , liftControlB
    , controlB
    , liftOpB
    , liftOpB_
    , liftDiscardB
    , MonadBaseInvariant
    , hoistisoB
    , MonadBaseFunctor
    , hoistB
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


-- transformers --------------------------------------------------------------
import           Data.Functor.Identity (Identity)


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
instance (MonadBase b m, MonadInner b (t m)) => MonadBase b (t m)


------------------------------------------------------------------------------
liftB :: MonadBase b m => b a -> m a
liftB = liftI


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
suspendB :: MonadBaseControl b m
    => m a
    -> OuterState b m
    -> b (OuterEffects b m a)
suspendB = suspendI


------------------------------------------------------------------------------
resumeB :: forall b m a. MonadBaseControl b m
    => OuterEffects b m a
    -> m a
resumeB = resumeI (Pm :: Pm b)


------------------------------------------------------------------------------
captureB :: forall b m. MonadBaseControl b m
    => m (OuterState b m)
captureB = captureI (Pm :: Pm b)


------------------------------------------------------------------------------
extractB :: forall proxy b m a. MonadBaseControl b m
    => proxy m
    -> OuterResult b m a
    -> Maybe a
extractB = extractI (Pm :: Pm b)


------------------------------------------------------------------------------
liftControlB :: MonadBaseControl b m
    => ((forall c. m c -> b (OuterEffects b m c)) -> b a)
    -> m a
liftControlB = liftControlI


------------------------------------------------------------------------------
controlB :: MonadBaseControl b m
    => ((forall c. m c -> b (OuterEffects b m c)) -> b (OuterEffects b m a))
    -> m a
controlB = controlI


------------------------------------------------------------------------------
liftOpB :: MonadBaseControl b m
    => ((a -> b (OuterEffects b m c)) -> b (OuterEffects b m d))
    -> (a -> m c)
    -> m d
liftOpB = liftOpI


------------------------------------------------------------------------------
liftOpB_ :: MonadBaseControl b m
    => (b (OuterEffects b m a) -> b (OuterEffects b m c))
    -> m a
    -> m c
liftOpB_ = liftOpI_


------------------------------------------------------------------------------
liftDiscardB :: MonadBaseControl b m
    => (b () -> b a)
    -> m ()
    -> m a
liftDiscardB = liftDiscardI


------------------------------------------------------------------------------
#if LANGUAGE_ConstraintKinds
type MonadBaseInvariant j n b m = (MonadInnerInvariant j n b m, MonadBase b m)
#else
class (MonadInnerInvariant j n b m, MonadBase b m) =>
    MonadBaseInvariant j n b m
        | m -> b
        , j m -> n
        , b j n -> m
        , n m -> j
instance (MonadInnerInvariant j n b m, MonadBase b m) =>
    MonadBaseInvariant j n b m
#endif


------------------------------------------------------------------------------
hoistisoB :: MonadBaseInvariant j n b m
    => (forall c. b c -> j c)
    -> (forall c. j c -> b c)
    -> m a
    -> n a
hoistisoB = hoistisoI


------------------------------------------------------------------------------
#if LANGUAGE_ConstraintKinds
type MonadBaseFunctor j n b m = (MonadInnerFunctor j n b m, MonadBase b m)
#else
class (MonadInnerFunctor j n b m, MonadBase b m) =>
    MonadBaseFunctor j n b m
        | m -> b
        , j m -> n
        , b j n -> m
        , n m -> j
instance (MonadInnerFunctor j n b m, MonadBase b m) =>
    MonadBaseFunctor j n b m
#endif


------------------------------------------------------------------------------
hoistB :: MonadBaseFunctor j n b m => (forall c. b c -> j c) -> m a -> n a
hoistB = hoistI
