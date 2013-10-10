{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Control.Monad.Lift.Unsafe
    ( defaultPeel'
    , defaultRestore'
    , defaultSuspend'
    , defaultExtract'
    )
where

-- base ----------------------------------------------------------------------
import           Control.Arrow ((***))
import           Control.Monad (liftM)
import           Unsafe.Coerce (unsafeCoerce)


-- layers --------------------------------------------------------------------
import           Control.Monad.Lift
                     ( MonadLiftControl
                     , LiftResult
                     , LiftState
                     , peel'
                     , restore'
                     , suspend'
                     , extract'
                     )


------------------------------------------------------------------------------
defaultPeel' :: MonadLiftControl i m
    => (forall b. n b -> m b)
    -> LiftState i n
    -> n a
    -> i (LiftResult i n a, LiftState i n)
defaultPeel' un s m = liftM (unsafeCoerce *** unsafeCoerce) $
    peel' (unsafeCoerce s) (un m)


------------------------------------------------------------------------------
defaultRestore' :: MonadLiftControl i m
    => (forall b. m b -> n b)
    -> proxy i
    -> LiftResult i n a
    -> LiftState i n
    -> n a
defaultRestore' nu p r s = nu (restore' p (unsafeCoerce r) (unsafeCoerce s))


------------------------------------------------------------------------------
defaultSuspend' :: MonadLiftControl i m
    => (forall b. m b -> n b)
    -> proxy i
    -> n (LiftState i n)
defaultSuspend' nu p = nu (liftM unsafeCoerce (suspend' p))


------------------------------------------------------------------------------
defaultExtract' :: forall proxy i m n a. MonadLiftControl i m
    => (forall b. n b -> m b)
    -> proxy i
    -> proxy n
    -> LiftResult i n a
    -> Maybe a
defaultExtract' _ p p' r =
    extract' p (unsafeCoerce p' :: proxy m) (unsafeCoerce r)
