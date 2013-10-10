{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}

module Control.Monad.Lift.Unsafe
    ( newtypePeel'
    , newtypeRestore'
    , newtypeSuspend'
    )
where

-- base ----------------------------------------------------------------------
import           Control.Arrow ((***))
import           Control.Monad (liftM)
import           Unsafe.Coerce (unsafeCoerce)


-- layers --------------------------------------------------------------------
import           Control.Monad.Lift
                     ( MonadLiftControl
                     , LiftState
                     , LiftResult
                     , peel'
                     , restore'
                     , suspend'
                     )


------------------------------------------------------------------------------
newtypePeel' :: MonadLiftControl i m
    => (forall b. n b -> m b)
    -> LiftState i n
    -> n a
    -> i (LiftResult i n a, LiftState i n)
newtypePeel' un s m = liftM (unsafeCoerce *** unsafeCoerce) $
    peel' (unsafeCoerce s) (un m)


------------------------------------------------------------------------------
newtypeRestore' :: MonadLiftControl i m
    => (forall b. m b -> n b)
    -> proxy i
    -> LiftResult i n a
    -> LiftState i n
    -> n a
newtypeRestore' nu p r s = nu (restore' p (unsafeCoerce r) (unsafeCoerce s))


------------------------------------------------------------------------------
newtypeSuspend' :: MonadLiftControl i m
    => (forall b. m b -> n b)
    -> proxy i
    -> n (LiftState i n)
newtypeSuspend' nu p = nu (liftM unsafeCoerce (suspend' p))
