{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE UndecidableInstances #-}
#ifdef LANGUAGE_ConstraintKinds
{-# LANGUAGE ConstraintKinds #-}
#endif

{-|

This module defines the 'MonadIO' family of interfaces, which consist of:

    * 'MonadIO'

    * 'MonadControlIO'

    * 'MInvariantIO'

    * 'MFunctorIO'

All the constraints and operations in the 'MonadIO' family of interfaces are
exactly identical to constraints and operations from the 'MonadLift' family,
except that the type of the inner monad is fixed to 'IO'. In many cases the
operations in the 'MonadLift' family are too polymorphic to be usable without
type signatures everywhere, so if you know that the monad from which you want
to lift is definitely 'IO', you will have a much easier time if you use these
operations.

(The point of 'MonadLift' is that it only needs to be written once, and then
'MonadIO', 'Control.Monad.Lift.Base.MonadBase' and all the rest come for free.
With @<http://hackage.haskell.org/package/transformers transformers>@ and
@<http://hackage.haskell.org/package/transformers-base transformers-base>@,
instances of 'Control.Monad.IO.Class.MonadIO' and
@<http://hackage.haskell.org/package/transformers-base/docs/Control-Monad-Base.html#t:MonadBase MonadBase>@
have to be manually written for every monad transformer.)

-}

module Control.Monad.Lift.IO
    ( MonadIO
    , liftIO
    , MonadControlIO
    , peelIO
    , restoreIO
    , suspendIO
    , extractIO
    , resultIO
    , liftControlIO
    , controlIO
    , liftIOOp
    , liftIOOp_
    , liftIODiscard
    , MInvariantIO
    , hoistisoIO
    , MFunctorIO
    , hoistIO
    )
where

-- layers --------------------------------------------------------------------
import           Control.Monad.Lift
                     ( MonadLift
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
-- | The constraint @'MonadIO' m@ holds when 'IO' is an inner monad of @m@
-- such that it is possible to lift computations in 'IO' into @m@ using
-- 'liftIO'.
--
-- It is neither possible nor necessary to manually write instances of
-- 'MonadIO'. It's simply a constraint synonym for @'MonadLift' 'IO'@. Any
-- monad built from stack of monad transformers with 'IO' at its
-- <Control-Monad-Lift-Base.html base> (or indeed any \"base\" monad @m@ that
-- satisfies the constraint @'MonadLift' 'IO' m@) is automatically an instance
-- of 'MonadIO'.
#if LANGUAGE_ConstraintKinds
type MonadIO = MonadLift IO
#else
class MonadLift IO m => MonadIO m
instance MonadLift IO m => MonadIO m
#endif


------------------------------------------------------------------------------
liftIO :: MonadIO m => IO a -> m a
liftIO = lift'


------------------------------------------------------------------------------
-- | The constraint @'MonadControlIO' m@ holds when 'IO' is an inner monad of @m@
-- such that it is possible to lift computations in 'IO' into @m@ using
-- 'liftIO'.
--
-- It is neither possible nor necessary to manually write instances of
-- 'MonadIO'. It's simply a constraint synonym for @'MonadLift' 'IO'@. Any
-- monad built from stack of monad transformers with 'IO' at its
-- <Control-Monad-Lift-Base.html base> (or indeed any \"base\" monad @m@ that
-- satisfies the constraint @'MonadLift' 'IO' m@) is automatically an instance
-- of 'MonadIO'.
#if LANGUAGE_ConstraintKinds
type MonadControlIO = MonadLiftControl IO
#else
class MonadLiftControl IO m => MonadControlIO m
instance MonadLiftControl IO m => MonadControlIO m
#endif


------------------------------------------------------------------------------
data Pm (m :: * -> *) = Pm


------------------------------------------------------------------------------
peelIO :: MonadControlIO m => m a -> LiftState IO m -> IO (Lift IO m a)
peelIO = peel'


------------------------------------------------------------------------------
restoreIO :: MonadControlIO m => Lift IO m a -> m a
restoreIO = restore' (Pm :: Pm IO)


------------------------------------------------------------------------------
suspendIO :: MonadControlIO m => m (LiftState IO m)
suspendIO = suspend' (Pm :: Pm IO)


------------------------------------------------------------------------------
extractIO :: MonadControlIO m => proxy m -> LiftResult IO m a -> Maybe a
extractIO = extract' (Pm :: Pm IO)


------------------------------------------------------------------------------
resultIO :: MonadControlIO m => m a -> m (LiftResult IO m a)
resultIO = result' (Pm :: Pm IO)


------------------------------------------------------------------------------
liftControlIO :: MonadControlIO m => ((forall b. m b -> IO (Lift IO m b)) -> IO a) -> m a
liftControlIO = liftControl'


------------------------------------------------------------------------------
controlIO :: MonadControlIO m => ((forall b. m b -> IO (Lift IO m b)) -> IO (Lift IO m a)) -> m a
controlIO = control'


------------------------------------------------------------------------------
liftIOOp :: MonadControlIO m => ((a -> IO (Lift IO m b)) -> IO (Lift IO m c)) -> (a -> m b) -> m c
liftIOOp = liftOp'


------------------------------------------------------------------------------
liftIOOp_ :: MonadControlIO m => (IO (Lift IO m a) -> IO (Lift IO m b)) -> m a -> m b
liftIOOp_ = liftOp_'


------------------------------------------------------------------------------
liftIODiscard :: MonadControlIO m => (IO () -> IO a) -> m () -> m a
liftIODiscard = liftDiscard'


------------------------------------------------------------------------------
#if LANGUAGE_ConstraintKinds
type MInvariantIO = MonadLiftInvariant IO
#else
class MonadLiftInvariant IO m => MInvariantIO m
instance MonadLiftInvariant IO m => MInvariantIO m
#endif


------------------------------------------------------------------------------
hoistisoIO :: MInvariantIO m => (forall b. IO b -> IO b) -> (forall b. IO b -> IO b) -> m a -> m a
hoistisoIO = hoistiso'


------------------------------------------------------------------------------
#if LANGUAGE_ConstraintKinds
type MFunctorIO = MonadLiftFunctor IO
#else
class MonadLiftFunctor IO m => MFunctorIO m
instance MonadLiftFunctor IO m => MFunctorIO m
#endif


------------------------------------------------------------------------------
hoistIO :: MFunctorIO m => (forall b. IO b -> IO b) -> m a -> m a
hoistIO = hoist'
