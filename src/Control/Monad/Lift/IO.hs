{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE UndecidableInstances #-}
#ifdef LANGUAGE_ConstraintKinds
{-# LANGUAGE ConstraintKinds #-}
#endif

#include "macros.h"

{-|

This module defines the 'MonadIO' family of interfaces, which consist of:

    * 'MonadIO'

    * 'MonadControlIO'

    * 'MInvariantIO'

    * 'MFunctorIO'

All the constraints and operations in the 'MonadIO' family of interfaces are
exactly identical to constraints and operations from the 'MonadInner' family,
except that the type of the inner monad is fixed to 'IO'. In many cases the
operations in the 'MonadInner' family are too polymorphic to be usable without
type signatures everywhere, so if you know that the monad from which you want
to lift is definitely 'IO', you will have a much easier time if you use these
operations.

(The point of 'MonadInner' is that it only needs to be written once, and then
'MonadIO', 'Control.Monad.Lift.Base.MonadBase' and all the rest come for free.
With H(transformers) and H(transformers-base), instances of
'Control.Monad.IO.Class.MonadIO' and
@<http://hackage.haskell.org/package/transformers-base/docs/Control-Monad-Base.html#t:MonadBase MonadBase>@
have to be manually written for every monad transformer.)

-}

module Control.Monad.Lift.IO
    ( MonadIO
    , liftIO
    , MonadControlIO
    , suspendIO
    , resumeIO
    , captureIO
    , extractIO
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
-- | The constraint @'MonadIO' m@ holds when 'IO' is an inner monad of @m@
-- such that it is possible to lift computations in 'IO' into @m@ using
-- 'liftIO'.
--
-- It is neither possible nor necessary to manually write instances of
-- 'MonadIO'. It's simply a constraint synonym for @'MonadInner' 'IO'@. Any
-- monad built from stack of monad transformers with 'IO' at its
-- <Control-Monad-Lift-Base.html base> (or indeed any \"base\" monad @m@ that
-- satisfies the constraint @'MonadInner' 'IO' m@) is automatically an instance
-- of 'MonadIO'.
#if LANGUAGE_ConstraintKinds
type MonadIO = MonadInner IO
#else
class MonadInner IO m => MonadIO m
instance MonadInner IO m => MonadIO m
#endif


------------------------------------------------------------------------------
liftIO :: MonadIO m => IO a -> m a
liftIO = liftI


------------------------------------------------------------------------------
-- | The constraint @'MonadControlIO' m@ holds when 'IO' is an inner monad of @m@
-- such that it is possible to lift computations in 'IO' into @m@ using
-- 'liftIO'.
--
-- It is neither possible nor necessary to manually write instances of
-- 'MonadIO'. It's simply a constraint synonym for @'MonadInner' 'IO'@. Any
-- monad built from stack of monad transformers with 'IO' at its
-- <Control-Monad-Lift-Base.html base> (or indeed any \"base\" monad @m@ that
-- satisfies the constraint @'MonadInner' 'IO' m@) is automatically an instance
-- of 'MonadIO'.
#if LANGUAGE_ConstraintKinds
type MonadControlIO = MonadInnerControl IO
#else
class MonadInnerControl IO m => MonadControlIO m
instance MonadInnerControl IO m => MonadControlIO m
#endif


------------------------------------------------------------------------------
data Pm (m :: * -> *) = Pm


------------------------------------------------------------------------------
suspendIO :: MonadControlIO m => m a -> OuterState IO m -> IO (OuterEffects IO m a)
suspendIO = suspendI


------------------------------------------------------------------------------
resumeIO :: MonadControlIO m => OuterEffects IO m a -> m a
resumeIO = resumeI (Pm :: Pm IO)


------------------------------------------------------------------------------
captureIO :: MonadControlIO m => m (OuterState IO m)
captureIO = captureI (Pm :: Pm IO)


------------------------------------------------------------------------------
extractIO :: MonadControlIO m => proxy m -> OuterResult IO m a -> Maybe a
extractIO = extractI (Pm :: Pm IO)


------------------------------------------------------------------------------
liftControlIO :: MonadControlIO m => ((forall b. m b -> IO (OuterEffects IO m b)) -> IO a) -> m a
liftControlIO = liftControlI


------------------------------------------------------------------------------
controlIO :: MonadControlIO m => ((forall b. m b -> IO (OuterEffects IO m b)) -> IO (OuterEffects IO m a)) -> m a
controlIO = controlI


------------------------------------------------------------------------------
liftIOOp :: MonadControlIO m => ((a -> IO (OuterEffects IO m b)) -> IO (OuterEffects IO m c)) -> (a -> m b) -> m c
liftIOOp = liftOpI


------------------------------------------------------------------------------
liftIOOp_ :: MonadControlIO m => (IO (OuterEffects IO m a) -> IO (OuterEffects IO m b)) -> m a -> m b
liftIOOp_ = liftOpI_


------------------------------------------------------------------------------
liftIODiscard :: MonadControlIO m => (IO () -> IO a) -> m () -> m a
liftIODiscard = liftDiscardI


------------------------------------------------------------------------------
#if LANGUAGE_ConstraintKinds
type MInvariantIO = MonadInnerInvariant IO
#else
class MonadInnerInvariant IO m => MInvariantIO m
instance MonadInnerInvariant IO m => MInvariantIO m
#endif


------------------------------------------------------------------------------
hoistisoIO :: MInvariantIO m => (forall b. IO b -> IO b) -> (forall b. IO b -> IO b) -> m a -> m a
hoistisoIO = hoistisoI


------------------------------------------------------------------------------
#if LANGUAGE_ConstraintKinds
type MFunctorIO = MonadInnerFunctor IO
#else
class MonadInnerFunctor IO m => MFunctorIO m
instance MonadInnerFunctor IO m => MFunctorIO m
#endif


------------------------------------------------------------------------------
hoistIO :: MFunctorIO m => (forall b. IO b -> IO b) -> m a -> m a
hoistIO = hoistI
