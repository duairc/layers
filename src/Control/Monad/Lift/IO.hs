{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE UndecidableInstances #-}

#ifdef LANGUAGE_SafeHaskell
{-# LANGUAGE Trustworthy #-}
#endif

#include "docmacros.h"
#include "newtypec.h"

{-|

This module defines the 'MonadIO' family of interfaces, which consist of:

    * 'MonadIO'

    * 'MonadIOControl'

    * 'MonadIOInvariant'

    * 'MonadIOFunctor'

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
T(transformers-base,Control-Monad-Base,MonadBase) have to be manually written
for every monad transformer.)

-}

module Control.Monad.Lift.IO
    ( MonadIO
    , liftIO
    , MonadIOControl
    , suspendIO
    , resumeIO
    , captureIO
    , extractIO
    , liftControlIO
    , controlIO
    , liftOpIO
    , liftOpIO_
    , liftDiscardIO
    , MonadIOInvariant
    , hoistisoIO
    , MonadIOFunctor
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
import           Control.Monad.Lift.Internal (coercePeelI)


------------------------------------------------------------------------------
-- | The constraint @'MonadIO' m@ holds when 'IO' is an inner monad of @m@
-- such that it is possible to lift computations in 'IO' into @m@ using
-- 'liftIO'.
--
-- It is neither possible nor necessary to manually write instances of
-- 'MonadIO'. It's simply a constraint synonym for @'MonadInner' 'IO'@. Any
-- monad built from stack of monad transformers with 'IO' at its
-- <Control-Monad-Lift-Base.html base> (or indeed any \"base\" monad @m@ that
-- satisfies the constraint @'MonadInner' 'IO' m@) is isomatically an instance
-- of 'MonadIO'.
newtypeC(MonadIO m, MonadInner IO m)


------------------------------------------------------------------------------
liftIO :: MonadIO m => IO a -> m a
liftIO = liftI


------------------------------------------------------------------------------
-- | The constraint @'MonadIOControl' m@ holds when 'IO' is an inner monad of
-- @m@ such that it is possible to lift computations in 'IO' into @m@ using
-- 'liftIO'.
--
-- It is neither possible nor necessary to manually write instances of
-- 'MonadIO'. It's simply a constraint synonym for @'MonadInner' 'IO'@. Any
-- monad built from stack of monad transformers with 'IO' at its
-- <Control-Monad-Lift-Base.html base> (or indeed any \"base\" monad @m@ that
-- satisfies the constraint @'MonadInner' 'IO' m@) is isomatically an instance
-- of 'MonadIO'.
newtypeC(MonadIOControl m, MonadInnerControl IO m)


------------------------------------------------------------------------------
data Pm (m :: * -> *) = Pm


------------------------------------------------------------------------------
suspendIO :: MonadIOControl m
    => m a
    -> OuterState IO m
    -> IO (OuterEffects IO m a)
suspendIO = suspendI


------------------------------------------------------------------------------
resumeIO :: MonadIOControl m
    => OuterEffects IO m a
    -> m a
resumeIO = resumeI (Pm :: Pm IO)


------------------------------------------------------------------------------
captureIO :: MonadIOControl m
    => m (OuterState IO m)
captureIO = captureI (Pm :: Pm IO)


------------------------------------------------------------------------------
extractIO :: MonadIOControl m => proxy m -> OuterResult IO m a -> Maybe a
extractIO = extractI (Pm :: Pm IO)


------------------------------------------------------------------------------
liftControlIO :: MonadIOControl m
    => ((forall b. m b -> IO (OuterEffects IO m b)) -> IO a)
    -> m a
liftControlIO f = liftControlI (\peel -> f (coercePeelI peel))


------------------------------------------------------------------------------
controlIO :: MonadIOControl m
    => ((forall b. m b -> IO (OuterEffects IO m b))
        -> IO (OuterEffects IO m a))
    -> m a
controlIO f = controlI (\peel -> f (coercePeelI peel))


------------------------------------------------------------------------------
liftOpIO :: MonadIOControl m
    => ((a -> IO (OuterEffects IO m b)) -> IO (OuterEffects IO m c))
    -> (a -> m b)
    -> m c
liftOpIO = liftOpI


------------------------------------------------------------------------------
liftOpIO_ :: MonadIOControl m
    => (IO (OuterEffects IO m a) -> IO (OuterEffects IO m b))
    -> m a
    -> m b
liftOpIO_ = liftOpI_


------------------------------------------------------------------------------
liftDiscardIO :: MonadIOControl m => (IO () -> IO a) -> m () -> m a
liftDiscardIO = liftDiscardI


------------------------------------------------------------------------------
newtypeC(MonadIOInvariant j n m, MonadInnerInvariant j n IO m)


------------------------------------------------------------------------------
hoistisoIO :: MonadIOInvariant j n m
    => (forall b. IO b -> j b)
    -> (forall b. j b -> IO b)
    -> m a
    -> n a
hoistisoIO = hoistisoI


------------------------------------------------------------------------------
newtypeC(MonadIOFunctor j n m, MonadInnerFunctor j n IO m)


------------------------------------------------------------------------------
hoistIO :: MonadIOFunctor j n m => (forall b. IO b -> j b) -> m a -> n a
hoistIO = hoistI
