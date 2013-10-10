{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

{-|

This module defines the 'MonadMask' interface, which consists of:

    * 'MonadMask' :: @(* -> *) -> Constraint@

    * 'getMaskingState' :: @MonadMask m => m MaskingState@

    * 'setMaskingState' :: @MonadMask m => MaskingState -> m a -> m a@

    * 'mask' :: @MonadMask m =>
        ((forall a n). MonadMask n => n a -> n a) -> m b) -> m b@

    * 'mask_' :: @MonadMask m => m a -> m a@

    * 'uninterruptibleMask' :: @MonadMask m =>
        ((forall a n). MonadMask n => n a -> n a) -> m b) -> m b@

    * 'uninterruptibleMask_' :: @MonadMask m => m a -> m a@

The 'MonadMask' interface is designed for compatibility with
"Control.Exception".

-}

module Monad.Mask
    ( MonadMask (getMaskingState, setMaskingState)
    , mask
    , mask_
    , uninterruptibleMask
    , uninterruptibleMask_
    )
where

-- base ----------------------------------------------------------------------
import           Control.Exception
                     ( MaskingState
                         ( Unmasked
                         , MaskedInterruptible
                         , MaskedUninterruptible
                         )
                     )
import qualified Control.Exception as E (getMaskingState)
import           Control.Monad.ST (ST)
import qualified Control.Monad.ST.Lazy as L (ST)
import           GHC.Conc.Sync (STM)
import           GHC.Base
                     ( maskAsyncExceptions#
                     , maskUninterruptible#
                     , unmaskAsyncExceptions#
                     )
import           GHC.IO (IO (IO))


-- transformers --------------------------------------------------------------
import           Data.Functor.Identity (Identity)
import           Data.Functor.Product (Product (Pair))


-- layers --------------------------------------------------------------------
import           Control.Monad.Lift (MonadTrans, MInvariant, lift, hoistiso)


------------------------------------------------------------------------------
-- | The 'MonadMask' type class is for dealing with asynchronous exceptions.
-- It contains the 'getMaskingState' and 'setMaskingState' operations for
-- getting and setting the 'MaskingState' of the current thread. However, you
-- should never need to use these operations: in particular, using
-- @setMaskingState@ can violate some invariants which are assumed internally
-- by this library. The only reason these functions are exposed at all is that
-- they are necessary to implement 'mask' (which is what you should use
-- instead), and unlike 'mask', their simpler type signature allows us to
-- define a universal pass-through instance of @MonadMask@ through any
-- 'MonadLayer', while @mask@ would require
-- 'Control.Monad.Layer.MonadLayerControl'.
--
-- /Every/ monad should be an instance of @MonadMask@, and we have provided
-- instances for every base monad in the @base@ and @transformers@ packages.
-- @getMaskingState@ and @setMaskingState@ have default definitions that only
-- need to be overridden in the case of 'IO' and monads layered on top of @IO@
-- (which we have already done), so it costs nothing to add an instance of
-- @MonadMask@ to a monad. (@MonadMask@ is a prerequisite for implementing
-- 'Monad.Try.MonadTry', which provides the
-- 'Monad.Try.bracket' family of functions, which is perhaps
-- more interesting than @MonadMask@ on its own.)
--
-- Minimal complete definition: instance head only.
class Monad m => MonadMask m where
    -- | Returns the 'MaskingState' for the current thread.
    getMaskingState :: m MaskingState

    -- | Sets the 'MaskingState' for the current thread to the given value.
    setMaskingState :: MaskingState -> m a -> m a

    getMaskingState = return MaskedInterruptible
    setMaskingState = const id


------------------------------------------------------------------------------
instance MonadMask Identity


------------------------------------------------------------------------------
instance (MonadMask f, MonadMask g) => MonadMask (Product f g) where
    getMaskingState = Pair getMaskingState getMaskingState
    setMaskingState s (Pair f g)
        = Pair (setMaskingState s f) (setMaskingState s g)


------------------------------------------------------------------------------
instance MonadMask Maybe


------------------------------------------------------------------------------
instance MonadMask (Either e)


------------------------------------------------------------------------------
instance MonadMask []


------------------------------------------------------------------------------
instance MonadMask ((->) r)


------------------------------------------------------------------------------
instance MonadMask (ST s)


------------------------------------------------------------------------------
instance MonadMask (L.ST s)


------------------------------------------------------------------------------
instance MonadMask STM


------------------------------------------------------------------------------
instance MonadMask IO where
    getMaskingState = E.getMaskingState
    setMaskingState Unmasked (IO i) = IO $ unmaskAsyncExceptions# i
    setMaskingState MaskedInterruptible (IO i) = IO $ maskAsyncExceptions# i
    setMaskingState MaskedUninterruptible (IO i) = IO $ maskUninterruptible# i
    {-# INLINE setMaskingState #-}


------------------------------------------------------------------------------
instance (MonadTrans t, MInvariant t, Monad (t m), MonadMask m) =>
    MonadMask (t m)
  where
    getMaskingState = lift getMaskingState
    {-# INLINE getMaskingState #-}
    setMaskingState s m = lift getMaskingState >>= \s' ->
        hoistiso (setMaskingState s) (setMaskingState s') m
    {-# INLINE setMaskingState #-}


------------------------------------------------------------------------------
-- | Executes a computation with asynchronous exceptions /masked/.  That is,
-- any thread which attempts to raise an exception in the current thread with
-- 'Control.Exception.throwTo' will be blocked until asynchronous exceptions
-- are unmasked again.
--
-- The argument passed to 'mask' is a function that takes as its argument
-- another function, which can be used to restore the prevailing masking state
-- within the context of the masked computation. For example, a common way to
-- use 'mask' is to protect the acquisition of a resource:
--
-- > mask $ \restore -> do
-- >     x <- acquire
-- >     restore (do_something_with x) `finally` release
--
-- This code guarantees that @acquire@ is paired with @release@, by masking
-- asynchronous exceptions for the critical parts. (Rather than write this
-- code yourself, it would be better to use 'bracket' which abstracts the
-- general pattern).
--
-- Note that the @restore@ action passed to the argument to @mask@ does not
-- necessarily unmask asynchronous exceptions, it just restores the masking
-- state to that of the enclosing context. Thus if asynchronous exceptions are
-- already masked, @mask@ cannot be used to unmask exceptions again. This is
-- so that if you call a library function with exceptions masked, you can be
-- sure that the library call will not be able to unmask exceptions again. If
-- you are writing library code and need to use asynchronous exceptions, the
-- only way is to create a new thread; see
-- 'Monad.Fork.forkWithUnmask'.
--
-- Asynchronous exceptions may still be received while in the masked state if
-- the masked thread /blocks/ in certain ways; see
-- "Control.Exception#interruptible".
--
-- Threads created by 'Monad.Fork.fork' inherit the masked
-- state from the parent; that is, to start a thread in blocked mode, use
-- @mask_ $ fork ...@.  This is particularly useful if you need to establish
-- an exception handler in the forked thread before any asynchronous
-- exceptions are received.
mask :: MonadMask m => ((forall a n. MonadMask n => n a -> n a) -> m b) -> m b
mask f = getMaskingState >>= \s -> case s of
    Unmasked -> setMaskingState MaskedInterruptible (f (setMaskingState s))
    _ -> f id
{-# INLINABLE mask #-}


------------------------------------------------------------------------------
-- | Like 'mask', but does not pass a @restore@ action to the argument.
mask_ :: MonadMask m => m a -> m a
mask_ = mask . const
{-# INLINABLE mask_ #-}


------------------------------------------------------------------------------
-- | Like 'mask', but the masked computation is not interruptible (see
-- "Control.Exception#interruptible"). THIS SHOULD BE USED WITH GREAT CARE,
-- because if a thread executing in 'uninterruptibleMask' blocks for any
-- reason, then the thread (and possibly the program, if this is the main
-- thread) will be unresponsive and unkillable. This function should only be
-- necessary if you need to mask exceptions around an interruptible operation,
-- and you can guarantee that the interruptible operation will only block for
-- a short period of time.
uninterruptibleMask :: MonadMask m
    => ((forall a n. MonadMask n => n a -> n a) -> m b)
    -> m b
uninterruptibleMask f = getMaskingState >>= \s -> case s of
    MaskedUninterruptible -> f id
    _ -> setMaskingState MaskedUninterruptible (f (setMaskingState s))
{-# INLINABLE uninterruptibleMask #-}


------------------------------------------------------------------------------
-- | Like 'uninterruptibleMask', but does not pass a @restore@ action to the
-- argument.
uninterruptibleMask_ :: MonadMask m => m a -> m a
uninterruptibleMask_ = uninterruptibleMask . const
{-# INLINABLE uninterruptibleMask_ #-}
