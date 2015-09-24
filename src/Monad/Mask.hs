{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
#if LANGUAGE_ConstraintKinds
{-# LANGUAGE ConstraintKinds #-}
#endif
#if __GLASGOW_HASKELL__ >= 707
{-# LANGUAGE ImpredicativeTypes #-}
#endif

#include <macros.h>

{-|

This module defines the 'MonadMask' G(monadinterface,interface). It, along
with its sister G(monadinterface,interface) 'Monad.Try.MonadTry', is designed
to be largely compatible with the "Control.Exception" module from H(base).
It consists of:

  * The 'MonadMask' constraint.
  * The 'mask' and 'mask_' operations.
  * The 'uninterruptibleMask' and 'uninterruptibleMask_' operations.
  * Instances of 'MonadMask':

      * For every G(basemonad,base monad) in the H(base) and H(transformers)
      packages:

          * 'Either'
          * @-@@>@
          * 'Identity'
          * 'IO'
          * @[@@]@
          * 'Maybe'
          * 'Proxy'
          * Lazy 'L.ST'
          * Strict 'ST'
          * 'STM'

      * G(universalpassthroughinstance,Pass-through instances) for:

          * Any G(innermonad,inner monad) with an existing 'MonadMask'
          instance wrapped by any G(monadlayer,monad layer) implementing
          'Control.Monad.Lift.MonadTrans' and 'Control.Monad.Lift.MInvariant'.
          * The 'Product' of any two G(monadictype,monadic types) which both
          have existing 'MonadMask' instances.
          * The <M(mmorph,Control-Monad-Trans-Compose)#t:ComposeT composition>
          of two G(monadlayer,monad layers) wrapped around an
          G(innermonad,inner monad), where either the
          G(innermonad,inner monad) or one or more of the composed
          G(monadlayer,monad layers) has an existing instance for 'MonadMask'.

  * The 'MaskingState' datatype.
  * The 'getMaskingState' and 'setMaskingState' operations (but these are only
  for implementing 'mask' and 'uninterruptibleMask', not to be used directly).

-}

module Monad.Mask
    ( MonadMask (getMaskingState, setMaskingState)
    , MaskingState (Unmasked, MaskedInterruptible, MaskedUninterruptible)
    , mask
    , mask_
    , uninterruptibleMask
    , uninterruptibleMask_
    )
where

-- base ----------------------------------------------------------------------
#if MIN_VERSION_base(4, 3, 0)
import           Control.Exception
                     ( MaskingState
                         ( Unmasked
                         , MaskedInterruptible
                         , MaskedUninterruptible
                         )
                     )
import qualified Control.Exception as E (getMaskingState)
#else
import           Control.Exception (block, unblock, blocked)
#endif
import           Control.Monad.ST (ST)
import qualified Control.Monad.ST.Lazy as L (ST)
#if MIN_VERSION_base(4, 7, 0)
import           Data.Proxy (Proxy)
#endif
#if MIN_VERSION_base(4, 3, 0)
import           GHC.Base
                     ( maskAsyncExceptions#
                     , maskUninterruptible#
                     , unmaskAsyncExceptions#
                     )
import           GHC.Conc.Sync (STM)
import           GHC.IO (IO (IO))
#else
import           GHC.Conc (STM)
#endif


#if MIN_VERSION_mmorph(1, 0, 1)
-- mmorph --------------------------------------------------------------------
import           Control.Monad.Trans.Compose (ComposeT (ComposeT))
#endif


-- transformers --------------------------------------------------------------
import           Data.Functor.Identity (Identity)
#if MIN_VERSION_transformers(0, 3, 0)
import           Data.Functor.Product (Product (Pair))
#endif


-- layers --------------------------------------------------------------------
import           Control.Monad.Lift.Top (MonadTopInvariant, liftT, hoistisoT)


------------------------------------------------------------------------------
-- | The 'MonadMask' type class is for dealing with asynchronous exceptions.
-- It contains the 'getMaskingState' and 'setMaskingState' operations for
-- getting and setting the 'MaskingState' of the current thread. However, you
-- should never need to use these operations: in particular, using
-- 'setMaskingState' can violate some invariants which are assumed internally
-- by this library. The only reason these functions are exposed at all is that
-- they are necessary to implement 'mask' (which is what you should use
-- instead), and unlike 'mask', their simpler type signature allows us to
-- define a G(universalpassthroughinstance,universal pass-through instance)
-- of 'MonadMask' through any G(monadlayer,monad layer) implementing
-- 'Control.Monad.Lift.MonadTrans' and 'Control.Monad.Lift.MInvariant' (which
-- in practice should be every monad layer), while 'mask' could only be lifted
-- through G(monadlayer,monad layers) which implement
-- 'Control.Monad.Lift.MonadTransControl'.
--
-- /Every/ monad should be an instance of 'MonadMask', and we have provided
-- instances for every G(basemonad,base monad) in the H(base) and
-- H(transformers) packages. 'getMaskingState' and 'setMaskingState' have
-- default definitions that only need to be overridden in the case of 'IO' and
-- monads layered on top of 'IO' (which we have already done), so it costs
-- nothing to add an instance of 'MonadMask' to a monad. ('MonadMask' is a
-- prerequisite for implementing 'Monad.Try.MonadTry', which provides the
-- 'Monad.Try.bracket' family of functions, which is perhaps more interesting
-- than 'MonadMask' on its own.)
--
-- Minimal complete definition: instance head only.
class Monad m => MonadMask m where
    -- | Returns the 'MaskingState' for the current thread.
    getMaskingState :: m MaskingState

    -- | Sets the 'MaskingState' for the current thread to the given value.
    setMaskingState :: MaskingState -> m a -> m a

    getMaskingState = return MaskedInterruptible
    setMaskingState = const id


#if !MIN_VERSION_base(4, 3, 0)
------------------------------------------------------------------------------
-- | Describes the behaviour of a thread when an asynchronous exception is
-- received.
data MaskingState
    = Unmasked
        -- ^ asynchronous exceptions are unmasked (the normal state)
    | MaskedInterruptible 
        -- ^ the state during 'mask': asynchronous exceptions are masked, but
        -- blocking operations may still be interrupted
    | MaskedUninterruptible
        -- ^ the state during 'uninterruptibleMask': asynchronous exceptions
        -- are masked, and blocking operations may not be interrupted
  deriving (Eq, Show)
#endif


------------------------------------------------------------------------------
instance MonadMask Identity


#if MIN_VERSION_transformers(0, 3, 0)
------------------------------------------------------------------------------
instance (MonadMask f, MonadMask g) => MonadMask (Product f g) where
    getMaskingState = Pair getMaskingState getMaskingState
    setMaskingState s (Pair f g)
        = Pair (setMaskingState s f) (setMaskingState s g)
#endif


#if MIN_VERSION_mmorph(1, 0, 1)
------------------------------------------------------------------------------
instance MonadMask (f (g m)) => MonadMask (ComposeT f g m) where
    getMaskingState = ComposeT getMaskingState
    setMaskingState s (ComposeT m) = ComposeT (setMaskingState s m)
#endif


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


#if MIN_VERSION_base(4, 7, 0)
------------------------------------------------------------------------------
instance MonadMask Proxy


#endif
------------------------------------------------------------------------------
instance MonadMask IO where
#if MIN_VERSION_base(4, 3, 0)
    getMaskingState = E.getMaskingState
    setMaskingState Unmasked (IO i) = IO $ unmaskAsyncExceptions# i
    setMaskingState MaskedInterruptible (IO i) = IO $ maskAsyncExceptions# i
    setMaskingState MaskedUninterruptible (IO i) = IO $ maskUninterruptible# i
#else
    getMaskingState = fmap (\b -> if b then MaskedInterruptible else Unmasked)
        blocked
    setMaskingState Unmasked = unblock 
    setMaskingState MaskedInterruptible = block
    setMaskingState MaskedUninterruptible = block
#endif


------------------------------------------------------------------------------
instance (MonadTopInvariant m t m, MonadMask m, Monad (t m)) => MonadMask (t m) where
    getMaskingState = liftT getMaskingState
    {-# INLINABLE getMaskingState #-}
    setMaskingState s m = liftT getMaskingState >>= \s' ->
        hoistisoT (setMaskingState s) (setMaskingState s') m
    {-# INLINABLE setMaskingState #-}


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
-- @'mask' $ \\restore -> do
--    x <- acquire
--    restore (do_something_with x) \``Monad.Try.finally`\` release@
--
-- This code guarantees that @acquire@ is paired with @release@, by masking
-- asynchronous exceptions for the critical parts. (Rather than write this
-- code yourself, it would be better to use 'Monad.Try.bracket' which
-- abstracts the general pattern).
--
-- Note that the @restore@ action passed to the argument to 'mask' does not
-- necessarily unmask asynchronous exceptions, it just restores the masking
-- state to that of the enclosing context. Thus if asynchronous exceptions are
-- already masked, 'mask' cannot be used to unmask exceptions again. This is
-- so that if you call a library function with exceptions masked, you can be
-- sure that the library call will not be able to unmask exceptions again. If
-- you are writing library code and need to use asynchronous exceptions, the
-- only way is to create a new thread; see
-- 'Monad.Fork.forkWithUnmask'.
--
-- Asynchronous exceptions may still be received while in the masked state if
-- the masked thread /blocks/ in certain ways; see
-- <M(base,Control-Exception)#interruptible here>.
--
-- Threads created by 'Monad.Fork.fork' inherit the masked
-- state from the parent; that is, to start a thread in blocked mode, use
-- @'mask_' $ 'Monad.Fork.fork' ...@.  This is particularly useful if you need
-- to establish an exception handler in the forked thread before any
-- asynchronous exceptions are received.
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
-- <M(base,Control-Exception)#interruptible here>).
-- __This should be used with great care__, because if a thread executing in
-- 'uninterruptibleMask' blocks for any reason, then the thread (and possibly
-- the program, if this is the main thread) will be unresponsive and
-- unkillable. This function should only be necessary if you need to mask
-- exceptions around an interruptible operation, and you can guarantee that
-- the interruptible operation will only block for a short period of time.
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
