{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

{-|

This module defines the 'MonadFork' interface, which consists of:

    * 'MonadFork' :: @(* -> *) -> Constraint@

    * 'fork' :: @MonadFork m => m () -> m ThreadId@

    * 'forkOn' :: @MonadFork m => Int -> m () -> m ThreadId@

    * 'forkWithUnmask' :: @MonadFork m =>
        ((forall a n. MonadMask n => n a -> n a) -> m ()) -> m ThreadId@

    * 'forkOnWithUnmask' :: @MonadFork m => Int ->
        ((forall a n. MonadMask n => n a -> n a) -> m ()) -> m ThreadId@

    * 'forkFinally' :: @(MonadTry m, MonadFork m) => m a ->
        (Either (m a) a -> m ()) -> m ThreadId@

The 'MonadFork' interface is designed for compatibility with
"Control.Concurrent".

-}

module Monad.Fork
    ( MonadFork (fork, forkOn)
    , forkWithUnmask
    , forkOnWithUnmask
    , forkFinally
    )
where

-- base ----------------------------------------------------------------------
import           Control.Concurrent (ThreadId, forkIO)
#if MIN_VERSION_base(4, 4, 0)
import qualified Control.Concurrent (forkOn)
#endif


-- transformers --------------------------------------------------------------
import           Data.Functor.Product (Product (Pair))

        
-- layers --------------------------------------------------------------------
import           Control.Monad.Lift (MonadTransControl, liftDiscard)
import           Monad.Mask
                     ( MonadMask
                     , MaskingState (Unmasked)
                     , mask
                     , setMaskingState
                     )
import           Monad.Try (MonadTry, mtry)


------------------------------------------------------------------------------
-- | The 'MonadFork' type class, for monads which support a fork operation.
--
-- An example of a monad which has a MonadFork instance that is not simply a
-- lifted form of @forkIO@ is the @ResourceT@ monad from the @resourcet@
-- package, which defines an operation @resourceForkIO@.
class MonadMask m => MonadFork m where
    -- | Sparks off a new thread to run the computation passed as the first
    -- argument, and returns the 'Control.Concurrent.ThreadId' of the newly
    -- created thread.
    --
    -- The new thread will be a lightweight thread; if you want to use a
    -- foreign library that uses thread-local storage, use
    -- 'Control.Concurrent.forkOS' instead. (Note that @forkOS@ is not
    -- included in the 'MonadFork' interface, so your monad @m@ must be an
    -- instance of @'Control.Monad.Layer.LiftControl' 'IO m@ to be able to use
    -- @forkOS@.
    --
    -- GHC note: the new thread inherits the masked state of the parent (see
    -- 'Monad.Mask.mask').
    --
    -- The newly created thread has an exception handler that discards the
    -- exceptions 'Control.Exception.BlockedIndefinitelyOnMVar',
    -- 'Control.Exception.BlockedIndefinitelyOnSTM', and
    -- 'Control.Exception.ThreadKilled', and passes all other exceptions to
    -- the uncaught exception handler.
    fork :: m () -> m ThreadId

    -- | Like 'fork', but lets you specify on which processor the thread
    -- should run. Unlike a 'fork' thread, a thread created by 'forkOn' will
    -- stay on the same processor for its entire lifetime ('fork' threads can
    -- migrate between processors according to the scheduling policy).
    -- 'forkOn' is useful for overriding the scheduling policy when you know
    -- in advance how best to distribute the threads.
    --
    -- The 'Int' argument specifies a capability number (see
    -- 'Control.Concurrent.getNumCapabilities'). Typically capabilities
    -- correspond to physical processors, but the exact behaviour is
    -- implementation-dependent. The value passed to 'forkOn' is interpreted
    -- modulo the total number of capabilities as returned by
    -- 'Control.Concurrent.getNumCapabilities'.
    --
    -- GHC note: the number of capabilities is specified by the @+RTS -N@
    -- option when the program is started. Capabilities can be fixed to actual
    -- processor cores with @+RTS -qa@ if the underlying operating system
    -- supports that, although in practice this is usually unnecessary (and
    -- may actually degrade perforamnce in some cases - experimentation is
    -- recommended).
    forkOn :: Int -> m () -> m ThreadId


------------------------------------------------------------------------------
instance MonadFork IO where
    fork = forkIO
#if MIN_VERSION_base(4, 4, 0)
    forkOn = Control.Concurrent.forkOn
#else
    forkOn _ = forkIO
#endif


------------------------------------------------------------------------------
instance (MonadFork f, MonadFork g) => MonadFork (Product f g) where
    fork (Pair f g) = Pair (fork f) (fork g)
    forkOn n (Pair f g) = Pair (forkOn n f) (forkOn n g)


------------------------------------------------------------------------------
instance (MonadTransControl t, MonadFork m, MonadMask (t m)) =>
    MonadFork (t m)
  where
    fork = liftDiscard fork
    {-# INLINE fork #-}
    forkOn = liftDiscard . forkOn
    {-# INLINE forkOn #-}


------------------------------------------------------------------------------
-- | Like 'fork', but the child thread is passed a function that can be used
-- to unmask asynchronous exceptions.  This function is typically used in the
-- following way
--
-- >  ... mask_ $ forkIOWithUnmask $ \unmask ->
-- >                 catch (unmask ...) handler
--
-- so that the exception handler in the child thread is established with
-- asynchronous exceptions masked, meanwhile the main body of the child thread
-- is executed in the unmasked state.
--
-- Note that the @unmask@ function passed to the child thread should only be
-- used in that thread; the behaviour is undefined if it is invoked in a
-- different thread.
forkWithUnmask :: MonadFork m
    => ((forall a n. MonadMask n => n a -> n a) -> m ())
    -> m ThreadId
forkWithUnmask m = fork $ m (setMaskingState Unmasked)
{-# INLINABLE forkWithUnmask #-}


------------------------------------------------------------------------------
-- | Like 'forkWithUnmask', but the child thread is pinned to the given CPU,
-- as with 'forkOn'.
forkOnWithUnmask :: MonadFork m
    => Int
    -> ((forall a n. MonadMask n => n a -> n a) -> m ())
    -> m ThreadId
forkOnWithUnmask c m = forkOn c $ m (setMaskingState Unmasked)
{-# INLINABLE forkOnWithUnmask #-}


------------------------------------------------------------------------------
-- | @fork@ a thread and call the supplied function when the thread is about
-- to terminate, with an exception or a returned value. The function is called
-- with asynchronous exceptions masked.
--
-- > forkFinally action and_then =
-- >     mask $ \restore ->
-- >         fork $ mtry (restore action) >>= and_then
--
-- This function is useful for informing the parent when a child terminates,
-- for example.
forkFinally :: (MonadTry m, MonadFork m)
    => m a
    -> (Either (m a) a -> m ())
    -> m ThreadId
forkFinally m sequel = mask $ \restore -> fork $ mtry (restore m) >>= sequel
{-# INLINABLE forkFinally #-}
