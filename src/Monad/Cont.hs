{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
#ifdef LANGUAGE_ConstraintKinds
{-# LANGUAGE ConstraintKinds #-}
#endif

{-|

This module defines the 'MonadCont' interface, which consists of:

    * 'MonadCont' :: @(* -> *) -> Constraint@

    * 'callCC' :: @MonadCont m => ((a -> m b) -> m a) -> m a@

The 'MonadCont' interface is designed for compatibility with the @MonadCont@
interface from the @mtl@ library.

-}

module Monad.Cont
    ( MonadCont (callCC)
    )
where

-- transformers --------------------------------------------------------------
import           Control.Monad.Trans.Cont (ContT (ContT))


-- layers --------------------------------------------------------------------
import           Control.Monad.Lift.Top
                    ( MonadTopControl
                    , liftT
                    , liftControlT
                    , resumeT
                    )


------------------------------------------------------------------------------
-- | The 'MonadCont' interface represents computations in continuation-passing
-- style (CPS). In continuation-passing style function result is not returned,
-- but instead is passed to another function, received as a parameter
-- (continuation). Computations are built up from sequences of nested
-- continuations, terminated by a final continuation (often id) which produces
-- the final result. Since continuations are functions which represent the
-- future of a computation, manipulation of the continuation functions can
-- achieve complex manipulations of the future of the computation, such as
-- interrupting a computation in the middle, aborting a portion of a
-- computation, restarting a computation, and interleaving execution of
-- computations. The @MonadCont@ interface adapts CPS to the structure of a
-- monad.
--
-- Before using the @MonadCont@ interface, be sure that you have a firm
-- understanding of continuation-passing style and that continuations
-- represent the best solution to your particular design problem. Many
-- algorithms which require continuations in other languages do not require
-- them in Haskell, due to Haskell's lazy semantics. Abuse of the @MonadCont@
-- interface can produce code that is impossible to understand and maintain.
class Monad m => MonadCont m where
    -- | 'callCC' (call-with-current-continuation) calls a function with the
    -- current continuation as its argument. Provides an escape continuation
    -- mechanism for use with instances of @MonadCont@. Escape continuations
    -- allow to abort the current computation and return a value immediately.
    -- They achieve a similar effect to 'Monad.Abort.abort' and
    -- 'Monad.Recover.recover' from the 'Monad.AbortMonadAbort' and
    -- 'Monad.Recover.MonadRecover' interfaces. Advantage of this function
    -- over calling @return@ is that it makes the continuation explicit,
    -- allowing more flexibility and better control.
    --
    -- The standard idiom used with @callCC@ is to provide a lambda-expression
    -- to name the continuation. Then calling the named continuation anywhere
    -- within its scope will escape from the computation, even if it is many
    -- layers deep within nested computations. 
    callCC :: ((a -> m b) -> m a) -> m a


------------------------------------------------------------------------------
instance Monad m => MonadCont (ContT r m) where
    callCC f = ContT $ \c -> let ContT m = f $ \a -> ContT $ \_ -> c a in m c
    {-# INLINABLE callCC #-}


------------------------------------------------------------------------------
instance (MonadTopControl t m, MonadCont m) => MonadCont (t m) where
    callCC f = liftControlT (\peel -> callCC $ \c -> peel . f $ \a ->
        liftT (peel (return a) >>= c)) >>= resumeT
    {-# INLINABLE callCC #-}
