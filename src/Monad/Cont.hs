{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

{-|

This module exports:

    1. The 'MonadCont' type class and its operation 'callCC'.

    2. An instance of 'MonadCont' for the 'ContT' monad transformer from the
    @transformers@ package.

    3. A universal pass-through instance of 'MonadCont' for any existing
    @MonadCont@ wrapped by a 'MonadLayerControl'.

-}

module Monad.Cont
    ( MonadCont (callCC)
    )
where

-- transformers --------------------------------------------------------------
import           Control.Monad.Trans.Cont (ContT (ContT))


-- layers --------------------------------------------------------------------
import           Control.Monad.Lift
                    ( MonadTransControl
                    , lift
                    , liftControl
                    , restore
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
    -- They achieve a similar effect to
    -- 'Monad.Exception.throw' and
    -- 'Monad.Exception.catch' from the
    -- 'Monad.Exception.MonadException' interface.
    -- Advantage of this function over calling @return@ is that it makes the
    -- continuation explicit, allowing more flexibility and better control.
    --
    -- The standard idiom used with @callCC@ is to provide a lambda-expression
    -- to name the continuation. Then calling the named continuation anywhere
    -- within its scope will escape from the computation, even if it is many
    -- layers deep within nested computations. 
    callCC :: ((a -> m b) -> m a) -> m a


------------------------------------------------------------------------------
instance Monad m => MonadCont (ContT r m) where
    callCC f = ContT $ \c -> let ContT m = f $ \a -> ContT $ \_ -> c a in m c
    {-# INLINE callCC #-}


------------------------------------------------------------------------------
instance (MonadTransControl t, MonadCont m, Monad (t m)) => MonadCont (t m)
  where
    callCC f = liftControl (\run -> callCC $ \c -> run . f $ \a ->
        lift (run (return a) >>= c)) >>= uncurry restore
    {-# INLINE callCC #-}
