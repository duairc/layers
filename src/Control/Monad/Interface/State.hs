{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

{-|

This module exports:

    1. The 'MonadState' type class and its operations 'state', 'get' and
    'put'.

    2. Instances of 'MonadState' for the relevant monad transformers from the
    @transformers@ package (lazy 'L.StateT', strict 'StateT', lazy 'L.RWST'
    and strict 'RWST').

    3. A universal pass-through instance of 'MonadState' for any existing
    @MonadState@ wrapped by a 'MonadLayer'.

    4. The utility operations 'modify' and 'gets'.

-}

module Control.Monad.Interface.State
    ( MonadState (state, get, put)
    , modify
    , gets
    )
where

-- base ----------------------------------------------------------------------
import           Control.Monad (liftM)
import           Data.Monoid (Monoid (mempty))


-- transformers --------------------------------------------------------------
import qualified Control.Monad.Trans.State.Lazy as L (StateT (StateT))
import           Control.Monad.Trans.State.Strict (StateT (StateT))
import qualified Control.Monad.Trans.RWS.Lazy as L (RWST (RWST))
import           Control.Monad.Trans.RWS.Strict (RWST (RWST))
import           Data.Functor.Product (Product (Pair))


-- layers --------------------------------------------------------------------
import           Control.Monad.Layer (MonadLayer (type Inner, layer))


------------------------------------------------------------------------------
-- | A pure functional language cannot update values in place because it
-- violates referential transparency. A common idiom to simulate such stateful
-- computations is to \"thread\" a state parameter through a sequence of
-- functions:
--
-- This approach works, but such code can be error-prone, messy and difficult
-- to maintain. The 'MonadState' interface hides the threading of the state
-- parameter inside the binding operation, simultaneously making the code
-- easier to write, easier to read and easier to modify. 
--
-- Minimal complete definition: 'state' or both 'get' and 'put'.
class Monad m => MonadState s m | m -> s where
    -- | Embed a simple state action into the monad.
    state :: (s -> (a, s)) -> m a

    -- | Return the state from the internals of the monad.
    get :: m s

    -- | Replace the state inside the monad.
    put :: s -> m ()

    state f = do
        s <- get
        let ~(a, s') = f s
        put s'
        return a
    {-# INLINE state #-}

    get = state (\s -> (s, s))
    {-# INLINE get #-}

    put s = state (\_ -> ((), s))
    {-# INLINE put #-}


------------------------------------------------------------------------------
instance Monad m => MonadState s (L.StateT s m) where
    state = L.StateT . (return .)
    {-# INLINE state #-}
    get = L.StateT $ \s -> return (s, s)
    {-# INLINE get #-}
    put s = L.StateT $ \_ -> return ((), s)
    {-# INLINE put #-}


------------------------------------------------------------------------------
instance Monad m => MonadState s (StateT s m) where
    state = StateT . (return .)
    {-# INLINE state #-}
    get = StateT $ \s -> return (s, s)
    {-# INLINE get #-}
    put s = StateT $ \_ -> return ((), s)
    {-# INLINE put #-}


------------------------------------------------------------------------------
instance (Monad m, Monoid w) => MonadState s (L.RWST r w s m) where
    state f = L.RWST $ \_ s -> let (a, s') = f s in return (a, s', mempty)
    {-# INLINE state #-}
    get = L.RWST $ \_ s -> return (s, s, mempty)
    {-# INLINE get #-}
    put s = L.RWST $ \_ _ -> return ((), s, mempty)
    {-# INLINE put #-}


------------------------------------------------------------------------------
instance (Monad m, Monoid w) => MonadState s (RWST r w s m) where
    state f = RWST $ \_ s -> case f s of (a, s') -> return (a, s', mempty)
    {-# INLINE state #-}
    get = RWST $ \_ s -> return (s, s, mempty)
    {-# INLINE get #-}
    put s = RWST $ \_ _ -> return ((), s, mempty)
    {-# INLINE put #-}


------------------------------------------------------------------------------
instance (MonadState s f, MonadState s g) => MonadState s (Product f g) where
    state f = Pair (state f) (state f)
    {-# INLINE state #-}
    get = Pair get get
    {-# INLINE get #-}
    put s = Pair (put s) (put s)
    {-# INLINE put #-}


------------------------------------------------------------------------------
instance (MonadLayer m, MonadState s (Inner m)) => MonadState s m where
    state = layer . state
    {-# INLINE state #-}
    get = layer get
    {-# INLINE get #-}
    put = layer . put
    {-# INLINE put #-}


------------------------------------------------------------------------------
-- | Monadic state transformer.
--
-- Maps an old state to a new state inside a state monad. The old state is
-- thrown away.
--
-- >>> :t modify ((+1) :: Int -> Int)
-- modify (...) :: (MonadState Int a) => a ()
--
-- This says that @modify (+1)@ acts over any 'Monad' that is a member of the
-- 'MonadState' class with an 'Int' state.
modify :: MonadState s m => (s -> s) -> m ()
modify f = state (\s -> ((), f s))
{-# INLINE modify #-}


------------------------------------------------------------------------------
-- | Gets specific component of the state, using a projection function
-- supplied.
gets :: MonadState s m => (s -> a) -> m a
gets f = liftM f get
{-# INLINE gets #-}