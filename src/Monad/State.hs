{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

#ifdef LANGUAGE_ConstraintKinds
{-# LANGUAGE ConstraintKinds #-}
#endif

#ifdef LANGUAGE_SafeHaskell
#if __GLASGOW_HASKELL__ >= 704
{-# LANGUAGE Safe #-}
#else
{-# LANGUAGE Trustworthy #-}
#endif
#endif

#include <docmacros.h>
#include <overlap.h>

{-|

This module defines the 'MonadState' G(monadinterface,interface). It is
designed to be compatible with the with the
T(mtl,Control-Monad-State-Class,Monadstate) interface from the H(mtl)
package. It consists of:

  * The 'MonadState' constraint.
  * The 'get' and 'put' operations.

  * Instances of 'MonadState':

      * For arbitrary G(innermonad,inner monads) wrapped by one of the
      following G(monadlayer,monad layers):

          * Lazy 'L.StateT'
          * Strict 'StateT'
          * Lazy 'L.RWST'
          * Strict 'RWST'

      * G(universalpassthroughinstance,Pass-through instances) for:

          * Any G(innermonad,inner monad) with an existing 'MonadState'
          instance wrapped by any G(monadlayer,monad layer) implementing
          'Control.Monad.Lift.MonadTrans'.
          * The 'Product' of any two G(monadictype,monadic types) which both
          have existing 'MonadState' instances.
          * The <M(mmorph,Control-Monad-Trans-Compose)#t:ComposeT composition>
          of two G(monadlayer,monad layers) wrapped around an
          G(innermonad,inner monad), where either the
          G(innermonad,inner monad) or one or more of the composed
          G(monadlayer,monad layers) has an existing instance for
          'MonadState'.

  * The 'gets' and 'modify' utility operations.

-}

module Monad.State
    ( MonadState (state, get, put)
    , modify
    , gets
    )
where

-- base ----------------------------------------------------------------------
import           Control.Monad (liftM)
#if !MIN_VERSION_base(4, 8, 0)
import           Data.Monoid (Monoid (mempty))
#endif


-- layers --------------------------------------------------------------------
import           Control.Monad.Lift.Top (MonadTop, liftT)


#if MIN_VERSION_mmorph(1, 0, 1)
-- mmorph --------------------------------------------------------------------
import           Control.Monad.Trans.Compose (ComposeT (ComposeT))


#endif
-- transformers --------------------------------------------------------------
import qualified Control.Monad.Trans.State.Lazy as L (StateT (StateT))
import           Control.Monad.Trans.State.Strict (StateT (StateT))
import qualified Control.Monad.Trans.RWS.Lazy as L (RWST (RWST))
import           Control.Monad.Trans.RWS.Strict (RWST (RWST))
#if MIN_VERSION_transformers(0, 3, 0)
import           Data.Functor.Product (Product (Pair))
#endif


------------------------------------------------------------------------------
-- | A pure functional language cannot update values in place because it
-- violates referential transparency. A common idiom to simulate such stateful
-- G(computation,computations) is to \"thread\" a state parameter through a
-- sequence of functions.
--
-- This approach works, but such code can be error-prone, messy and difficult
-- to maintain. The 'MonadState' G(monadinterface,interface) hides the
-- threading of the state parameter inside the binding operation,
-- simultaneously making the code easier to write, easier to read and easier
-- to modify. 
--
-- Minimal complete definition: 'state' or both 'get' and 'put'.
class Monad m => MonadState s m | m -> s where
    -- | Embed a simple state G(computation,action) into the monad.
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
    {-# INLINABLE state #-}

    get = state (\s -> (s, s))

    put s = state (\_ -> ((), s))

#ifdef MinimalPragma
    {-# MINIMAL state | (get, put) #-}

#endif

------------------------------------------------------------------------------
instance Monad m => MonadState s (L.StateT s m) where
    state = L.StateT . (return .)
    get = L.StateT $ \s -> return (s, s)
    put s = L.StateT $ \_ -> return ((), s)


------------------------------------------------------------------------------
instance Monad m => MonadState s (StateT s m) where
    state = StateT . (return .)
    get = StateT $ \s -> return (s, s)
    put s = StateT $ \_ -> return ((), s)


------------------------------------------------------------------------------
instance (Monad m, Monoid w) => MonadState s (L.RWST r w s m) where
    state f = L.RWST $ \_ s -> let (a, s') = f s in return (a, s', mempty)
    get = L.RWST $ \_ s -> return (s, s, mempty)
    put s = L.RWST $ \_ _ -> return ((), s, mempty)


------------------------------------------------------------------------------
instance (Monad m, Monoid w) => MonadState s (RWST r w s m) where
    state f = RWST $ \_ s -> case f s of (a, s') -> return (a, s', mempty)
    get = RWST $ \_ s -> return (s, s, mempty)
    put s = RWST $ \_ _ -> return ((), s, mempty)


#if MIN_VERSION_transformers(0, 3, 0)
------------------------------------------------------------------------------
instance (MonadState s f, MonadState s g) => MonadState s (Product f g) where
    state f = Pair (state f) (state f)
    get = Pair get get
    put s = Pair (put s) (put s)


#endif
#if MIN_VERSION_mmorph(1, 0, 1)
------------------------------------------------------------------------------
instance MonadState s (f (g m)) => MonadState s (ComposeT f g m) where
    state f = ComposeT (state f)
    get = ComposeT get
    put s = ComposeT (put s)


#endif
------------------------------------------------------------------------------
instance __OVERLAPPABLE__ (MonadTop t m, MonadState s m, Monad (t m)) =>
    MonadState s (t m)
  where
    state = liftT . state
    {-# INLINABLE state #-}
    get = liftT get
    {-# INLINABLE get #-}
    put = liftT . put
    {-# INLINABLE put #-}


------------------------------------------------------------------------------
-- | Monadic state transformer.
--
-- Maps an old state to a new state inside a state monad. The old state is
-- thrown away.
--
-- @>>> __:t 'modify' ('++'\"?\")__
--'modify' (...) :: ('MonadState' 'String' a) => a ()@
--
-- This says that @'modify' ('++'\"?\")@ acts over any monad that is a member
-- of the 'MonadState' class with a 'String' state.
modify :: MonadState s m => (s -> s) -> m ()
modify f = state (\s -> ((), f s))
{-# INLINABLE modify #-}


------------------------------------------------------------------------------
-- | Gets specific component of the state, using a projection function
-- supplied.
gets :: MonadState s m => (s -> a) -> m a
gets f = liftM f get
{-# INLINABLE gets #-}
