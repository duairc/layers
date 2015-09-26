{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
#ifdef LANGUAGE_ConstraintKinds
{-# LANGUAGE ConstraintKinds #-}
#endif

#include <macros.h>

{-|

This module defines the 'MonadCont' G(monadinterface,interface). It is
designed to be compatible with the with the
T(mtl,Control-Monad-Cont-Class,MonadCont) interface from the H(mtl)
package. It consists of:

  * The 'MonadCont' constraint.
  * The 'callCC' operation.
  * Instances of 'MonadCont':

     * For arbitrary G(innermonad,inner monads) wrapped by 'ContT'.
     * G(universalpassthroughinstance,Pass-through instances) for:

          * Any G(innermonad,inner monad) with an existing 'MonadCont'
          instance wrapped by any G(monadlayer,monad layer) implementing
          'Control.Monad.Lift.MonadTransControl'.
          * The <M(mmorph,Control-Monad-Trans-Compose)#t:ComposeT composition>
          of two G(monadlayer,monad layers) wrapped around an
          G(innermonad,inner monad), where either the
          G(innermonad,inner monad) or one or more of the composed
          G(monadlayer,monad layers) has an existing instance for
          'MonadCont'.

-}

module Monad.Cont
    ( MonadCont (callCC)
    )
where

#if MIN_VERSION_mmorph(1, 0, 1)
-- mmorph --------------------------------------------------------------------
import           Control.Monad.Trans.Compose (ComposeT (ComposeT))
#endif


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
-- | The 'MonadCont' G(monadinterface,interface) represents computations in
-- continuation-passing style (CPS). In continuation-passing style function
-- result is not returned, but instead is passed to another function, received
-- as a parameter (continuation). Computations are built up from sequences of
-- nested continuations, terminated by a final continuation (often 'id') which
-- produces the final result. Since continuations are functions which
-- represent the future of a computation, manipulation of the continuation
-- functions can achieve complex manipulations of the future of the
-- computation, such as interrupting a computation in the middle, aborting a
-- portion of a computation, restarting a computation, and interleaving
-- execution of computations. The 'MonadCont' G(monadinterface,interface)
-- adapts CPS to the structure of a monad.
--
-- Before using the 'MonadCont' G(monadinterface,interface), be sure that you
-- have a firm understanding of continuation-passing style and that
-- continuations represent the best solution to your particular design
-- problem. Many algorithms which require continuations in other languages do
-- not require them in Haskell, due to Haskell's lazy semantics. Abuse of the
-- 'MonadCont' G(monadinterface,interface) can produce code that is impossible
-- to understand and maintain.
--
-- Minimal complete definition: 'callCC'.
class Monad m => MonadCont m where
    -- | 'callCC' (call-with-current-continuation) calls a function with the
    -- current continuation as its argument. Provides an escape continuation
    -- mechanism for use with instances of 'MonadCont'. Escape continuations
    -- allow to abort the current computation and return a value immediately.
    -- They achieve a similar effect to 'Monad.Abort.abort' and
    -- 'Monad.Recover.recover' from the 'Monad.Abort.MonadAbort' and
    -- 'Monad.Recover.MonadRecover' G(monadinterface,interfaces). Advantage of
    -- this function over calling 'return' is that it makes the continuation
    -- explicit, allowing more flexibility and better control.
    --
    -- The standard idiom used with 'callCC' is to provide a lambda-expression
    -- to name the continuation. Then calling the named continuation anywhere
    -- within its scope will escape from the computation, even if it is many
    -- layers deep within nested computations.
    callCC :: ((a -> m b) -> m a) -> m a

#ifdef MINIMALSupport
    {-# MINIMAL callCC #-}
#endif


------------------------------------------------------------------------------
instance MonadCont (ContT r m) where
    callCC f = ContT $ \c -> let ContT m = f $ \a -> ContT $ \_ -> c a in m c
    {-# INLINABLE callCC #-}


#if MIN_VERSION_mmorph(1, 0, 1)
------------------------------------------------------------------------------
instance MonadCont (f (g m)) => MonadCont (ComposeT f g m) where
    callCC f =
        ComposeT (callCC (\c -> let ComposeT m = f (ComposeT . c) in m))
    {-# INLINABLE callCC #-}
#endif


------------------------------------------------------------------------------
instance _OVERLAPPABLE (MonadTopControl t m, MonadCont m, Monad (t m)) =>
    MonadCont (t m)
  where
    callCC f = liftControlT (\peel -> callCC $ \c -> peel . f $ \a ->
        liftT (peel (return a) >>= c)) >>= resumeT
    {-# INLINABLE callCC #-}
