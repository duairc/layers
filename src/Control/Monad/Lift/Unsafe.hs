{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE Unsafe #-}

{-|

The implementation of the 'MonadLiftControl' interface changes to use type
families (like 'Control.Monad.Lift.MonadTransControl') when compiled with GHC
7.8 or above. One of the consequences of this is that it's no longer possible
to derive instances of 'MonadLiftControl' using the
@GeneralizedNewtypeDeriving@ extension. (In fact, this wouldn't have been
possible anyway under 7.8, because of the changes to the behaviour of
@GeneralizedNewtypeDeriving@ that come with the new
<http://ghc.haskell.org/trac/ghc/wiki/Roles roles> implementation (which fixes
GHC bug <http://ghc.haskell.org/trac/ghc/ticket/7148 #7148>).) And because the
'LiftResult' and 'LiftState' type families used in this implementation are
<http://ghc.haskell.org/trac/ghc/wiki/NewAxioms/ClosedTypeFamilies closed>,
there is no \"correct\" way to make new instances of 'MonadLiftControl'.

Rather than lose this useful feature altogether, this module provides the
'defaultPeel'', 'defaultRestore'', 'defaultSuspend'' and 'defaultExtract''
operations. These operations can be used to implement an instance
@'MonadLiftControl' i n@ for some inner monad @i@, if @n@ is isomorphic to an
@m@ for which there exists an instance @'MonadLiftControl' i m@ (e.g., if @n@
is a newtype wrapper around @m@). These operations use @unsafeCoerce@
internally in their implementation, but in such a way that should be okay as
long as the given isomorphism is valid. For this reason though, this module is
marked as <http://ghc.haskell.org/trac/ghc/wiki/SafeHaskell Unsafe>.

Here is an example that (safely) uses this module:

@
{\-\# LANGUAGE FlexibleContexts #-\}
{\-\# LANGUAGE GeneralizedNewtypeDeriving #-\}
{\-\# LANGUAGE MultiParamTypeClasses #-\}

import "Control.Applicative"
import "Control.Monad.Lift"
import "Control.Monad.Lift.Unsafe"
import "Control.Monad.Trans.State"

newtype MyMonad a = MyMonad { runMyMonad :: StateT [Int] IO a }
  deriving
    ( Functor
    , Applicative
    , Monad
    , 'Control.Monad.Lift.MonadLift' IO
    , 'Control.Monad.Lift.MonadLiftInvariant' IO
    , 'Control.Monad.Lift.MonadLiftFunctor' IO
    )

instance 'MonadLiftControl' IO MyMonad where
    'peel''    = 'defaultPeel'' runMyMonad
    'restore'' = 'defaultRestore'' MyMonad
    'suspend'' = 'defaultSuspend'' MyMonad
    'extract'' = 'defaultExtract'' MyMonad
@

-}

module Control.Monad.Lift.Unsafe
    ( defaultPeel'
    , defaultRestore'
    , defaultSuspend'
    , defaultExtract'
    )
where

-- base ----------------------------------------------------------------------
import           Control.Arrow ((***))
import           Control.Monad (liftM)
import           Unsafe.Coerce (unsafeCoerce)


-- layers --------------------------------------------------------------------
import           Control.Monad.Lift
                     ( MonadLiftControl
                     , Lift
                     , LiftResult
                     , LiftState
                     , peel'
                     , restore'
                     , suspend'
                     , extract'
                     )


------------------------------------------------------------------------------
-- | Used when implementing a custom instance of @'MonadLiftControl' i@ for
-- some monad @n@.
--
-- @n@ must be isomorphic to a monad @m@ which is already an instance of
-- @'MonadLiftControl' i@.
--
-- 'defaultPeel'' takes the @n -> m@ half of the isomorphism.
defaultPeel' :: MonadLiftControl i m
    => (forall b. n b -> m b)
    -> n a
    -> LiftState i n
    -> i (Lift i n a)
defaultPeel' un m s = liftM (unsafeCoerce *** unsafeCoerce) $
    peel' (un m) (unsafeCoerce s)


------------------------------------------------------------------------------
-- | Used when implementing a custom instance of @'MonadLiftControl' i@ for
-- some monad @n@.
--
-- @n@ must be isomorphic to a monad @m@ which is already an instance of
-- @'MonadLiftControl' i@.
--
-- 'defaultRestore'' takes the @m -> n@ half of the isomorphism.
defaultRestore' :: MonadLiftControl i m
    => (forall b. m b -> n b)
    -> proxy i
    -> Lift i n a
    -> n a
defaultRestore' nu p (r, s) = nu (restore' p (unsafeCoerce r, unsafeCoerce s))


------------------------------------------------------------------------------
-- | Used when implementing a custom instance of @'MonadLiftControl' i@ for
-- some monad @n@.
--
-- @n@ must be isomorphic to a monad @m@ which is already an instance of
-- @'MonadLiftControl' i@.
--
-- 'defaultSuspend'' takes the @m -> n@ half of the isomorphism.
defaultSuspend' :: MonadLiftControl i m
    => (forall b. m b -> n b)
    -> proxy i
    -> n (LiftState i n)
defaultSuspend' nu p = nu (liftM unsafeCoerce (suspend' p))


------------------------------------------------------------------------------
-- | Used when implementing a custom instance of @'MonadLiftControl' i@ for
-- some monad @n@.
--
-- @n@ must be isomorphic to a monad @m@ which is already an instance of
-- @'MonadLiftControl' i@.
--
-- 'defaultExtract'' takes the @m -> n@ half of the isomorphism.
defaultExtract' :: forall proxy i m n a. MonadLiftControl i m
    => (forall b. m b -> n b)
    -> proxy i
    -> proxy n
    -> LiftResult i n a
    -> Maybe a
defaultExtract' _ p p' r =
    extract' p (unsafeCoerce p' :: proxy m) (unsafeCoerce r)
