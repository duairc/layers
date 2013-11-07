{-# LANGUAGE CPP #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
#if __GLASGOW_HASKELL__ >= 707
{-# LANGUAGE ImpredicativeTypes #-}
#endif

#include "macros.h"

{-|

This module houses the core machinery of the H(layers) package. It exports
everything you need to implement monad transformers compatible with
H(layers)' modular
G(monadinterface, monad interfaces), and everything that you need to lift
G(computation, computations), G(controloperation, operations) and
G(morphism, morphisms) through arbitrarily complicated
G(monadtransformerstack, stacks of monad transformers).

The H(layers) machinery is built upon two twin families of interfaces. The
'MonadTrans' family of interfaces provides operations for lifting
G(computation, computations), G(controloperation, operations) and
G(morphism, morphisms) up exactly one level in the transformer
stack. That is, it lifts computations from the monad @m@ to the monad @t m@.
The 'MonadInner' family of interfaces provides operations for lifting
computations, operations and morphisms from any level of the transformer stack
to the top of the transformer stack. That is, it lifts computations from from
the monad @i@ to the monad @m@ (where @i@ is some inner monad of @m@, e.g.,
if @m@ is a monad built from a stack of transformers and @i@ a monad around
which one or more of those transformers is wrapped). Each of the 'MonadInner'
interfaces is defined recursively in terms of its 'MonadTrans' counterpart.

The 'MonadTrans' family of interfaces is mainly used by libraries that
implement monad transformers and monad interfaces, while the 'MonadInner'
family is used by applications make use of (stacks of) these transformers.

-}

module Control.Monad.Lift
    (
    -- * The @MonadTrans@ family
    -- $transfamily

    -- ** Lifting computations
      MonadTrans (lift)

    -- ** Lifting control operations
    , MonadTransControl (suspend, resume, capture, extract)
    , LayerEffects
    , LayerResult
    , LayerState
    , liftControl
    , control
    , liftOp
    , liftOp_
    , liftDiscard

    -- ** Lifting morphisms
    , MInvariant (hoistiso)
    , MFunctor (hoist)

    -- * The @MonadInner@ family
    -- $innerfamily

    -- ** Lifting computations
    , MonadInner (liftI)

    -- ** Lifting control operations
    , MonadInnerControl (suspendI, resumeI, captureI, extractI)
    , OuterEffects
    , OuterResult
    , OuterState
    , liftControlI
    , controlI
    , liftOpI
    , liftOpI_
    , liftDiscardI

    -- *** Defaults
    -- $defaults
    , defaultSuspendI
    , defaultResumeI
    , defaultExtractI
    , defaultCaptureI

    -- ** Lifting morphisms
    , MonadInnerInvariant (hoistisoI)
    , MonadInnerFunctor (hoistI)
    )
where

-- base ----------------------------------------------------------------------
#if __GLASGOW_HASKELL__ >= 704
import           Control.Arrow (first)
#endif
import           Control.Arrow ((***))
import           Control.Monad (join, liftM)
import           Data.Monoid (Monoid, mempty)
import           GHC.Exts (Any)
import           Unsafe.Coerce (unsafeCoerce)


-- transformers --------------------------------------------------------------
import           Control.Monad.Trans.Class (MonadTrans (lift))
import           Control.Monad.Trans.Cont (ContT (ContT))
import           Control.Monad.Trans.Error (Error, ErrorT (ErrorT))
import           Control.Monad.Trans.Identity (IdentityT (IdentityT))
import           Control.Monad.Trans.List (ListT (ListT))
import           Control.Monad.Trans.Maybe (MaybeT (MaybeT))
import           Control.Monad.Trans.Reader (ReaderT (ReaderT))
import qualified Control.Monad.Trans.RWS.Lazy as L (RWST (RWST))
import           Control.Monad.Trans.RWS.Strict (RWST (RWST))
import qualified Control.Monad.Trans.State.Lazy as L (StateT (StateT))
import           Control.Monad.Trans.State.Strict (StateT (StateT))
import qualified Control.Monad.Trans.Writer.Lazy as L (WriterT (WriterT))
import           Control.Monad.Trans.Writer.Strict (WriterT (WriterT))
import           Data.Functor.Identity (Identity (Identity))


-- mmorph --------------------------------------------------------------------
import           Control.Monad.Morph (MFunctor (hoist))


{-$transfamily

The 'MonadTrans' family of interfaces consists of:

    * 'MonadTrans', re-exported from the
        @<http://hackage.haskell.org/package/transformers transformers>@
        package.

    * 'MonadTransControl', defined by
        @<http://hackage.haskell.org/package/layers layers>@.

    * 'MInvariant', defined by
        @<http://hackage.haskell.org/package/layers layers>@.

    * 'MFunctor', re-exported from the
        @<http://hackage.haskell.org/package/mmorph mmorph>@ package.

Ideally, all of these classes would be re-exports from more popular packages,
because then it would be possible to write monad transformers compatible with
@<http://hackage.haskell.org/package/layers layers>@' monad interfaces without
ever incurring a dependency on
@<http://hackage.haskell.org/package/layers layers>@. As 'MonadTrans' and
'MFunctor' come from
@<http://hackage.haskell.org/package/transformers transformers>@ and
@<http://hackage.haskell.org/package/mmorph mmorph>@ respectively, we're
already half way there. @<http://hackage.haskell.org/package/mmorph mmorph>@
is also the most sensible home for 'MInvariant', and
<https://github.com/Gabriel439/Haskell-MMorph-Library/pull/1 hopefully> it
will get moved there soon. 'MonadTransControl' is more complicated: there is a
<http://hackage.haskell.org/package/monad-control/docs/Control-Monad-Trans-Control.html#t:MonadTransControl very similar class>
(the design of which I originally copied) defined in the
@<http://hackage.haskell.org/package/monad-control monad-control>@
package, which is a relatively popular package. However,
@<http://hackage.haskell.org/package/layers layers>@' version has a few
important differences that stop it from being able to use
@<http://hackage.haskell.org/package/monad-control monad-control>@'
@<http://hackage.haskell.org/package/monad-control/docs/Control-Monad-Trans-Control.html#t:MonadTransControl MonadTransControl>@
without losing some of its features (notably the 'Monad.Try.MonadTry'
interface). However, It is conceivable that these changes could be merged into
@<http://hackage.haskell.org/package/monad-control monad-control>@ some day,
in which case I would be happy to make
@<http://hackage.haskell.org/package/layers layers>@ depend on
@<http://hackage.haskell.org/package/monad-control monad-control>@ and make
'MonadTransControl' a re-export.

-}

{-$innerfamily

The 'MonadInner' family of interfaces consists of:

    * 'MonadInner'

    * 'MonadInnerControl'

    * 'MonadInnerInvariant'

    * 'MonadInnerFunctor'

Each operation in the 'MonadInner' family of interfaces is an analogue of an
operation from the 'MonadTrans' family of interfaces. The naming of these
operations reflect this relationship.

-}

------------------------------------------------------------------------------
-- | The constraint @'MonadTransControl' t@ holds if @t@ is a monad
-- transformer through which control operations can be lifted. There are a
-- variety of operations for doing so, depending on the exact type of the
-- control operation in question, including 'liftControl', 'control',
-- 'liftOp', 'liftOp_' and 'liftDiscard'. These are all built on top of the
-- more primitive 'capture', 'suspend' and 'resume' operations.
--
-- The thing that all \"control\" operations have in common is that they have
-- a monadic argument somewhere in a contravariant position (otherwise they
-- could be lifted with just 'lift'). For example, let's say you have a
-- control operation @f :: m a -> m b@, and you want to make a lifted version
-- @f' :: t m a -> t m b@. @f'@ needs to somehow \"lower\" its argument from
-- @t m a@ to @m a@ so that it can be passed to the original @f@.
--
-- Naively, we might try to type class an operation like this and call it
-- @peel@ (because it \"peels\" off the @t@ layer of a @t m a@ computation):
--
-- @
-- peel :: 'Monad' m => t m a -> m a
-- @
--
-- Unfortunately, the only monad transformers that could provide such an
-- operation are trivial (i.e., isomorphic to 'IdentityT'). To see why other
-- monad transformers cannot provide such an operation, let's consider the
-- more complicated 'RWST' monad transformer. It is defined like this:
--
-- @
-- newtype 'RWST' r w s m a = 'RWST' { 'Control.Monad.Trans.RWS.Strict.runRWST' :: r -> s -> m (a, s, w) }
-- @
--
-- It's not possible to make an operation @'RWST' r w s m a -> m a@, because
-- 'RWST'​'s inner function needs an @r@ and an @s@ value before it can return
-- a value in @m@. However, we could write an operation
-- @'RWST' r w s m a -> r -> s -> m a@. Given that we have also have the
-- operation @'IdentityT' m a -> m a@ for 'IdentityT', we want some pattern
-- that can abstract this into a single operation. To do this we introduce an
-- associated type synonym to the 'MonadTransControl' class called
-- 'LayerState'.
-- 
-- @
-- type 'LayerState' 'IdentityT' m = ()
-- type 'LayerState' ('RWST' r w s) m = (r, s)
-- @
--
-- Now we can have an operation with a single type that fits both 'RWST' and
-- 'IdentityT':
--
-- @
-- peel :: 'Monad' m => t m a -> 'LayerState' t m -> m a
-- @
--
-- This is better, but it's still far from perfect. First of all, we don't
-- really want to peel away the @t@ layer completely, because then we lose all
-- of its side-effects. What we really want is to be able to 'suspend' them
-- temporarily (allowing us to work in the monad @m@ underneath @t@) and then
-- later 'resume' them when we're finished working in @m@. Secondly, even if
-- peeling really was what we wanted to do, there are several monad
-- transformers for which we couldn't do this even if we wanted to. Consider
-- 'MaybeT', defined as follows:
--
-- @
-- newtype 'MaybeT' m a = { 'Control.Monad.Trans.Maybe.runMaybeT' :: m ('Maybe' a) }
-- @
-- 
-- If the value inside the @m@ computation wrapped by 'MaybeT' is a 'Nothing',
-- we can't get an @a@ out of it. The closest to the proposed type for 'peel'
-- above that we could get for 'MaybeT' would be:
--
-- @
-- peel :: 'MaybeT' m a -> 'LayerState' 'MaybeT' m -> m ('Maybe' a)
-- @
--
-- Similarly, for @'ErrorT' e@, the closest we could get would be:
--
-- @
-- peel :: 'ErrorT' e m a -> 'LayerState' ('ErrorT' e) m -> m ('Either' e a)
-- @
--
-- Again, we can use an associated type synonym to make all of these
-- operations fit a single pattern. We call this one 'LayerResult'. Here are
-- some of its instances:
--
-- @
-- type 'LayerResult' 'IdentityT' = 'Identity'
-- type 'LayerResult' 'MaybeT' = 'Maybe'
-- type 'LayerResult' ('ErrorT' e) = 'Either' e
-- type 'LayerResult' ('RWST' r w s) = (,) w
-- @
--
-- How exactly is it decided what @'LayerResult' t@ should be for a particular
-- monad transformer? There are several things to consider. Let's say we have
-- a monad transformer @t@ that we want to make an instance of
-- 'MonadTransControl'. Let's also assume that its 'LayerState' is @()@ (like
-- 'ErrorT', 'IdentityT' and 'MaybeT'), because this is easier to explain
-- first. The goal should be for @'LayerResult' t a@ to expand to a type
-- which is isomorphic to the type inside the @m@ computation wrapped by our
-- monad transformer. To take 'MaybeT' as an example again, it wraps an @m@
-- computation the type inside of which is @'Maybe' a@. Therefore we want
-- @'LayerResult' 'MaybeT' a@ to expands to @'Maybe' a@, which it does,
-- because @'LayerResult' 'MaybeT' = 'Maybe'@ as shown above.
--
-- The next thing to consider is that 'LayerResult' is a type family which
-- takes a monad transformer (of kind @(* -> *) -> * -> *@) as its argument,
-- and returns a type constructor @* -> *@. Note that it is not a type family
-- which takes a monad transformer @(* -> *) -> * -> *@ and a value @*@ and
-- returns a value @*@! So, we might be tempted to say:
--
-- @
-- type 'LayerResult' 'IdentityT' a = a
-- @
--
-- But we can't, because @'LayerResult' t@ must be a type constructor
-- @* -> *@, not a type @*@. The reason we do this is because we want the
-- compiler to able to infer @a ~ b@ if it knows that @'LayerResult' t a ~
-- 'LayerResult' t b@. It couldn't do this if we allowed definitions like the
-- above because
-- <http://www.haskell.org/haskellwiki/GHC/Type_families#Injectivity.2C_type_inference.2C_and_ambiguity type families are not injective>.
-- This is why we set @'LayerResult' 'IdentityT'@ to 'Identity'.
-- (@'Identity' a@ is of course isomorphic to @a@.)
--
-- Now, what if our monad transformer has a 'LayerState' that isn't just @()@?
-- Let's consider 'RWST' again. The type inside the @m@ computation wrapped by
-- 'RWST' is @(a, s, w)@, but its 'LayerResult' is @(,) w@. Now, @(,) w a@
-- expands to @(w, a@), which is isomorphic to @(a, w)@, but what about the
-- @s@ in the middle?
--
-- The answer is that monad transformers which have a 'LayerState' value often
-- update all or part of it as one of the side-effects of their computations.
-- This is the case with 'RWST', where the @s@ value you see in the \"result\"
-- is actually an updated value of the @s@ which is part of the 'LayerState'
-- of 'RWST', not part of the result proper.
--
-- This actually captures everything we need to implement the 'suspend'
-- operation we described above:
--
-- @
-- 'suspend' :: 'Monad' m => t m a -> 'LayerState' t m -> m ('LayerEffects' t m a)
-- @
--
-- @
-- type 'LayerEffects' t m a = ('LayerResult' t a, 'LayerState' t m)
-- @
--
-- (The purpose of the 'LayerEffects' type synonym is twofold: it makes the
-- type signatures of 'suspend' and other operatoins a little bit less scary,
-- and it also communicates that the combination of a 'LayerResult' and a
-- 'LayerState' together reify the side-effects of a monad transformer.)
--
-- There are two important operations in the 'MonadTransControl' that we have
-- only alluded to so far: 'capture' and 'resume'.
--
-- @
-- 'capture' :: ('MonadTransControl' t, 'Monad' m) => t m ('LayerState' t m)
-- 'resume' :: ('MonadTransControl' t, 'Monad' m) => 'LayerEffects' t m a -> t m a
-- @
--
-- 'capture' captures the current @'LayerState' t m@ for the monad @t m@. This
-- is where the 'LayerState' that 'suspend' takes as its argument comes from.
-- 'resume' is the inverse of 'suspend': it takes the suspended side-effects
-- of a monad transformer @t@ reified by a @'LayerEffects' t m a@ value, and
-- returns a returns a reconstructed computation of type @t m a@ with those
-- side-effects.
--
-- Taken together, we can use these operations to define @f'@, a lifted
-- version of the @f@ operation described above.
--
-- @
-- f' :: ('MonadTransControl' t, 'Monad' (t m), 'Monad' m) => t m a -> t m b
-- f' t = 'capture' '>>=' 'lift' '.' f '.' 'suspend' t '>>=' 'resume'
-- @
--
-- The full instance for 'RWST' is given below:
--
-- @
-- instance 'Monoid' w => 'MonadTransControl' ('RWST' r w s) where
--     type 'LayerResult' ('RWST' r w s) = (,) w
--     type 'LayerState' ('RWST' r w s) m = (r, s)
--     'suspend' ('RWST' m) (r, s) = 'liftM' (\(a, s', w) -> ((w, a), (r, s'))) (m r s)
--     'resume' ((w, a), (_, s)) = 'RWST' '$' \_ _ -> 'return' (a, s, w)
--     'capture' = 'RWST' '$' \r s -> 'return' ((r, s), s, 'mempty')
--     'extract' _ (_, a) = 'Just' a
-- @
class MonadTrans t => MonadTransControl t where
    -- | The part of the result type of the inner function of @t@ which is not
    -- part of the (updated) 'LayerState'.
    --
    -- Note: On versions of GHC prior to 7.4, 'LayerResult' is an associated
    -- /data/ type instead of an associated type synonym due to GHC bug
    -- <http://hackage.haskell.org/trac/ghc/ticket/5595 #5595>. If you're
    -- defining an instance of 'MonadTransControl' and you want your code to
    -- work on older versions of GHC as well, you're unfortunately going to
    -- have to write two versions of the instance, once using associated type
    -- synonyms, the other using associated data types, and then use @CPP@
    -- pragmas to switch between them.
#if __GLASGOW_HASKELL__ >= 704
    type LayerResult t :: * -> *
#else
    data LayerResult t :: * -> *
#endif

    -- | The parameters needed by the inner function of @t@ to return a
    -- computation in the monad @m@. We call these parameters \"state\",
    -- because a component of the return value of the @m@-computation returned
    -- by @t@'s inner function is often meant to update one or more of these
    -- parameters, like a 'Control.Monad.Trans.State.Strict.State' monad.
    --
    -- Note: On versions of GHC prior to 7.4, 'LayerState' is an associated
    -- /data/ type instead of an associated type synonym due to GHC bug
    -- <http://hackage.haskell.org/trac/ghc/ticket/5595 #5595>. If you're
    -- defining an instance of 'MonadTransControl' and you want your code to
    -- work on older versions of GHC as well, you're unfortunately going to
    -- have to write two versions of the instance, once using associated type
    -- synonyms, the other using associated data types, and then use @CPP@
    -- pragmas to switch between them.
#if __GLASGOW_HASKELL__ >= 704
    type LayerState t (m :: * -> *) :: *
#else
    data LayerState t :: (* -> *) -> *
#endif

    -- | 'suspend', given a computation @m@ of type @t m a@ and the current
    -- 'LayerState' of the monad @t m@ (given by 'capture'), suspends the
    -- side-effects of @m@ which come from the monad transformer @t@ by
    -- returning a computation in the monad @m@ that returns these reified
    -- side-effects (i.e., a @'LayerEffects' t m a@ value). This gives a
    -- version of @m@ which can be passed to control operations in the monad
    -- @m@.
    --
    -- The suspended side-effects of @t@ can later be recovered by 'resume'.
    -- This is expressed in the following law:
    --
    -- [Preservation] @'capture' '>>=' 'lift' '.' 'suspend' t '>>=' 'resume' ≡ t@
    suspend :: Monad m => t m a -> LayerState t m -> m (LayerEffects t m a)

    -- | Reconstructs a compuation @t m a@ with the same side-effects in monad
    -- transformer @t@ as those reified by the given @'LayerEffects' t m a@
    -- value.
    --
    -- Instances should satisfy the following law:
    --
    -- [Preservation] @'capture' '>>=' 'lift' '.' 'suspend' t '>>=' 'resume' ≡ t@
    resume :: Monad m => LayerEffects t m a -> t m a

    -- | Captures the current 'LayerState' of the monad transformer @t@ in the
    -- monad @t m@. This can be passed to 'suspend' along with a computation
    -- in the monad @t m@ to suspend its side-effects in @t@.
    --
    -- Instances should satisfy the following law:
    --
    -- [Preservation] @'capture' '>>=' 'lift' '.' 'suspend' t '>>=' 'resume' ≡ t@
    capture :: Monad m => t m (LayerState t m)

    -- | 'extract' inspects a @'LayerResult' t a@ value (a component of the
    -- reified side-effects of the monad transformer @t@ given by 'suspend')
    -- and tries to \"extract\" an @a@ value from it, if possible. If not,
    -- this means that one of the side-effects reified by the given value is a
    -- short-circuit.
    --
    -- Instances should satisfy the following laws:
    --
    -- [Preserve-Unit]
    --     @extractResult ('return' a) ≡ 'return' ('Just' a)@
    --
    -- [Implies-Zero]
    --     @(extractResult m
    --         ≡ 'liftM' ('const' 'Nothing') m) ⇒ (∀f. m '>>=' f ≡ m)@
    --
    -- The @extractResult@ operation in terms of which these laws are defined
    -- is given by:
    --
    -- @
    -- extractResult :: forall t m a. ('MonadTransControl' t, 'Monad' (t m), 'Monad' m)
    --     => t m a
    --     -> t m ('Maybe' a)
    -- extractResult t = do
    --     state <- 'capture'
    --     'lift' '$' do
    --         (result, _) <- 'suspend' t state
    --         'return' '$' 'extract' ('Data.Proxy.Proxy' :: 'Data.Proxy.Proxy' t) result
    -- @
    extract :: proxy t -> LayerResult t a -> Maybe a


------------------------------------------------------------------------------
-- | We can reify the side-effects in the monad transformer @t@ of a
-- computation of type @t m a@ with a combination of its  @'LayerResult' t a@
-- and an updated @'LayerState' t m@.
type LayerEffects t m a = (LayerResult t a, LayerState t m)


------------------------------------------------------------------------------
instance Error e => MonadTransControl (ErrorT e) where
#if __GLASGOW_HASKELL__ >= 704
    type LayerResult (ErrorT e) = Either e
    type LayerState (ErrorT e) m = ()
    suspend (ErrorT m) _ = liftM (\a -> (a, ())) m
    resume (a, _) = ErrorT $ return a
    capture = return ()
    extract _ = either (const Nothing) Just
#else
    newtype LayerResult (ErrorT e) a = ER (Either e a)
    newtype LayerState (ErrorT e) m = ES ()
    suspend (ErrorT m) _ = liftM (\a -> (ER a, ES ())) m
    resume (ER a, _) = ErrorT $ return a
    capture = return (ES ())
    extract _ (ER e) = either (const Nothing) Just e
#endif


------------------------------------------------------------------------------
instance MonadTransControl IdentityT where
#if __GLASGOW_HASKELL__ >= 704
    type LayerResult IdentityT = Identity
    type LayerState IdentityT m = ()
    suspend (IdentityT m) _ = liftM (\a -> (Identity a, ())) m
    resume (Identity a, _) = IdentityT $ return a
    capture = return ()
    extract _ (Identity a) = Just a
#else
    newtype LayerResult IdentityT a = IR a
    newtype LayerState IdentityT m = IS ()
    suspend (IdentityT m) _ = liftM (\a -> (IR a, IS ())) m
    resume (IR a, _) = IdentityT $ return a
    capture = return (IS ())
    extract _ (IR a) = Just a
#endif


------------------------------------------------------------------------------
instance MonadTransControl ListT where
#if __GLASGOW_HASKELL__ >= 704
    type LayerResult ListT = []
    type LayerState ListT m = ()
    suspend (ListT m) _ = liftM (\a -> (a, ())) m
    resume (a, _) = ListT $ return a
    capture = return ()
    extract _ = foldr (const . Just) Nothing
#else
    newtype LayerResult ListT a = LR [a]
    newtype LayerState ListT m = LS ()
    suspend (ListT m) _ = liftM (\a -> (LR a, LS ())) m
    resume (LR a, _) = ListT $ return a
    capture = return (LS ())
    extract _ (LR xs) = foldr (const . Just) Nothing xs
#endif


------------------------------------------------------------------------------
instance MonadTransControl MaybeT where
#if __GLASGOW_HASKELL__ >= 704
    type LayerResult MaybeT = Maybe
    type LayerState MaybeT m = ()
    suspend (MaybeT m) _ = liftM (\a -> (a, ())) m
    resume (a, _) = MaybeT $ return a
    capture = return ()
    extract _ = id
#else
    newtype LayerResult MaybeT a = MR (Maybe a)
    newtype LayerState MaybeT m = MS ()
    suspend (MaybeT m) _ = liftM (\a -> (MR a, MS ())) m
    resume (MR a, _) = MaybeT $ return a
    capture = return (MS ())
    extract _ (MR a) = a
#endif


------------------------------------------------------------------------------
instance MonadTransControl (ReaderT r) where
#if __GLASGOW_HASKELL__ >= 704
    type LayerResult (ReaderT r) = Identity
    type LayerState (ReaderT r) m = r
    suspend (ReaderT m) r = liftM (\a -> (Identity a, r)) (m r)
    resume (Identity a, _) = ReaderT $ \_ -> return a
    capture = ReaderT $ \r -> return r
    extract _ (Identity a) = Just a
#else
    newtype LayerResult (ReaderT r) a = RR a
    newtype LayerState (ReaderT r) m = RS r
    suspend (ReaderT m) (RS r) = liftM (\a -> (RR a, RS r)) (m r)
    resume (RR a, _) = ReaderT $ \_ -> return a
    capture = ReaderT $ \r -> return (RS r)
    extract _ (RR a) = Just a
#endif


------------------------------------------------------------------------------
instance MonadTransControl (StateT s) where
#if __GLASGOW_HASKELL__ >= 704
    type LayerResult (StateT s) = Identity
    type LayerState (StateT s) m = s
    suspend (StateT m) s = liftM (first Identity) (m s)
    resume (Identity a, s) = StateT $ \_ -> return (a, s)
    capture = StateT $ \s -> return (s, s)
    extract _ (Identity a) = Just a
#else
    newtype LayerResult (StateT s) a = SR a
    newtype LayerState (StateT s) m = SS s
    suspend (StateT m) (SS s) = liftM (SR *** SS) (m s)
    resume (SR a, SS s) = StateT $ \_ -> return (a, s)
    capture = StateT $ \s -> return (SS s, s)
    extract _ (SR a) = Just a
#endif


------------------------------------------------------------------------------
instance MonadTransControl (L.StateT s) where
#if __GLASGOW_HASKELL__ >= 704
    type LayerResult (L.StateT s) = Identity
    type LayerState (L.StateT s) m = s
    suspend (L.StateT m) s = liftM (first Identity) (m s)
    resume (Identity a, s) = L.StateT $ \_ -> return (a, s)
    capture = L.StateT $ \s -> return (s, s)
    extract _ (Identity a) = Just a
#else
    newtype LayerResult (L.StateT s) a = SR' a
    newtype LayerState (L.StateT s) m = SS' s
    suspend (L.StateT m) (SS' s) = liftM (SR' *** SS') (m s)
    resume (SR' a, SS' s) = L.StateT $ \_ -> return (a, s)
    capture = L.StateT $ \s -> return (SS' s, s)
    extract _ (SR' a) = Just a
#endif


------------------------------------------------------------------------------
instance Monoid w => MonadTransControl (RWST r w s) where
#if __GLASGOW_HASKELL__ >= 704
    type LayerResult (RWST r w s) = (,) w
    type LayerState (RWST r w s) m = (r, s)
    suspend (RWST m) (r, s) = liftM (\(a, s', w) -> ((w, a), (r, s'))) (m r s)
    resume ((w, a), (_, s)) = RWST $ \_ _ -> return (a, s, w)
    capture = RWST $ \r s -> return ((r, s), s, mempty)
    extract _ (_, a) = Just a
#else
    newtype LayerResult (RWST r w s) a = RWSR (a, w)
    newtype LayerState (RWST r w s) m = RWSS (r, s)
    suspend (RWST m) (RWSS (r, s)) =
        liftM (\(a, s', w) -> (RWSR (a, w), RWSS (r, s'))) (m r s)
    resume (RWSR (a, w),  RWSS (_, s)) = RWST $ \_ _ -> return (a, s, w)
    capture = RWST $ \r s -> return (RWSS (r, s), s, mempty)
    extract _ (RWSR (a, _)) = Just a
#endif


------------------------------------------------------------------------------
instance Monoid w => MonadTransControl (L.RWST r w s) where
#if __GLASGOW_HASKELL__ >= 704
    type LayerResult (L.RWST r w s) = (,) w
    type LayerState (L.RWST r w s) m = (r, s)
    suspend (L.RWST m) (r, s) = liftM (\(a, s', w) -> ((w, a), (r, s'))) (m r s)
    resume ((w, a), (_, s)) = L.RWST $ \_ _ -> return (a, s, w)
    capture = L.RWST $ \r s -> return ((r, s), s, mempty)
    extract _ (_, a) = Just a
#else
    newtype LayerResult (L.RWST r w s) a = RWSR' (a, w)
    newtype LayerState (L.RWST r w s) m = RWSS' (r, s)
    suspend (L.RWST m) (RWSS' (r, s)) =
        liftM (\(a, s', w) -> (RWSR' (a, w), RWSS' (r, s'))) (m r s)
    resume (RWSR' (a, w), RWSS' (_, s)) = L.RWST $ \_ _ -> return (a, s, w)
    capture = L.RWST $ \r s -> return (RWSS' (r, s), s, mempty)
    extract _ (RWSR' (a, _)) = Just a
#endif


------------------------------------------------------------------------------
instance Monoid w => MonadTransControl (WriterT w) where
#if __GLASGOW_HASKELL__ >= 704
    type LayerResult (WriterT w) = (,) w
    type LayerState (WriterT w) m = ()
    suspend (WriterT m) _ = liftM (\(a, w) -> ((w, a), ())) m
    resume ((w, a), _) = WriterT $ return (a, w)
    capture = return ()
    extract _ (_, a) = Just a
#else
    newtype LayerResult (WriterT w) a = WR (a, w)
    newtype LayerState (WriterT w) m = WS ()
    suspend (WriterT m) _ = liftM (\a -> (WR a, WS ())) m
    resume (WR a, _) = WriterT $ return a
    capture = return (WS ())
    extract _ (WR (a, _)) = Just a
#endif


------------------------------------------------------------------------------
instance Monoid w => MonadTransControl (L.WriterT w) where
#if __GLASGOW_HASKELL__ >= 704
    type LayerResult (L.WriterT w) = (,) w
    type LayerState (L.WriterT w) m = ()
    suspend (L.WriterT m) _ = liftM (\(a, w) -> ((w, a), ())) m
    resume ((w, a), _) = L.WriterT $ return (a, w)
    capture = return ()
    extract _ (_, a) = Just a
#else
    newtype LayerResult (L.WriterT w) a = WR' (a, w)
    newtype LayerState (L.WriterT w) m = WS' ()
    suspend (L.WriterT m) _ = liftM (\a -> (WR' a, WS' ())) m
    resume ((WR' a), _) = L.WriterT $ return a
    capture = return (WS' ())
    extract _ (WR' (a, _)) = Just a
#endif


------------------------------------------------------------------------------
-- | 'liftControl' is a composition of 'capture', 'suspend' and 'lift'
-- provided for convenience (and compability with
-- @<http://hackage.haskell.org/package/monad-control monad-control>@).
--
-- It takes a continuation, to which it passes a version of 'suspend' (usually
-- called @peel@), which can be used to effectively \"lower\" a computation
-- in the monad @t m a@ to @m a@ (storing the captured side-effects of @t@ in
-- the return value).
liftControl :: (MonadTransControl t, Monad (t m), Monad m)
    => ((forall b. t m b -> m (LayerEffects t m b)) -> m a)
    -> t m a
liftControl f = capture >>= \s -> lift $ f (flip suspend s)
{-# INLINABLE liftControl #-}


------------------------------------------------------------------------------
-- | A version of 'liftControl' that automatically resumes the captured
-- side-effects returned from the continuation.
--
-- @
-- catch' :: ('Control.Exception.Exception' e, 'MonadTransControl' t, 'Monad' (t 'IO')) => t m b -> (e -> t m b) -> t m b
-- catch' m h = 'control' (\peel -> 'Control.Exception.catch' (peel m) (peel '.' h))
-- @
control :: (MonadTransControl t, Monad (t m), Monad m)
    => ((forall b. t m b -> m (LayerEffects t m b)) -> m (LayerEffects t m a))
    -> t m a
control f = liftControl f >>= resume
{-# INLINABLE control #-}


------------------------------------------------------------------------------
-- | A particular application of 'liftControl' that lifts control operations
-- from @(a -> m b) -> m c@ to @(a -> t m b) -> t m c@.
--
-- @
-- withMVar' :: ('MonadTransControl' t, 'Monad' (t 'IO')) => 'Control.Concurrent.MVar.MVar' a -> (a -> t 'IO' b) -> t 'IO' b
-- withMVar' = 'liftOp' '.' 'Control.Concurrent.MVar.withMVar'
-- @
liftOp :: (MonadTransControl t, Monad (t m), Monad m)
    => ((a -> m (LayerEffects t m b)) -> m (LayerEffects t m c))
    -> (a -> t m b)
    -> t m c
liftOp f = \g -> control (\peel -> f $ peel . g)
{-# INLINABLE liftOp #-}


------------------------------------------------------------------------------
-- | A particular application of 'liftControl' that lifts control operations
-- from @m a -> m b@ to @t m a -> t m b@.
--
-- @
-- mask_' :: ('MonadTransControl' t, 'Monad' (t 'IO')) => t 'IO' a -> t 'IO' a
-- mask_' = 'liftOp_' 'Control.Exception.mask_'
-- @
liftOp_ :: (MonadTransControl t, Monad (t m), Monad m)
    => (m (LayerEffects t m a) -> m (LayerEffects t m b))
    -> t m a
    -> t m b
liftOp_ f = \m -> control (\peel -> f $ peel m)
{-# INLINABLE liftOp_ #-}


------------------------------------------------------------------------------
-- | A particular application of 'liftControl' that lifts control operations
-- from @m () -> m a@ to @t m () -> t m a@.
--
-- @
-- forkIO' :: ('MonadTransControl' t, Monad (t 'IO')) => t m () -> t m 'Control.Concurrent.ThreadId'
-- forkIO' = 'liftDiscard' 'Control.Concurrent.forkIO'
-- @
--
-- Note: While the computation @t m ()@ passed to the resulting operation has
-- access to the 'LayerEffects' of @t@, it is run only for its side-effects
-- in @m@. Its side-effects in @t@ are discarded.
liftDiscard :: (MonadTransControl t, Monad (t m), Monad m)
    => (m () -> m a)
    -> t m ()
    -> t m a
liftDiscard f m = liftControl $ \peel -> f $ liftM (const ()) $ peel m
{-# INLINABLE liftDiscard #-}


------------------------------------------------------------------------------
-- | An invariant functor in the category of monads, using 'hoistiso' as the
-- analog of
-- @<http://hackage.haskell.org/package/invariant/docs/Data-Functor-Invariant.html#t:Invariant invmap>@:
class MInvariant t where
    -- | Lift a monad isomorphism between @m@ and @n@ into a monad morphism
    -- from @(t m)@ to @(t n)@.
    --
    -- The following laws hold for valid instances of 'MInvariant':
    --
    --     [Identity] @'hoistiso' 'id' 'id' ≡ 'id'@
    --
    --     [Composition]
    --         @'hoistiso' f g '.' 'hoistiso' f' g' ≡
    --             'hoistiso' (f '.' f') (g' '.' g)@
    --
    -- Note: The homomorphism produced by @'hoistiso' f g@ is only valid if
    -- @f@ and @g@ form a valid isomorphism, i.e., @f '.' g ≡ 'id'@ and
    -- @g '.' f ≡ 'id'@.
    hoistiso :: Monad m
        => (forall b. m b -> n b)
        -> (forall b. n b -> m b)
        -> t m a
        -> t n a


------------------------------------------------------------------------------
instance MInvariant (ContT r) where
    hoistiso f g (ContT m) = ContT $ f . m . (g .)


------------------------------------------------------------------------------
instance MInvariant (ErrorT e) where
    hoistiso f _ = hoist f


------------------------------------------------------------------------------
instance MInvariant IdentityT where
    hoistiso f _ = hoist f


------------------------------------------------------------------------------
instance MInvariant ListT where
    hoistiso f _ = \(ListT m) -> ListT $ f m


------------------------------------------------------------------------------
instance MInvariant MaybeT where
    hoistiso f _ = hoist f


------------------------------------------------------------------------------
instance MInvariant (ReaderT r) where
    hoistiso f _ = hoist f


------------------------------------------------------------------------------
instance MInvariant (RWST r w s) where
    hoistiso f _ = hoist f


------------------------------------------------------------------------------
instance MInvariant (L.RWST r w s) where
    hoistiso f _ = hoist f


------------------------------------------------------------------------------
instance MInvariant (StateT s) where
    hoistiso f _ = hoist f


------------------------------------------------------------------------------
instance MInvariant (L.StateT s) where
    hoistiso f _ = hoist f


------------------------------------------------------------------------------
instance MInvariant (WriterT w) where
    hoistiso f _ = hoist f


------------------------------------------------------------------------------
instance MInvariant (L.WriterT w) where
    hoistiso f _ = hoist f


------------------------------------------------------------------------------
-- | The constraint @'MonadInner' i m@ holds when @i@ is an
-- <Documentation-Layers-Glossary.html#innermonad inner monad> of @m@ such
-- that it is possible to lift computations from @i@ into @m@ using 'liftI'.
class (Monad i, Monad m) => MonadInner i m where
    -- | 'liftI' takes a computation from an inner monad @i@ of @m@ and lifts
    -- it into @m@.
    --
    -- The following laws hold for valid instances of 'MonadInner':
    --
    --     [Identity] @'liftI' '.' 'return' ≡ 'return'@
    --
    --     [Composition] @'liftI' m '>>=' 'liftI' '.' f ≡ 'liftI' (m '>>=' f)@
    --
    -- The difference between 'liftI' and 'lift' is that 'lift' only lifts
    -- from the monad directly beneath the top of the stack, while 'liftI' can
    -- lift from /any/ monad anywhere in the stack (including @m@ itself).
    --
    -- If you know that you want to lift from the monad directly beneath the
    -- top of the stack, it's often better to use 'lift' than 'liftI'. This
    -- improves type inference because 'lift' is less polymorphic than
    -- 'liftI'. Similarly, you might also consider using
    -- 'Control.Monad.Lift.IO.liftIO' or 'Control.Monad.Lift.Base.liftBase'
    -- if you know that the monad from which you want to lift is 'IO' or the
    -- <Control-Monad-Lift-Base.html base monad> of a transformer stack.
    liftI :: i a -> m a


------------------------------------------------------------------------------
instance Monad m => MonadInner m m where
    liftI = id


------------------------------------------------------------------------------
instance (Monad m, Monad (t m)) => MonadInner (t m) (t m) where
    liftI = id


------------------------------------------------------------------------------
instance (MonadTrans t, Monad (t m), MonadInner i m) => MonadInner i (t m) where
    liftI = lift . liftI


------------------------------------------------------------------------------
-- | The constraint @'MonadInnerControl' i m@ holds when @i@ is an
-- <Documentation-Layers-Glossary.html#innermonad inner monad> of @m@ such
-- that it is possible to lift control operations from @i@ to @m@. There are a
-- variety of operations for doing so, depending on the exact type of the
-- control operation in question, including 'liftIControl', 'controlI',
-- 'liftIOp', 'liftIOp_' and 'liftIDiscard'. These are all built on top of
-- the more primitive 'captureI', 'suspendI' and 'resumeI' operations.
class MonadInner i m => MonadInnerControl i m where
    -- | 'suspendI', given a computation @m@ of type @m a@ and the current
    -- @'OuterState' i@ of the monad @m@ (given by calling 'captureI' with a
    -- proxy argument to select the @i@), suspends the side-effects of @m@
    -- which come from the outer layers around @i@ of the monad @m@, returning
    -- a computation in the monad @i@ that returns these side-effects reified
    -- by an @'OuterEffects' i m a@ value. This gives a version of @m@ which
    -- can be passed to control operations in the monad @i@.
    --
    -- The suspended side-effects of @m@ can later be recovered by 'resumeI'.
    -- This is expressed in the following law:
    --
    -- [Preservation] @'captureI' ('Data.Proxy.Proxy' :: 'Data.Proxy.Proxy' i) '>>=' 'liftI' '.' 'suspendI' t '>>=' 'resumeI' ('Data.Proxy.Proxy' :: 'Data.Proxy.Proxy' i) ≡ t@
    suspendI :: m a -> OuterState i m -> i (OuterEffects i m a)

    -- | Reconstructs a compuation @m a@ with the same side-effects in the
    -- outer layers of the monad @m@ that wrap around the monad @i@ as those
    -- reified by the given @'OuterEffects' i m a@ value.
    --
    -- Instances should satisfy the following law:
    --
    -- [Preservation] @'captureI' ('Data.Proxy.Proxy' :: 'Data.Proxy.Proxy' i) '>>=' 'liftI' '.' 'suspendI' t '>>=' 'resumeI' ('Data.Proxy.Proxy' :: 'Data.Proxy.Proxy' i) ≡ t@
    resumeI :: proxy i -> OuterEffects i m a -> m a

    -- | Captures the current 'OuterState' of the outer layers of the monad
    -- @m@ that wrap around the monad @i@. This can be passed to 'suspendI'
    -- along with a computation in the monad @m@ to suspend its side-effects
    -- in these outer layers.
    --
    -- Instances should satisfy the following law:
    --
    -- [Preservation] @'captureI' ('Data.Proxy.Proxy' :: 'Data.Proxy.Proxy' i) '>>=' 'liftI' '.' 'suspendI' t '>>=' 'resumeI' ('Data.Proxy.Proxy' :: 'Data.Proxy.Proxy' i) ≡ t@
    captureI :: proxy i -> m (OuterState i m)

    -- | 'extractI' inspects an @'OuterResult' i m a@ value (a component of
    -- the reified side-effects of the outer layers of the monad @m@ around @i@
    -- given by 'suspendI') and tries to \"extract\" an @a@ value from it, if possible.
    -- If not, this means that one of the side-effects reified by the given
    -- value is a short-circuit.
    --
    -- Instances should satisfy the following laws:
    --
    -- [Preserve-Unit]
    --     @extractResult' ('Data.Proxy.Proxy' :: 'Data.Proxy.Proxy' i) ('return' a)
    --         ≡ 'return' ('Just' a)@
    --
    -- [Implies-Zero]
    --     @(extractResult' ('Data.Proxy.Proxy' :: 'Data.Proxy.Proxy' i) m
    --         ≡ 'liftM' ('const' 'Nothing') m) ⇒ (∀f. m '>>=' f ≡ m)@
    --
    -- The @extractResult'@ operation in terms of which these laws are defined
    -- is given by:
    --
    -- @
    -- extractResult' :: forall proxy i m a. 'MonadInnerControl' i m
    --     => proxy i
    --     -> m a
    --     -> m ('Maybe' a)
    -- extractResult' i m = do
    --     state <- 'capture'' i
    --     'lift'' '$' do
    --         (result, _) <- 'suspend'' m state
    --         'return' '$' 'extract'' i ('Data.Proxy.Proxy' :: 'Data.Proxy.Proxy' m) result
    -- @
    extractI :: proxy i -> proxy' m -> OuterResult i m a -> Maybe a


------------------------------------------------------------------------------
-- | The combined
-- <Documentation-Layers-Glossary.html#layerresult layer effects> of all the
-- <Documentation-Layers-Glossary.html#outerlayer outer layers> around @i@ of
-- the monad @m@.
type OuterEffects i m a = (OuterResult i m a, OuterState i m)


------------------------------------------------------------------------------
-- | The combined
-- <Documentation-Layers-Glossary.html#layerresult layer results> of all the
-- <Documentation-Layers-Glossary.html#outerlayer outer layers> around @i@ of
-- the monad @m@.
--
-- Note: On GHC 7.8 and up, this is implemented as a
-- <http://ghc.haskell.org/trac/ghc/wiki/NewAxioms/ClosedTypeFamilies closed type family>.
-- Older versions of GHC do not support closed type families, but we use
-- various hacks involving 'Any' and 'unsafeCoerce' to provide the same
-- interface. You should not need to worry about this; I am pretty sure it is
-- safe.
#if __GLASGOW_HASKELL__ >= 704
type family OuterResult (i :: * -> *) (m :: * -> *) :: * -> *
#if __GLASGOW_HASKELL__ >= 707
  where
    OuterResult m m = Identity
    OuterResult i (t m) = ComposeResult i t m
    OuterResult i m = OuterResult_ i m -- this is only for newtypes, see
                                       -- the defaults section
-- closed type families are only supported on GHC 7.8 and above
#else
type instance OuterResult i m = OuterResult_ i m
#endif
#else
type OuterResult i m = OuterResult_ i m
-- we can't use a type family on GHC 7.2 and older because we run into GHC
-- bug <http://hackage.haskell.org/trac/ghc/ticket/5595 #5595>, so we use a
-- type synonym instead
#endif


------------------------------------------------------------------------------
newtype OuterResult_ (i :: * -> *) (m :: * -> *) (a :: *) = OuterResult_ Any


------------------------------------------------------------------------------
-- | The combined
-- <Documentation-Layers-Glossary.html#layerresult layer states> of all the
-- <Documentation-Layers-Glossary.html#outerlayer outer layers> around @i@ of
-- the monad @m@.
--
-- Note: On GHC 7.8 and up, this is implemented as a
-- <http://ghc.haskell.org/trac/ghc/wiki/NewAxioms/ClosedTypeFamilies closed type family>.
-- Older versions of GHC do not support closed type families, but we use
-- various hacks involving 'Any' and 'unsafeCoerce' to provide the same
-- interface. You should not need to worry about this; I am pretty sure it is
-- safe.
#if __GLASGOW_HASKELL__ >= 704
type family OuterState (i :: * -> *) (m :: * -> *) :: *
#if __GLASGOW_HASKELL__ >= 707
  where
    OuterState m m = ()
    OuterState i (t m) = (LayerState t m, OuterState i m)
    OuterState i m = OuterState_ i m -- this is only for newtypes, see
                                     -- the defaults section
-- closed type families are only supported on GHC 7.8 and above
#else
type instance OuterState i m = OuterState_ i m
#endif
#else
type OuterState i m = OuterState_ i m
-- we can't use a type family on GHC 7.2 and older because we run into GHC
-- bug <http://hackage.haskell.org/trac/ghc/ticket/5595 #5595>, so we use a
-- type synonym instead
#endif


------------------------------------------------------------------------------
newtype OuterState_ (i :: * -> *) (m :: * -> *) = OuterState_ Any


------------------------------------------------------------------------------
newtype ComposeResult i t m a
    = ComposeResult (OuterResult i m (LayerResult t a, LayerState t m))


------------------------------------------------------------------------------
#if __GLASGOW_HASKELL__ >= 707
toS, fromS, toR, fromR :: a -> a
toS = id; fromS = id; toR = id; fromR = id
#else
toS :: x -> OuterState_ i m; toR :: x -> OuterResult_ i m a
toS = toOuterState_; toR = toOuterResult_;
fromS :: OuterState_ i m -> x; fromR :: OuterResult_ i m a -> x
fromS = fromOuterState_; fromR = fromOuterResult_
#endif
{-# INLINE toS #-}
{-# INLINE toR #-}
{-# INLINE fromS #-}
{-# INLINE fromR #-}


------------------------------------------------------------------------------
toOuterState_ :: x -> OuterState_ i m
toOuterState_ = OuterState_ . unsafeCoerce


------------------------------------------------------------------------------
fromOuterState_ :: OuterState_ i m -> x
fromOuterState_ (OuterState_ x) = unsafeCoerce x


------------------------------------------------------------------------------
toOuterResult_ :: x -> OuterResult_ i m a
toOuterResult_ = OuterResult_ . unsafeCoerce


------------------------------------------------------------------------------
fromOuterResult_ :: OuterResult_ i m a -> x
fromOuterResult_ (OuterResult_ x) = unsafeCoerce x


------------------------------------------------------------------------------
instance MonadInner m m => MonadInnerControl m m where
    suspendI m _ = liftM (\a -> (toR $ Identity a, toS ())) m
    resumeI _ (r, _) = let Identity a = fromR r in return a
    captureI _ = return $ toS ()
    extractI _ _ r = let Identity a = fromR r in Just a


------------------------------------------------------------------------------
instance (Monad m, MonadInner (t m) (t m)) => MonadInnerControl (t m) (t m)
  where
    suspendI m _ = liftM (\a -> (toR $ Identity a, toS ())) m
    resumeI _ (r, _) = let Identity a = fromR r in return a
    captureI _ = return $ toS ()
    extractI _ _ r = let Identity a = fromR r in Just a


------------------------------------------------------------------------------
instance
    ( MonadTransControl t
    , Monad (t m)
    , MonadInner i (t m)
    , MonadInnerControl i m
#if __GLASGOW_HASKELL__ >= 707
    , OuterResult i (t m) ~ ComposeResult i t m
    , OuterState i (t m) ~ (LayerState t m, OuterState i m)
#endif
    )
  =>
    MonadInnerControl i (t m)
  where
    suspendI (m :: t m a) s = do
        let (ls, os) = fromS s
        let compose or_ = ComposeResult or_ :: ComposeResult i t m a
        let f (or_, os') = (toR (compose or_), toS (ls, os'))
        liftM f $ suspendI (suspend m ls) os

    resumeI p ((r, s) :: OuterEffects i (t m) a) = do
        let ComposeResult r' = (fromR r :: ComposeResult i t m a)
        let (_, s') = fromS s
        lift (resumeI p (r', s')) >>= resume

    captureI p = capture >>= \a -> lift (captureI p) >>= \b ->
        return $ toS (a, b)

    extractI _ _ (r :: OuterResult i (t m) a) =
        let ComposeResult r' = (fromR r :: ComposeResult i t m a) in join $
            fmap (extract (Pt :: Pt t) . fst) $
                extractI (Pm :: Pm i) (Pm :: Pm m) r'


------------------------------------------------------------------------------
data Pm (m :: * -> *) = Pm


------------------------------------------------------------------------------
data Pt (t :: (* -> *) -> * -> *) = Pt


------------------------------------------------------------------------------
-- | 'liftControlI' is a version of 'liftI' that makes it possible to lift
-- control operations from the inner monad @i@ to the outer monad @m@. It
-- takes a continuation, to which it passes a version of 'suspend'' (usually
-- called @peel@), which is kind of an \"inverse\" of 'liftI'.
--
-- The difference between 'liftControlI' and 'liftControl' is that
-- 'liftControl' only lifts from the monad directly beneath the top of the
-- stack, while 'liftControlI' can lift from /any/ monad anywhere in the stack
-- (including @m@ itself).
--
-- If you know that you want to lift from the monad directly beneath the top
-- of the stack, it's often better to use 'liftControl' than 'liftControlI'.
-- This improves type inference because 'liftControl' is less polymorphic than
-- 'liftControlI'. Similarly, you might also consider using
-- 'Control.Monad.Lift.IO.liftControlIO' or
-- 'Control.Monad.Lift.Base.liftBaseControl' if you know that the monad from
-- which you want to lift is 'IO' or the <Control-Monad-Lift-Base.html base>
-- monad of a transformer stack.
liftControlI :: forall i m a. MonadInnerControl i m
    => ((forall b. m b -> i (OuterEffects i m b)) -> i a)
    -> m a
liftControlI f = captureI (Pm :: Pm i) >>= \s -> liftI $
    f (flip suspendI s)
{-# INLINABLE liftControlI #-}


------------------------------------------------------------------------------
-- | A version of 'liftControlI' that automatically restores to the outer
-- monad the captured side-effects returned from the continuation.
--
-- @
-- catch' :: ('Control.Exception.Exception' e, 'MonadInnerControl' 'IO' m) => m b -> (e -> m b) -> m b
-- catch' m h = 'controlI' (\peel -> 'Control.Exception.catch' (peel m) (peel '.' h))
-- @
--
-- The difference between 'controlI' and 'control' is that 'control' only
-- lifts from the monad directly beneath the top of the stack, while
-- 'controlI' can lift from /any/ monad anywhere in the stack (including @m@
-- itself).
--
-- If you know that you want to lift from the monad directly beneath the top
-- of the stack, it's often better to use 'control' than 'controlI'. This
-- improves type inference because 'control' is less polymorphic than
-- 'controlI'. Similarly, you might also consider using
-- 'Control.Monad.Lift.IO.controlIO' or 'Control.Monad.Lift.Base.controlBase'
-- if you know that the monad from which you want to lift is 'IO' or the
-- <Control-Monad-Lift-Base.html base monad> of a transformer stack.
controlI :: forall i m a. MonadInnerControl i m
    => ((forall b. m b -> i (OuterEffects i m b)) -> i (OuterEffects i m a))
    -> m a
controlI f = liftControlI f >>= resumeI (Pm :: Pm i)


------------------------------------------------------------------------------
-- | A particular application of 'liftControlI' that lifts control operations
-- from @(a -> i b) -> i b@ to @(a -> m b) -> m b@.
--
-- @
-- withMVar' :: 'MonadInnerControl' 'IO' m => 'Control.Concurrent.MVar.MVar' a -> (a -> m b) -> m b
-- withMVar' = 'liftOpI' '.' 'Control.Concurrent.MVar.withMVar'
-- @
--
-- The difference between 'liftOpI' and 'liftOp' is that 'liftOp' only lifts
-- from the monad directly beneath the top of the stack, while 'liftOpI'
-- can lift from /any/ monad anywhere in the stack (including @m@ itself).
--
-- If you know that you want to lift from the monad directly beneath the top
-- of the stack, it's often better to use 'liftOp' than 'liftOpI'. This
-- improves type inference because 'liftOp' is less polymorphic than
-- 'liftOpI'. Similarly, you might also consider using
-- 'Control.Monad.Lift.IO.liftIOOp' or 'Control.Monad.Lift.Base.liftBaseOp'
-- if you know that the monad from which you want to lift is 'IO' or the
-- <Control-Monad-Lift-Base.html base monad> of a transformer stack.
liftOpI :: MonadInnerControl i m
     => ((a -> i (OuterEffects i m b)) -> i (OuterEffects i m c))
     -> (a -> m b)
     -> m c
liftOpI f = \g -> controlI $ \peel -> f $ peel . g
{-# INLINABLE liftOpI #-}


------------------------------------------------------------------------------
-- | A particular application of 'liftControlI' that lifts control operations
-- from @i a -> i b@ to @m a -> m b@.
--
-- @
-- mask_' :: 'MonadInnerControl' 'IO' m => m a -> m a
-- mask_' = 'liftOpI_' 'Control.Exception.mask_'
-- @
--
-- The difference between 'liftOpI_' and 'liftOp_' is that 'liftOp_' only
-- lifts from the monad directly beneath the top of the stack, while
-- 'liftOpI_' can lift from /any/ monad anywhere in the stack (including @m@
-- itself).
--
-- If you know that you want to lift from the monad directly beneath the top
-- of the stack, it's often better to use 'liftOp_' than 'liftOpI_'. This
-- improves type inference because 'liftOp_' is less polymorphic than
-- 'liftOpI_'. Similarly, you might also consider using
-- 'Control.Monad.Lift.IO.liftIOOp_' or 'Control.Monad.Lift.Base.liftBaseOp_'
-- if you know that the monad from which you want to lift is 'IO' or the
-- <Control-Monad-Lift-Base.html base monad> of a transformer stack.
liftOpI_ :: MonadInnerControl i m
    => (i (OuterEffects i m a) -> i (OuterEffects i m b))
     -> m a
     -> m b
liftOpI_ f = \m -> controlI $ \peel -> f $ peel m
{-# INLINABLE liftOpI_ #-}


------------------------------------------------------------------------------
-- | A particular application of 'liftControlI' that lifts control operations
-- from @i () -> i a@ to @m () -> m a@.
--
-- @
-- forkIO' :: 'MonadInnerControl' 'IO' m => m () -> m 'Control.Concurrent.ThreadId'
-- forkIO' = 'liftDiscardI' 'Control.Concurrent.forkIO'
-- @
--
-- The difference between 'liftDiscardI' and 'liftDiscard' is that
-- 'liftDiscard' only lifts from the monad directly beneath the top of the
-- stack, while 'liftDiscardI' can lift from /any/ monad anywhere in the stack
-- (including @m@ itself).
--
-- If you know that you want to lift from the monad directly beneath the top
-- of the stack, it's often better to use 'liftDiscard' than 'liftDiscardI'.
-- This improves type inference because 'liftDiscard' is less polymorphic than
-- 'liftDiscardI'. Similarly, you might also consider using
-- 'Control.Monad.Lift.IO.liftIODiscard' or
-- 'Control.Monad.Lift.Base.liftBaseDiscard' if you know that the monad from
-- which you want to lift is 'IO' or the
-- <Documentation-Layers-Glossary.html#basemonad base monad> of a transformer stack.
--
-- Note: While the computation @m ()@ passed to the resulting operation has
-- access to the @'OuterEffects' i@ of @m@, it is run only for its side-effects
-- in @i@. Its side-effects in the outer layers of @m@ are discarded.
liftDiscardI :: MonadInnerControl i m => (i () -> i a) -> m () -> m a
liftDiscardI f = \m -> liftControlI $ \peel -> f $ liftM (const ()) $ peel m
{-# INLINABLE liftDiscardI #-}


{-$defaults

The changes to the behaviour of the @GeneralizedNewtypeDeriving@ extension
that come with the new <http://ghc.haskell.org/trac/ghc/wiki/Roles roles>
mechanism in GHC 7.8 (which fixes GHC bug
<http://ghc.haskell.org/trac/ghc/ticket/7148 #7148>) make it no longer
possible to automatically derive instances of 'MonadInnerControl' the way it
is for the other classes in the 'MonadInner' familiy.

Rather than lose this useful feature altogether, the operations
'defaultSuspendI', 'defaultResumeI', 'defaultCaptureI' and 'defaultExtractI'
are provided. These operations can be used to implement an instance
@'MonadInnerControl' i n@ for some inner monad @i@, if @n@ is isomorphic to an
@m@ for which there exists an instance @'MonadInnerControl' i m@ (e.g., if @n@
is a newtype wrapper around @m@). These operations use 'unsafeCoerce'
internally in their implementation, but in such a way that should be okay as
long as the given isomorphism is valid.

Here is an example that (safely) uses these operations:

@
{\-\# LANGUAGE FlexibleContexts #-\}
{\-\# LANGUAGE GeneralizedNewtypeDeriving #-\}
{\-\# LANGUAGE MultiParamTypeClasses #-\}

import "Control.Applicative"
import "Control.Monad.Lift"
import "Control.Monad.Trans.State.Strict"

newtype MyMonad a = MyMonad { runMyMonad :: 'StateT' ['Int'] 'IO' a }
  deriving
    ( 'Functor'
    , 'Control.Applicative.Applicative'
    , 'Monad'
    , 'Control.Monad.Lift.MonadInner' 'IO'
    , 'Control.Monad.Lift.MonadInnerInvariant' 'IO'
    , 'Control.Monad.Lift.MonadInnerFunctor' 'IO'
    )

instance 'MonadInnerControl' 'IO' MyMonad where
    'suspendI' = 'defaultSuspendI' runMyMonad
    'resumeI'  = 'defaultResumeI'  MyMonad
    'captureI' = 'defaultCaptureI' MyMonad
    'extractI' = 'defaultExtractI' MyMonad
@

If you rely on a derived instance of 'MonadInnerControl' on a @newtype@, and
you want your code to work with GHC 7.8 and above, you should use these
operations to manually define an instance (as above) rather than using the
@GeneralizedNewtypeDeriving@ extension.

-}


------------------------------------------------------------------------------
-- | Used when implementing a custom instance of @'MonadInnerControl' i@ for
-- some monad @n@.
--
-- @n@ must be isomorphic to a monad @m@ which is already an instance of
-- @'MonadInnerControl' i@.
--
-- 'defaultSuspendI' takes the @n -> m@ half of the isomorphism.
defaultSuspendI
    :: forall i m n a.
        ( MonadInnerControl i m
#if __GLASGOW_HASKELL__ >= 707
        , OuterResult i n ~ OuterResult_ i n
        , OuterState i n ~ OuterState_ i n
#endif
        )
    => (forall b. n b -> m b)
    -> n a
    -> OuterState i n
    -> i (OuterEffects i n a)
defaultSuspendI un m s = liftM (toOuterResult_ *** toOuterState_) $
    suspendI (un m) (fromOuterState_ s)


------------------------------------------------------------------------------
-- | Used when manually defining an instance of @'MonadInnerControl' i@ for
-- some monad @n@.
--
-- @n@ must be isomorphic to a monad @m@ which is already an instance of
-- @'MonadInnerControl' i@.
--
-- 'defaultResumeI' takes the @m -> n@ half of the isomorphism.
defaultResumeI
    :: forall proxy i n m a.
        ( MonadInnerControl i m
#if __GLASGOW_HASKELL__ >= 707
        , OuterResult i n ~ OuterResult_ i n
        , OuterState i n ~ OuterState_ i n
#endif
        )
    => (forall b. m b -> n b)
    -> proxy i
    -> OuterEffects i n a
    -> n a
defaultResumeI nu p = nu . resumeI p . (fromOuterResult_ *** fromOuterState_)


------------------------------------------------------------------------------
-- | Used when manually defining an instance of @'MonadInnerControl' i@ for
-- some monad @n@.
--
-- @n@ must be isomorphic to a monad @m@ which is already an instance of
-- @'MonadInnerControl' i@.
--
-- 'defaultCaptureI' takes the @m -> n@ half of the isomorphism.
defaultCaptureI :: forall proxy i m n.
        ( MonadInnerControl i m
#if __GLASGOW_HASKELL__ >= 707
        , OuterState i n ~ OuterState_ i n
#endif
        )
    => (forall b. m b -> n b)
    -> proxy i
    -> n (OuterState i n)
defaultCaptureI nu p = nu (liftM toOuterState_ (captureI p))


------------------------------------------------------------------------------
-- | Used when manually defining an instance of @'MonadInnerControl' i@ for
-- some monad @n@.
--
-- @n@ must be isomorphic to a monad @m@ which is already an instance of
-- @'MonadInnerControl' i@.
--
-- 'defaultExtractI' takes the @m -> n@ half of the isomorphism.
defaultExtractI
    :: forall proxy proxy' i m n a.
        ( MonadInnerControl i m
#if __GLASGOW_HASKELL__ >= 707
        , OuterResult i n ~ OuterResult_ i n
#endif
        )
    => (forall b. m b -> n b)
    -> proxy i
    -> proxy' n
    -> OuterResult i n a
    -> Maybe a
defaultExtractI _ p _ r = extractI p (Pm :: Pm m) (fromOuterResult_ r)


------------------------------------------------------------------------------
-- | The constraint @'MonadInnerInvariant' i m@ holds when @i@ is an inner
-- monad of @m@ such that it is possible to lift monad automorphisms of @i@ to
-- monad endomorphisms of @m@ using 'hoistisoI'. In other words,
-- @'MonadInnerInvariant' i m@ implies the existence of an invariant functor
-- in the category of monads from @i@ to @m@.
class MonadInner i m => MonadInnerInvariant i m where
    -- | Lift an automorphism of @i@ to an endomorphism of @m@.
    --
    -- The following laws hold for valid instances of 'MonadInnerInvariant':
    --
    --     [Identity] @'hoistisoI' 'id' 'id' ≡ 'id'@
    --
    --     [Composition]
    --         @'hoistisoI' f g '.' 'hoistisoI' f' g' ≡
    --             'hoistisoI' (f '.' f') (g' '.' g)@
    --
    -- Note: The endomorphism produced by @'hoistisoI' f g@ is only valid if
    -- @f@ and @g@ form a valid isomorphism, i.e., @f '.' g ≡ 'id'@ and
    -- @g '.' f ≡ 'id'@.
    --
    -- There are two main differences between 'hoistisoI' and 'hoistiso'. The
    -- first is that 'hoistiso' only lifts from the monad directly beneath the
    -- top of the stack, while 'hoistisoI' can lift from /any/ monad anywhere
    -- in the stack (including @m@ itself). The second is that 'hoistisoI' can
    -- only accept an automorphism (producing an endomorphism), while
    -- 'hoistiso' can accept any isomorphism, producing a homomorphism. In
    -- other words, the morphism passed to 'hoistiso' can be used to change
    -- the type of the inner monad, but this is not possible with 'hoistisoI'.
    --
    -- If you know that you want to lift from the monad directly beneath the
    -- top of the stack, it's often better to use 'hoistiso' than 'hoistisoI'.
    -- This improves type inference because 'hoistiso' is less polymorphic
    -- than 'hoistisoI'.  Similarly, you might also consider using
    -- 'Control.Monad.Lift.IO.hoistisoIO' or
    -- 'Control.Monad.Lift.Base.hoistisoBase' if you know that the monad from
    -- which you want to lift is 'IO' or the
    -- <Control-Monad-Lift-Base.html base monad> of a transformer stack.
    hoistisoI
        :: (forall b. i b -> i b)
        -> (forall b. i b -> i b)
        -> m a
        -> m a


------------------------------------------------------------------------------
instance MonadInner m m => MonadInnerInvariant m m where
    hoistisoI f _ = f


------------------------------------------------------------------------------
instance (Monad m, MonadInner (t m) (t m)) => MonadInnerInvariant (t m) (t m)
  where
    hoistisoI f _ = f


------------------------------------------------------------------------------
instance (MInvariant t, Monad m, MonadInnerInvariant i m, MonadInner i (t m))
    => MonadInnerInvariant i (t m)
  where
    hoistisoI f g = hoistiso (hoistisoI f g) (hoistisoI g f)
    {-# INLINABLE hoistisoI #-}


------------------------------------------------------------------------------
-- | The constraint @'MonadInnerFunctor' i m@ holds when @i@ is an inner monad
-- of @m@ such that it is possible to lift monad morphisms of @i@ to monad
-- morphisms of @m@ using 'hoistI'. 'hoistI' is more powerful than
-- 'hoistisoI' because 'hoistI' can lift morphisms which do not have an
-- inverse, while 'hoistisoI' can only lift isomorphisms. In other words,
-- @'MonadInnerFunctor' i m@ implies the existence of a functor in the
-- category of monads from @i@ to @m@.
class MonadInnerInvariant i m => MonadInnerFunctor i m where
    -- | Lift an endomorphism of @i@ to an endomorphism of @m@.
    --
    -- The following laws hold for valid instances of 'MonadInnerFunctor':
    --
    --     [Identity] @'hoistI' 'id' ≡ 'id'@
    --
    --     [Composition] @'hoistI' f '.' 'hoistI' g ≡ 'hoistI' (f '.' g)@
    --
    -- There are two main differences between 'hoistI' and 'hoist'. The first
    -- is that 'hoist' only lifts from the monad directly beneath the top of
    -- the stack, while 'hoistI' can lift from /any/ monad anywhere in the
    -- stack (including @m@ itself). The second is that 'hoistI' can only
    -- accept an endomorphism, while 'hoist' can accept any homomorphism. In
    -- other words, the morphism passed to 'hoist' can be used to change the
    -- type of the inner monad, but this is not possible with 'hoistI'.
    --
    -- If you know that you want to lift from the monad directly beneath the
    -- top of the stack, it's often better to use 'hoist' than 'hoistI'. This
    -- improves type inference because 'hoist' is less polymorphic than
    -- 'hoistI'. Similarly, you might also consider using
    -- 'Control.Monad.Lift.IO.hoistIO' or 'Control.Monad.Lift.Base.hoistBase'
    -- if you know that the monad from which you want to lift is 'IO' or the
    -- <Control-Monad-Lift-Base.html base monad> of a transformer stack.
    hoistI :: (forall b. i b -> i b) -> m a -> m a


------------------------------------------------------------------------------
instance MonadInnerInvariant m m => MonadInnerFunctor m m where
    hoistI f = f


------------------------------------------------------------------------------
instance (Monad m, MonadInnerInvariant (t m) (t m)) =>
    MonadInnerFunctor (t m) (t m)
  where
    hoistI f = f


------------------------------------------------------------------------------
instance
    ( MInvariant t
    , MFunctor t
    , Monad m
    , MonadInnerInvariant i (t m)
    , MonadInnerFunctor i m
    )
  =>
    MonadInnerFunctor i (t m)
  where
    hoistI f = hoist (hoistI f)
    {-# INLINABLE hoistI #-}
