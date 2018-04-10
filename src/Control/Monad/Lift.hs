{-# LANGUAGE CPP #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -fno-warn-warnings-deprecations #-}

#ifdef LANGUAGE_DefaultSignatures
{-# LANGUAGE DefaultSignatures #-}
#endif

#ifdef LANGUAGE_SafeHaskell
{-# LANGUAGE Trustworthy #-}
#endif

#include "docmacros.h"
#include "newtypec.h"
#include "overlap.h"

{-|

This module houses the core machinery of the H(layers) package. It exports
everything you need to implement G(monadtransformer, monad transformers)
compatible with H(layers)' modular
G(monadinterface,monad interfaces), and everything that you need to lift
G(computation,computations), G(controloperation,operations) and
G(morphism,morphisms) through arbitrarily complicated
G(monadtransformerstack,stacks) of G(monadtransformer,monad transformers).

The H(layers) machinery is built upon two twin families of interfaces. The
'MonadTrans' family of interfaces provides operations for lifting
G(computation,computations), G(controloperation,operations) and
G(morphism,morphisms) up exactly one level in the
G(monadtransformerstack,transformer stack). That is, it lifts
G(computation,computations) from the monad @m@ to the monad @t m@, where @t@
is a G(monadtransformer,monad transformer). The 'MonadInner' family of
interfaces provides operations for lifting
G(computation,computations), G(controloperation,operations) and
G(morphism,morphisms) from /any/ level of the
G(monadtransformerstack,transformer stack)
to the top of the G(monadtransformerstack,transformer stack). That is, it
lifts G(computation,computations) from from the monad @i@ to the monad @m@,
where @i@ an G(innermonad,inner monad) of @m@. Each of the 'MonadInner'
interfaces is defined recursively in terms of its 'MonadTrans' counterpart.

The 'MonadTrans' family of interfaces is mainly used by libraries that
implement G(monadtransformer,monad transformers) and
G(monadinterfaces, monad interfaces), while the 'MonadInner'
family is mainly used by applications make use of
(G(monadtransformerstack,stacks of)) these
G(monadtransformerstack,transformers).

-}

module Control.Monad.Lift
    (
    -- * The @MonadTrans@ family
    -- $transfamily

    -- ** Lifting computations
      MonadTrans (lift)

    -- ** Lifting control operations
    , MonadTransControl (suspend, resume, capture, extract)
    , LayerEffects, LayerResult, LayerState
    , liftControl, control, liftOp, liftOp_, liftDiscard

    -- ** Lifting morphisms
    , MInvariant (hoistiso), MFunctor (hoist)

    -- * The @MonadInner@ family
    -- $innerfamily

    -- ** Lifting computations
    , MonadInner (liftI)

    -- ** Lifting control operations
    , MonadInnerControl (suspendI, resumeI, captureI, extractI, mapI)
    , OuterEffects, OuterResult, OuterState
    , liftControlI, controlI, liftOpI, liftOpI_, liftDiscardI

    -- ** Lifting morphisms
    , MonadInnerInvariant (hoistisoI), MonadInnerFunctor (hoistI)

    -- * Defaults
    -- $defaults
    , Iso1, Codomain1, from1, to1

    -- ** For the @MonadTrans@ family
    -- *** Computations
    , DefaultMonadTrans, defaultLift
    , DefaultMonadTrans2, defaultLift2
    , DefaultMonadTrans3, defaultLift3
    , DefaultMonadTrans4, defaultLift4

    -- *** Control operations
    , DefaultMonadTransControl, DefaultLayerResult, DefaultLayerState
    , defaultSuspend, defaultResume, defaultCapture, defaultExtract
    , DefaultMonadTransControl2, DefaultLayerResult2, DefaultLayerState2
    , defaultSuspend2, defaultResume2, defaultCapture2, defaultExtract2
    , DefaultMonadTransControl3, DefaultLayerResult3, DefaultLayerState3
    , defaultSuspend3, defaultResume3, defaultCapture3, defaultExtract3
    , DefaultMonadTransControl4, DefaultLayerResult4, DefaultLayerState4
    , defaultSuspend4, defaultResume4, defaultCapture4, defaultExtract4

    -- *** Morphisms
    , DefaultMInvariant, defaultHoistiso, DefaultMFunctor, defaultHoist
    , DefaultMInvariant2, defaultHoistiso2, DefaultMFunctor2, defaultHoist2
    , DefaultMInvariant3, defaultHoistiso3, DefaultMFunctor3, defaultHoist3
    , DefaultMInvariant4, defaultHoistiso4, DefaultMFunctor4, defaultHoist4

    -- ** For the @MonadInner@ family
    -- *** Computations
    , DefaultMonadInner, defaultLiftI

    -- *** Control operations
    , DefaultMonadInnerControl
    , defaultSuspendI, defaultResumeI, defaultCaptureI, defaultExtractI
    , defaultMapI

    -- *** Morphisms
    , DefaultMonadInnerInvariant, defaultHoistisoI
    , DefaultMonadInnerFunctor, defaultHoistI
    )
where

-- base ----------------------------------------------------------------------
import           Control.Arrow ((***), first)
import           Control.Monad (liftM, liftM2, liftM3, liftM4)
import           Data.Functor.Identity (Identity (Identity))
#if !MIN_VERSION_base(4, 8, 0)
import           Data.Monoid (Monoid, mempty)
#endif


-- layers --------------------------------------------------------------------
import           Control.Monad.Lift.Internal
                     ( LayerEffects, LayerResult, LayerState, coercePeel
                     , ComposeResult2 (ComposeResult2)
                     , ComposeResult3 (ComposeResult3)
                     , ComposeResult4 (ComposeResult4)
                     , OuterEffects, OuterResult, OuterState, coercePeelI
                     , ComposeResult (ComposeResult), fromR, toR, fromS, toS
                     , Iso1, Codomain1, from1, to1
                     )


-- mmorph --------------------------------------------------------------------
import           Control.Monad.Morph (MFunctor (hoist))
#if MIN_VERSION_mmorph(1, 0, 1)
import           Control.Monad.Trans.Compose (ComposeT)
#endif


-- transformers --------------------------------------------------------------
import           Control.Monad.Trans.Class (MonadTrans (lift))
import           Control.Monad.Trans.Cont (ContT (ContT))
#if !MIN_VERSION_transformers(0, 6, 0)
import           Control.Monad.Trans.Error (Error, ErrorT (ErrorT))
#endif
#if MIN_VERSION_transformers(0, 4, 0)
import           Control.Monad.Trans.Except (ExceptT (ExceptT))
#endif
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


{-$transfamily

The 'MonadTrans' family of interfaces consists of:

  * 'MonadTrans', re-exported from the H(transformers) package.
  * 'MonadTransControl', defined by H(layers).
  * 'MInvariant', defined by H(layers).
  * 'MFunctor', re-exported from the H(mmorph) package.

Ideally, all of these classes would be re-exports from more popular packages,
because then it would be possible to write monad transformers compatible with
H(layers)' monad interfaces without ever incurring a dependency on
H(layers). As 'MonadTrans' and 'MFunctor' come from H(transformers) and
H(mmorph) respectively, we're already half way there.
H(mmorph) is also the most sensible home for 'MInvariant', and
<https://github.com/Gabriel439/Haskell-MMorph-Library/pull/1 hopefully> it
will get moved there soon. 'MonadTransControl' is more complicated: there is a
TT(monad-control,Control-Monad-Trans-Control,MonadTransControl,very similar class)
(the design of which I originally copied) defined in the H(monad-control)
package, which is a relatively popular package. However, H(layers)' version
has a few important differences that stop it from being able to use
H(monad-control)'s
T(monad-control,Control-Monad-Trans-Control,MonadTransControl) without losing
some of its features (notably the 'Monad.Try.MonadTry'
G(monadinterface,interface)). However, it is conceivable that these changes
could be merged into H(monad-control) some day, in which case I would be happy
to make H(layers) depend on H(monad-control) and make 'MonadTransControl' a
re-export.

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
-- | The constraint @'MonadTransControl' t@ holds if @t@ is a
-- G(monadtransformer,monad transformer) through which
-- G(controloperation,control operations) can be lifted. There are a
-- variety of operations for doing so, depending on the exact type of the
-- G(controloperation,control operation) in question, including
-- 'liftControl', 'control', 'liftOp', 'liftOp_' and 'liftDiscard'. These are
-- all built on top of the more primitive 'capture', 'suspend' and 'resume'
-- operations.
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
-- @peel :: 'Monad' m => t m a -> m a@
--
-- Unfortunately, the only monad transformers that could provide such an
-- operation are trivial (i.e., isomorphic to 'IdentityT'). To see why other
-- monad transformers cannot provide such an operation, let's consider the
-- more complicated 'RWST' monad transformer. It is defined like this:
--
-- @newtype 'RWST' r w s m a = 'RWST' { 'Control.Monad.Trans.RWS.Strict.runRWST' :: r -> s -> m (a, s, w) }@
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
-- @type 'LayerState' 'IdentityT' = ()
--type 'LayerState' ('RWST' r w s) = (r, s)@
--
-- Now we can have an operation with a single type that fits both 'RWST' and
-- 'IdentityT':
--
-- @peel :: 'Monad' m => t m a -> 'LayerState' t -> m a@
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
-- @newtype 'MaybeT' m a = { 'Control.Monad.Trans.Maybe.runMaybeT' :: m ('Maybe' a) }@
-- 
-- If the value inside the @m@ computation wrapped by 'MaybeT' is a 'Nothing',
-- we can't get an @a@ out of it. The closest to the proposed type for 'peel'
-- above that we could get for 'MaybeT' would be:
--
-- @peel :: 'MaybeT' m a -> 'LayerState' 'MaybeT' -> m ('Maybe' a)@
--
-- Similarly, for @'ExceptT' e@, the closest we could get would be:
--
-- @peel :: 'ExceptT' e m a -> 'LayerState' ('ExceptT' e) -> m ('Either' e a)@
--
-- Again, we can use an associated type synonym to make all of these
-- operations fit a single pattern. We call this one 'LayerResult'. Here are
-- some of its instances:
--
-- @type 'LayerResult' 'IdentityT' = 'Identity'
--type 'LayerResult' 'MaybeT' = 'Maybe'
--type 'LayerResult' ('ExceptT' e) = 'Either' e
--type 'LayerResult' ('RWST' r w s) = (,) w@
--
-- How exactly is it decided what @'LayerResult' t@ should be for a particular
-- monad transformer? There are several things to consider. Let's say we have
-- a monad transformer @t@ that we want to make an instance of
-- 'MonadTransControl'. Let's also assume that its 'LayerState' is @()@ (like
-- 'ExceptT', 'IdentityT' and 'MaybeT'), because this is easier to explain
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
-- @type 'LayerResult' 'IdentityT' a = a@
--
-- But we can't, because @'LayerResult' t@ must be a type constructor
-- @* -> *@, not a type @*@. The reason we do this is because we want the
-- compiler to able to infer @a ~ b@ if it knows that @'LayerResult' t a ~
-- 'LayerResult' t b@. It couldn't do this if we allowed definitions like the
-- above because
-- HWT(GHC/Type_families#Injectivity.2C_type_inference.2C_and_ambiguity,type families are not injective).
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
-- @'suspend' :: 'Monad' m => t m a -> 'LayerState' t -> m ('LayerEffects' t a)@
--
-- @type 'LayerEffects' t a = ('LayerResult' t a, 'LayerState' t)@
--
-- (The purpose of the 'LayerEffects' type synonym is twofold: it makes the
-- type signatures of 'suspend' and other operations a little bit less scary,
-- and it also communicates that the combination of a 'LayerResult' and a
-- 'LayerState' together reify the side-effects of a monad transformer.)
--
-- There are two important operations in the 'MonadTransControl' that we have
-- only alluded to so far: 'capture' and 'resume'.
--
-- @'capture' :: ('MonadTransControl' t, 'Monad' m) => t m ('LayerState' t)
--'resume' :: ('MonadTransControl' t, 'Monad' m) => 'LayerEffects' t a -> t m a@
--
-- 'capture' captures the current @'LayerState' t@ for the monad @t m@. This
-- is where the 'LayerState' that 'suspend' takes as its argument comes from.
-- 'resume' is the inverse of 'suspend': it takes the suspended side-effects
-- of a monad transformer @t@ reified by a @'LayerEffects' t a@ value, and
-- returns a returns a reconstructed computation of type @t m a@ with those
-- side-effects.
--
-- Taken together, we can use these operations to define @f'@, a lifted
-- version of the @f@ operation described above.
--
-- @f' :: ('MonadTransControl' t, 'Monad' (t m), 'Monad' m) => t m a -> t m b
--f' t = 'capture' '>>=' 'lift' '.' f '.' 'suspend' t '>>=' 'resume'@
--
-- The full instance for 'RWST' is given below:
--
-- @instance 'Monoid' w => 'MonadTransControl' ('RWST' r w s) where
--    type 'LayerResult' ('RWST' r w s) = (,) w
--    type 'LayerState' ('RWST' r w s) = (r, s)
--    'suspend' ('RWST' m) (r, s) = 'liftM' (\\(a, s', w) -> ((w, a), (r, s'))) (m r s)
--    'resume' ((w, a), (_, s)) = 'RWST' '$' \\_ _ -> 'return' (a, s, w)
--    'capture' = 'RWST' '$' \\r s -> 'return' ((r, s), s, 'mempty')
--    'extract' _ (_, a) = 'Right' a@
class (MonadTrans t, Functor (LayerResult t)) => MonadTransControl t where
    -- | Given a G(computation,computation) @m@ of type @t m a@ and the
    -- current G(layerstate,layer state) of the @t@ G(monadlayer,layer),
    -- 'suspend' suspends the G(sideeffect,side-effects) of @m@ which come
    -- from this G(monadlayer,layer) by returning a new
    -- G(computation,computation) in the monad @m@ that returns the
    -- G(layereffect,reification) of these G(sideeffects,side-effects) in
    -- a @'LayerEffects' t a@ value. This gives a version of @m@ which
    -- can be passed to G(controloperation,control operations) in the monad
    -- @m@.
    --
    -- The suspended G(sideeffect,side-effects) of the @t@
    -- G(monadlayer,layer) can later be recovered by 'resume'. This is
    -- expressed in the following law:
    --
    -- [Preservation] @'capture' '>>=' 'lift' '.' 'suspend' t '>>=' 'resume' ≡ t@
    suspend :: Monad m => t m a -> LayerState t -> m (LayerEffects t a)

    -- | Reconstructs a G(computation,computation) @t m a@ with the same
    -- G(sideeffect,side-effects) in the @t@ G(monadlayer,layer) as those
    -- G(layereffect,reified) by the given @'LayerEffects' t a@ value.
    --
    -- Instances should satisfy the following law:
    --
    -- [Preservation] @'capture' '>>=' 'lift' '.' 'suspend' t '>>=' 'resume' ≡ t@
    resume :: Monad m => LayerEffects t a -> t m a

    -- | Captures the current G(layerstate,layer state) of the @t@
    -- G(monadlayer,layer) of the monad @t m@.
    --
    -- Instances should satisfy the following law:
    --
    -- [Preservation] @'capture' '>>=' 'lift' '.' 'suspend' t '>>=' 'resume' ≡ t@
    capture :: Monad m => t m (LayerState t)

    -- | 'extract' inspects a G(layerresult,layer result), given by
    -- suspending the G(sideeffect,side-effects) in the @t@
    -- G(monadlayer,layer) of a G(computation,computation) of type @t m a@,
    -- and tries to \"extract\" an @a@ value from it. If it can't, this
    -- implies that @t@ G(shortcircuit,short-circuited).
    --
    -- Instances should satisfy the following laws:
    --
    -- [Preserve-Unit]
    --     @extractResult ('return' a) ≡ 'return' ('Just' a)@
    -- [Implies-Zero]
    --     @(extractResult m
    --         ≡ 'liftM' ('const' 'Nothing') m) ⇒ (∀f. m '>>=' f ≡ m)@
    --
    -- The @extractResult@ operation in terms of which these laws are defined
    -- is given by:
    --
    -- @extractResult :: forall t m a. ('MonadTransControl' t, 'Monad' (t m), 'Monad' m)
    --    => t m a
    --    -> t m ('Maybe' a)
    --extractResult t = do
    --    state <- 'capture'
    --    'lift' '$' do
    --        (result, _) <- 'suspend' t state
    --        'return' '$' 'extract' ('Data.Proxy.Proxy' :: 'Data.Proxy.Proxy' t) result@
    extract :: proxy t -> LayerResult t a -> Either (LayerResult t b) a


#if !MIN_VERSION_transformers(0, 6, 0)
------------------------------------------------------------------------------
instance Error e => MonadTransControl (ErrorT e) where
    suspend (ErrorT m) _ = liftM (\a -> (a, ())) m
    resume (a, _) = ErrorT $ return a
    capture = return ()
    extract _ = either (Left . Left) Right


#endif
#if MIN_VERSION_transformers(0, 4, 0)
------------------------------------------------------------------------------
instance MonadTransControl (ExceptT e) where
    suspend (ExceptT m) _ = liftM (\a -> (a, ())) m
    resume (a, _) = ExceptT $ return a
    capture = return ()
    extract _ = either (Left . Left) Right


#endif
------------------------------------------------------------------------------
instance MonadTransControl IdentityT where
    suspend (IdentityT m) _ = liftM (\a -> (Identity a, ())) m
    resume (Identity a, _) = IdentityT $ return a
    capture = return ()
    extract _ (Identity a) = Right a


------------------------------------------------------------------------------
instance MonadTransControl ListT where
    suspend (ListT m) _ = liftM (\a -> (a, ())) m
    resume (a, _) = ListT $ return a
    capture = return ()
    extract _ = foldr (const . Right) (Left [])


------------------------------------------------------------------------------
instance MonadTransControl MaybeT where
    suspend (MaybeT m) _ = liftM (\a -> (a, ())) m
    resume (a, _) = MaybeT $ return a
    capture = return ()
    extract _ = maybe (Left Nothing) Right


------------------------------------------------------------------------------
instance MonadTransControl (ReaderT r) where
    suspend (ReaderT m) r = liftM (\a -> (Identity a, r)) (m r)
    resume (Identity a, _) = ReaderT $ \_ -> return a
    capture = ReaderT return
    extract _ (Identity a) = Right a


------------------------------------------------------------------------------
instance MonadTransControl (StateT s) where
    suspend (StateT m) s = liftM (first Identity) (m s)
    resume (Identity a, s) = StateT $ \_ -> return (a, s)
    capture = StateT $ \s -> return (s, s)
    extract _ (Identity a) = Right a


------------------------------------------------------------------------------
instance MonadTransControl (L.StateT s) where
    suspend (L.StateT m) s = liftM (first Identity) (m s)
    resume (Identity a, s) = L.StateT $ \_ -> return (a, s)
    capture = L.StateT $ \s -> return (s, s)
    extract _ (Identity a) = Right a


------------------------------------------------------------------------------
instance Monoid w => MonadTransControl (RWST r w s) where
    suspend (RWST m) (r, s) = liftM (\(a, s', w) -> ((w, a), (r, s'))) (m r s)
    resume ((w, a), (_, s)) = RWST $ \_ _ -> return (a, s, w)
    capture = RWST $ \r s -> return ((r, s), s, mempty)
    extract _ (_, a) = Right a


------------------------------------------------------------------------------
instance Monoid w => MonadTransControl (L.RWST r w s) where
    suspend (L.RWST m) (r, s) = liftM (\(a, s', w) -> ((w, a), (r, s'))) (m r s)
    resume ((w, a), (_, s)) = L.RWST $ \_ _ -> return (a, s, w)
    capture = L.RWST $ \r s -> return ((r, s), s, mempty)
    extract _ (_, a) = Right a


------------------------------------------------------------------------------
instance Monoid w => MonadTransControl (WriterT w) where
    suspend (WriterT m) _ = liftM (\(a, w) -> ((w, a), ())) m
    resume ((w, a), _) = WriterT $ return (a, w)
    capture = return ()
    extract _ (_, a) = Right a


------------------------------------------------------------------------------
instance Monoid w => MonadTransControl (L.WriterT w) where
    suspend (L.WriterT m) _ = liftM (\(a, w) -> ((w, a), ())) m
    resume ((w, a), _) = L.WriterT $ return (a, w)
    capture = return ()
    extract _ (_, a) = Right a


------------------------------------------------------------------------------
-- | 'liftControl' is a composition of 'capture', 'suspend' and 'lift'
-- provided for convenience (and compability with H(monad-control)).
--
-- It takes a continuation, to which it passes an operation which can be used
-- to \"lower\" a G(computation,computation) in the monad @t m@ to a
-- G(computation,computation) in the monad @m@, which returns the
-- G(layereffect,reified) G(sideeffect,side-effects) of the @t@
-- G(monadlayer,layer) of the original computation.
liftControl :: forall t m a. (MonadTransControl t, Monad (t m), Monad m)
    => ((forall b. t m b -> m (LayerEffects t b)) -> m a)
    -> t m a
liftControl f = capture >>= \s -> lift $ f (flip suspend s)
{-# INLINE liftControl #-}


------------------------------------------------------------------------------
-- | A version of 'liftControl' that automatically resumes the
-- G(layereffect,suspended side-effects) returned from the continuation.
--
-- @catch' :: ('Control.Exception.Exception' e, 'MonadTransControl' t, 'Monad' (t 'IO')) => t m b -> (e -> t m b) -> t m b
--catch' m h = 'control' (\\peel -> 'Control.Exception.catch' (peel m) (peel '.' h))@
control :: (MonadTransControl t, Monad (t m), Monad m)
    => ((forall b. t m b -> m (LayerEffects t b)) -> m (LayerEffects t a))
    -> t m a
control f = liftControl (\peel -> f (coercePeel peel)) >>= resume
{-# INLINE control #-}


------------------------------------------------------------------------------
-- | A particular application of 'liftControl' that lifts
-- G(controloperation,control operations) from @(a -> m b) -> m c@ to
-- @(a -> t m b) -> t m c@.
--
-- @withMVar' :: ('MonadTransControl' t, 'Monad' (t 'IO')) => 'Control.Concurrent.MVar.MVar' a -> (a -> t 'IO' b) -> t 'IO' b
--withMVar' = 'liftOp' '.' 'Control.Concurrent.MVar.withMVar'@
liftOp :: (MonadTransControl t, Monad (t m), Monad m)
    => ((a -> m (LayerEffects t b)) -> m (LayerEffects t c))
    -> (a -> t m b)
    -> t m c
liftOp f = \g -> control (\peel -> f $ peel . g)
{-# INLINE liftOp #-}


------------------------------------------------------------------------------
-- | A particular application of 'liftControl' that lifts
-- G(controloperation,control operations) from @m a -> m b@ to
-- @t m a -> t m b@.
--
-- @mask_' :: ('MonadTransControl' t, 'Monad' (t 'IO')) => t 'IO' a -> t 'IO' a
--mask_' = 'liftOp_' 'Control.Exception.mask_'@
liftOp_ :: (MonadTransControl t, Monad (t m), Monad m)
    => (m (LayerEffects t a) -> m (LayerEffects t b))
    -> t m a
    -> t m b
liftOp_ f = \m -> control (\peel -> f $ peel m)
{-# INLINE liftOp_ #-}


------------------------------------------------------------------------------
-- | A particular application of 'liftControl' that lifts
-- G(controloperation,control operations) from @m () -> m a@ to
-- @t m () -> t m a@.
--
-- @forkIO' :: ('MonadTransControl' t, Monad (t 'IO')) => t m () -> t m 'Control.Concurrent.ThreadId'
--forkIO' = 'liftDiscard' 'Control.Concurrent.forkIO'@
--
-- Note: While the G(computation,computation) @t m ()@ passed to the
-- resulting operation can access to the G(layerstate,layer state) of and
-- produce G(sideeffect,side-effects) in the @t@ G(monadlayer,layer), it is
-- run only for its G(sideeffect,side-effects) in @m@. The
-- G(sideeffect,side-effects) in the @t@ G(monadlayer,layer) are discarded.
liftDiscard :: (MonadTransControl t, Monad (t m), Monad m)
    => (m () -> m a)
    -> t m ()
    -> t m a
liftDiscard f m = liftControl $ \peel -> f $ liftM (const ()) $ peel m
{-# INLINE liftDiscard #-}


------------------------------------------------------------------------------
-- | An G(morphism,invariant functor in the category of monads), using
-- 'hoistiso' as the analog of V(invariant,Data-Functor-Invariant,invmap).
class MInvariant t where
    -- | Lift a G(morphism,monad isomorphism) between @m@ and @n@ into a
    -- G(morphism,monad morphism) from @t m@ to @t n@.
    --
    -- The following laws hold for valid instances of 'MInvariant':
    --
    -- [Identity] @'hoistiso' 'id' 'id' ≡ 'id'@
    -- [Composition]
    --     @'hoistiso' f g '.' 'hoistiso' f' g' ≡
    --         'hoistiso' (f '.' f') (g' '.' g)@
    --
    -- Note: The G(morphism,homomorphism) produced by @'hoistiso' f g@ is
    -- only valid if @f@ and @g@ form a valid G(morphism,isomorphism), i.e.,
    -- @f '.' g ≡ 'id'@ and @g '.' f ≡ 'id'@.
    hoistiso :: (Monad m, Monad n)
        => (forall b. m b -> n b)
        -> (forall b. n b -> m b)
        -> t m a
        -> t n a

#ifdef LANGUAGE_DefaultSignatures
    default hoistiso :: (MFunctor t, Monad m)
        => (forall b. m b -> n b)
        -> (forall b. n b -> m b)
        -> t m a
        -> t n a
    hoistiso f _ = hoist f

#endif

------------------------------------------------------------------------------
instance MInvariant (ContT r) where
    hoistiso f g (ContT m) = ContT $ f . m . (g .)


#if !MIN_VERSION_transformers(0, 6, 0)
------------------------------------------------------------------------------
instance MInvariant (ErrorT e) where
    hoistiso f _ = hoist f


#endif
#if MIN_VERSION_transformers(0, 4, 0)
instance MInvariant (ExceptT e) where
    hoistiso f _ = \(ExceptT m) -> ExceptT $ f m


#endif


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
-- G(innermonad,inner monad) of @m@ such that it is possible to lift
-- G(computation,computations) from @i@ into @m@ using 'liftI'.
class (Monad i, Monad m) => MonadInner i m where
    -- | 'liftI' takes a computation from an G(innermonad,inner monad) @i@ of
    -- @m@ and lifts it into @m@.
    --
    -- The following laws hold for valid instances of 'MonadInner':
    --
    --     [Identity] @'liftI' '.' 'return' ≡ 'return'@
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

#ifdef LANGUAGE_DefaultSignatures
-- DefaultSignatures doesn't work with multi-parameter type classes in GHC 7.2
#if __GLASGOW_HASKELL__ >= 704
    default liftI :: DefaultMonadInner i m => i a -> m a
    liftI = defaultLiftI
#endif
#endif


------------------------------------------------------------------------------
instance Monad m => MonadInner m m where
    liftI = id


------------------------------------------------------------------------------
instance __OVERLAPPABLE__
    (Monad i, Monad (t m), MonadInner i m, MonadTrans t, tm ~ t m)
  =>
    MonadInner i tm
  where
    liftI = lift . liftI


#if MIN_VERSION_mmorph(1, 0, 1)
------------------------------------------------------------------------------
instance (Monad (f (g m)), DefaultMonadInner (f (g m)) (ComposeT f g m)) =>
    MonadInner (f (g m)) (ComposeT f g m)
  where
    liftI = defaultLiftI


#endif
------------------------------------------------------------------------------
-- | The constraint @'MonadInnerControl' i m@ holds when @i@ is an
-- G(innermonad,inner monad) of @m@ such that it is possible to lift
-- G(controloperation,control operations) from @i@ to @m@. There are a
-- variety of operations for doing so, depending on the exact type of the
-- G(controloperation,control operation) in question, including
-- 'liftControlI', 'controlI', 'liftOpI', 'liftOpI_' and 'liftDiscardI'. These
-- are all built on top of the more primitive 'captureI', 'suspendI' and
-- 'resumeI' operations.
class
    ( MonadInner i m
#ifdef ClosedTypeFamilies
    , Functor (OuterResult i m)
#endif
    )
  =>
    MonadInnerControl i m
  where
    -- | Given a G(computation,computation) @m@ of type @m a@ and the current
    -- G(layerstate,layer state) of the G(outerlayer,outer layers) around
    -- @i@ of @m@, 'suspendI' suspends the
    -- G(sideeffect,side-effects) of @m@ which come from these
    -- G(monadlayer,layers) by returning a new G(computation,computation) in
    -- the monad @i@ that returns the G(layereffect,reification) of these
    -- G(sideeffects,side-effects) in an @'OuterEffects' i m a@ value. This
    -- gives a version of @m@ which can be passed to
    -- G(controloperation,control operations) in the monad @i@.
    --
    -- The suspended G(sideeffect,side-effects) of the
    -- G(outerlayer,outer layers) around @i@ of @m@ can later be recovered
    -- by 'resumeI'. This is expressed in the following law:
    --
    -- [Preservation] @'captureI' ('Data.Proxy.Proxy' :: 'Data.Proxy.Proxy' i) '>>=' 'liftI' '.' 'suspendI' t '>>=' 'resumeI' ('Data.Proxy.Proxy' :: 'Data.Proxy.Proxy' i) ≡ t@
    suspendI :: m a -> OuterState i m -> i (OuterEffects i m a)

    -- | Reconstructs a G(computation,computation) @m a@ with the same
    -- G(sideeffect,side-effects) in the G(outerlayer,outer layers) around
    -- @i@ of @m@ as those G(layereffect,reified) by the given
    -- @'OuterEffects' i m a@ value.
    --
    -- Instances should satisfy the following law:
    --
    -- [Preservation] @'captureI' ('Data.Proxy.Proxy' :: 'Data.Proxy.Proxy' i) '>>=' 'liftI' '.' 'suspendI' t '>>=' 'resumeI' ('Data.Proxy.Proxy' :: 'Data.Proxy.Proxy' i) ≡ t@
    resumeI :: proxy i -> OuterEffects i m a -> m a

    -- | Captures the current G(layerstate,layer state) of the
    -- G(monadlayer,outer layers) around @i@ of the monad @m@.
    --
    -- Instances should satisfy the following law:
    --
    -- [Preservation] @'captureI' ('Data.Proxy.Proxy' :: 'Data.Proxy.Proxy' i) '>>=' 'liftI' '.' 'suspendI' t '>>=' 'resumeI' ('Data.Proxy.Proxy' :: 'Data.Proxy.Proxy' i) ≡ t@
    captureI :: proxy i -> m (OuterState i m)

    -- | 'extractI' inspects a G(layerresult,layer result), given by
    -- suspending the G(sideeffect,side-effects) in the
    -- G(outerlayer,outer layers) around @i@ of a
    -- G(computation,computation) of type @m a@, and tries to
    -- \"extract\" an @a@ value from it. If it can't, this implies that
    -- one the G(outerlayer,outer layers) G(shortcircuit,short-circuited).
    --
    -- Instances should satisfy the following laws:
    --
    -- [Preserve-Unit]
    --     @extractResultI ('Data.Proxy.Proxy' :: 'Data.Proxy.Proxy' i) ('return' a)
    --         ≡ 'return' ('Just' a)@
    -- [Implies-Zero]
    --     @(extractResultI ('Data.Proxy.Proxy' :: 'Data.Proxy.Proxy' i) m
    --         ≡ 'liftM' ('const' 'Nothing') m) ⇒ (∀f. m '>>=' f ≡ m)@
    --
    -- The @extractResultI@ operation in terms of which these laws are defined
    -- is given by:
    --
    -- @extractResultI :: forall proxy i m a. 'MonadInnerControl' i m
    --    => proxy i
    --    -> m a
    --    -> m ('Maybe' a)
    --extractResultI i m = do
    --    state <- 'captureI' i
    --    'liftI' '$' do
    --        (result, _) <- 'suspendI' m state
    --        'return' '$' 'extractI' i ('Data.Proxy.Proxy' :: 'Data.Proxy.Proxy' m) result@
    extractI :: proxy i -> proxy' m -> OuterResult i m a
        -> Either (OuterResult i m b) a

    -- | 'mapI' maps a function @f@ over the G(layerresult,layer results) of
    -- the G(outerlayer,outer layers) of @m@ around @i@.
    --
    -- On versions of GHC supporting
    -- BWT(NewAxioms/ClosedTypeFamilies, closed type families), this is
    -- redundant thanks to the @'Functor' ('OuterResult' i m)@ superclass
    -- constraint on 'MonadInnerControl'. It's only necessary because of how
    -- we implement 'OuterResult' on older GHCs.
    --
    -- Regardless, the only value to which this should ever be assigned is
    -- 'defaultMapI' (see <#g:9 Defaults>).
    mapI :: proxy i -> proxy' m -> (a -> b) -> OuterResult i m a
        -> OuterResult i m b
#ifdef ClosedTypeFamilies
    mapI _ _ = fmap
    {-# INLINE mapI #-}
#endif

#ifdef LANGUAGE_DefaultSignatures
-- DefaultSignatures doesn't work with multi-parameter type classes in GHC 7.2
#if __GLASGOW_HASKELL__ >= 704
    default suspendI :: DefaultMonadInnerControl i m
        => m a -> OuterState i m -> i (OuterEffects i m a)
    suspendI = defaultSuspendI

    default resumeI :: DefaultMonadInnerControl i m => proxy i
        -> OuterEffects i m a -> m a
    resumeI = defaultResumeI

    default captureI :: DefaultMonadInnerControl i m => proxy i
        -> m (OuterState i m)
    captureI = defaultCaptureI

    default extractI :: DefaultMonadInnerControl i m => proxy i -> proxy' m
        -> OuterResult i m a -> Either (OuterResult i m b) a
    extractI = defaultExtractI
#ifndef ClosedTypeFamilies

    default mapI :: DefaultMonadInnerControl i m => proxy i -> proxy' m
        -> (a -> b) -> OuterResult i m a -> OuterResult i m b
    mapI = defaultMapI
#endif
#endif
#endif


------------------------------------------------------------------------------
instance MonadInner m m => MonadInnerControl m m where
    suspendI m _ = liftM (\a -> (toR $ Identity a, toS ())) m
    resumeI _ (r, _) = let Identity a = fromR r in return a
    captureI _ = return $ toS ()
    extractI _ _ r = let Identity a = fromR r in Right a
    mapI _ _ f r = toR $ Identity (f a)
      where
        Identity a = fromR r


------------------------------------------------------------------------------
instance __OVERLAPPABLE__
    ( MonadInner i (t m)
    , MonadInnerControl i m
    , MonadTransControl t
#ifdef ClosedTypeFamilies
    , OuterResult i (t m) ~ ComposeResult i t m
    , OuterState i (t m) ~ (LayerState t, OuterState i m)
#endif
    , tm ~ t m
    )
  =>
    MonadInnerControl i tm
  where
    suspendI (m :: tm a) s = liftM f $ suspendI (suspend m ls) os
      where
        (ls, os) = fromS s
        compose or_ = ComposeResult or_ :: ComposeResult i t m a
        f (or_, os') = (toR (compose or_), toS (ls, os'))
    {-# INLINE suspendI #-}

    resumeI p ((r, s) :: OuterEffects i tm a) =
        lift (resumeI p (r', s')) >>= resume
      where
        (_, s') = fromS s
        ComposeResult r' = fromR r :: ComposeResult i t m a
    {-# INLINE resumeI #-}

    captureI p = do
        a <- capture
        b <- lift (captureI p)
        return $ toS (a, b)
    {-# INLINE captureI #-}

    extractI i _ (r :: OuterResult i tm a) = either left right $
        extractI i m r'
      where
        t = Pt :: Pt t
        m = Pm :: Pm m

        ComposeResult r' = fromR r :: ComposeResult i t m a

        left :: forall b. OuterResult i m (LayerResult t b, LayerState t)
            -> Either (OuterResult i tm b) a
        left or_ = Left $ toR or'
          where
            or' :: ComposeResult i t m b
            or' = ComposeResult or_
        {-# INLINE left #-}

        right :: forall b. (LayerResult t a, LayerState t)
            -> Either (OuterResult i tm b) a
        right (lr, ls) = case extract t lr of
            Left e -> Left $ toR or'
              where
                or' :: ComposeResult i t m b
                or' = ComposeResult $ mapI i m (const (e, ls)) r'
            Right a -> Right a
        {-# INLINE right #-}
    {-# INLINE extractI #-}

    mapI i _ (f :: a -> b) (r :: OuterResult i tm a) = toR r'
      where
        m = Pm :: Pm m
        r' :: ComposeResult i t m b
        r' = ComposeResult $ mapI i m (first (fmap f)) r''
          where
            ComposeResult r'' = fromR r :: ComposeResult i t m a
    {-# INLINE mapI #-}



#if MIN_VERSION_mmorph(1, 0, 1)
------------------------------------------------------------------------------
instance
    ( DefaultMonadInnerControl (f (g m)) (ComposeT f g m)
    , Functor (LayerResult f), Functor (LayerResult g)
    )
  =>
    MonadInnerControl (f (g m)) (ComposeT f g m)
  where
    suspendI = defaultSuspendI
    resumeI = defaultResumeI
    captureI = defaultCaptureI
    extractI = defaultExtractI
    mapI = defaultMapI


#endif
------------------------------------------------------------------------------
data Pm (m :: * -> *) = Pm


------------------------------------------------------------------------------
data Pt (t :: (* -> *) -> * -> *) = Pt


------------------------------------------------------------------------------
-- | 'liftControlI' is a composition of 'captureI', 'suspendI' and 'liftI'
-- provided for convenience (and compability with H(monad-control)).
--
-- It takes a continuation, to which it passes an operation which can be
-- used to \"lower\" a G(computation,computation) in the monad @m@ to a
-- G(computation,computation) in the monad @i@, which returns the
-- G(layereffect,reified) G(sideeffect,side-effects) of the
-- G(outerlayer,outer layers) around @i@ of @m@ of the original computation.
liftControlI :: forall i m a. MonadInnerControl i m
    => ((forall b. m b -> i (OuterEffects i m b)) -> i a)
    -> m a
liftControlI f = captureI (Pm :: Pm i) >>= \s -> liftI $ f (flip suspendI s)
{-# INLINE liftControlI #-}


------------------------------------------------------------------------------
-- | A version of 'liftControlI' that automatically resumes the
-- G(layereffect,suspended side-effects) returned from the continuation.
--
-- @catch' :: ('Control.Exception.Exception' e, 'MonadInnerControl' 'IO' m) => m b -> (e -> m b) -> m b
--catch' m h = 'controlI' (\\peel -> 'Control.Exception.catch' (peel m) (peel '.' h))@
controlI :: forall i m a. MonadInnerControl i m
    => ((forall b. m b -> i (OuterEffects i m b)) -> i (OuterEffects i m a))
    -> m a
controlI f = liftControlI (\peel -> f (coercePeelI peel))
    >>= resumeI (Pm :: Pm i)
{-# INLINE controlI #-}


------------------------------------------------------------------------------
-- | A particular application of 'liftControlI' that lifts
-- G(controloperation, control operations) from @(a -> i b) -> i b@ to
-- @(a -> m b) -> m b@.
--
-- @withMVar' :: 'MonadInnerControl' 'IO' m => 'Control.Concurrent.MVar.MVar' a -> (a -> m b) -> m b
--withMVar' = 'liftOpI' '.' 'Control.Concurrent.MVar.withMVar'@
liftOpI :: MonadInnerControl i m
     => ((a -> i (OuterEffects i m b)) -> i (OuterEffects i m c))
     -> (a -> m b)
     -> m c
liftOpI f = \g -> controlI $ \peel -> f $ peel . g
{-# INLINE liftOpI #-}


------------------------------------------------------------------------------
-- | A particular application of 'liftControlI' that lifts
-- G(controloperation,control operations) from @i a -> i b@ to @m a -> m b@.
--
-- @mask_' :: 'MonadInnerControl' 'IO' m => m a -> m a
--mask_' = 'liftOpI_' 'Control.Exception.mask_'@
liftOpI_ :: MonadInnerControl i m
    => (i (OuterEffects i m a) -> i (OuterEffects i m b))
     -> m a
     -> m b
liftOpI_ f = \m -> controlI $ \peel -> f $ peel m
{-# INLINE liftOpI_ #-}


------------------------------------------------------------------------------
-- | A particular application of 'liftControlI' that lifts
-- G(controloperation,control operations) from @i () -> i a@ to
-- @m () -> m a@.
--
-- @forkIO' :: 'MonadInnerControl' 'IO' m => m () -> m 'Control.Concurrent.ThreadId'
--forkIO' = 'liftDiscardI' 'Control.Concurrent.forkIO'@
-- 
-- Note: While the computation @m ()@ passed to the resulting operation has
-- access to the @'OuterEffects' i@ of @m@, it is run only for its side-effects
-- in @i@. Its side-effects in the outer layers of @m@ are discarded.
liftDiscardI :: MonadInnerControl i m => (i () -> i a) -> m () -> m a
liftDiscardI f = \m -> liftControlI $ \peel -> f $ liftM (const ()) $ peel m
{-# INLINE liftDiscardI #-}


------------------------------------------------------------------------------
-- | The constraint @'MonadInnerInvariant' i m@ holds when @i@ is an
-- G(innermonad,inner monad) of @m@ such that it is possible to lift
-- G(morphism, monad automorphisms) of @i@ to G(morphism,monad endomorphisms)
-- of @m@ using 'hoistisoI'. In other words, @'MonadInnerInvariant' i m@
-- implies the existence of a
-- G(morphism,invariant functor in the category of monads) from
-- @i@ to @m@.
class (MonadInner i m, MonadInner j n) =>
    MonadInnerInvariant j n i m
        | i j m -> n
        , i j n -> m
        , j n m -> i
        , i n m -> j
  where
    hoistisoI
        :: (forall b. i b -> j b)
        -> (forall b. j b -> i b)
        -> m a
        -> n a

#ifdef LANGUAGE_DefaultSignatures
-- DefaultSignatures doesn't work with multi-parameter type classes in GHC 7.2
#if __GLASGOW_HASKELL__ >= 704
    default hoistisoI :: DefaultMonadInnerInvariant j n i m
        => (forall b. i b -> j b)
        -> (forall b. j b -> i b)
        -> m a
        -> n a
    hoistisoI = defaultHoistisoI
#endif
#endif


------------------------------------------------------------------------------
instance (MonadInner m m, MonadInner n n) => MonadInnerInvariant n n m m where
    hoistisoI f _ = f


------------------------------------------------------------------------------
instance __OVERLAPPABLE__
    ( MonadInner i (t m), MonadInner j (t n)
    , MonadInnerInvariant j n i m, MonadInnerInvariant i m j n
    , MInvariant t
    , tn ~ t n, tm ~ t m
    )
  =>
    MonadInnerInvariant j tn i tm
  where
    hoistisoI f g = hoistiso (hoistisoI f g) (hoistisoI g f)
    {-# INLINE hoistisoI #-}


#if MIN_VERSION_mmorph(1, 0, 1)
------------------------------------------------------------------------------
instance
    ( DefaultMonadInnerInvariant
        (h (k n))
        (ComposeT h k n)
        (f (g m))
        (ComposeT f g m)
-- don't know why I have to do this on GHC 7.0, but I do
    , Monad (k n)
    , h ~ f
    )
  =>
    MonadInnerInvariant (h (k n)) (ComposeT h k n) (f (g m)) (ComposeT f g m)
  where
    hoistisoI = defaultHoistisoI


#endif
------------------------------------------------------------------------------
class MonadInnerInvariant j n i m => 
    MonadInnerFunctor j n i m
        | i j m -> n
        , i j n -> m
        , j n m -> i
        , i n m -> j
  where
    hoistI :: (forall b. i b -> j b) -> m a -> n a

#ifdef LANGUAGE_DefaultSignatures
-- DefaultSignatures doesn't work with multi-parameter type classes in GHC 7.2
#if __GLASGOW_HASKELL__ >= 704
    default hoistI :: DefaultMonadInnerFunctor j n i m
        => (forall b. i b -> j b)
        -> m a
        -> n a
    hoistI = defaultHoistI
#endif
#endif


------------------------------------------------------------------------------
instance MonadInnerInvariant n n m m => MonadInnerFunctor n n m m where
    hoistI f = f


------------------------------------------------------------------------------
instance __OVERLAPPABLE__
    ( MonadInnerFunctor j n i m, MFunctor t
    , MonadInnerInvariant j (t n) i (t m)
    , tn ~ t n, tm ~ t m
    )
  =>
    MonadInnerFunctor j tn i tm
  where
    hoistI f = hoist (hoistI f)
    {-# INLINE hoistI #-}


#if MIN_VERSION_mmorph(1, 0, 1)
------------------------------------------------------------------------------
instance
    ( DefaultMonadInnerFunctor
        (h (k n))
        (ComposeT h k n)
        (f (g m))
        (ComposeT f g m)
-- don't know why I have to do this on GHC 7.0, but I do
    , Monad (k n)
    , h ~ f
    , k ~ g
    )
  =>
    MonadInnerFunctor (h (k n)) (ComposeT h k n) (f (g m)) (ComposeT f g m)
  where
    hoistI = defaultHoistI


#endif
------------------------------------------------------------------------------
{-$defaults

The changes to the behaviour of the @GeneralizedNewtypeDeriving@ extension
that come with the new BWT(Roles, roles) mechanism in GHC 7.8 (which fixes GHC
bug B(7148) make it no longer possible to automatically derive instances of
'MonadInnerControl' the way it is for the other classes in the 'MonadInner'
familiy.

Rather than lose this useful feature altogether, the operations
'defaultSuspendI', 'defaultResumeI', 'defaultCaptureI' and 'defaultExtractI'
are provided. These operations can be used to implement an instance
@'MonadInnerControl' i n@ for some G(innermonad,inner monad) @i@, if @n@ is
G(morphism,isomorphic) to an @m@ for which there exists an instance
@'MonadInnerControl' i m@ (e.g., if @n@ is a newtype wrapper around @m@).


These operations use 'unsafeCoerce' internally in their implementation, but
in such a way that should be okay as long as the given
G(morphism,isomorphism) is valid.

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
    )

instance 'MonadInnerControl' 'IO' MyMonad where
    'suspendI' = 'defaultSuspendI' runMyMonad
    'resumeI'  = 'defaultResumeI'  MyMonad
    'captureI' = 'defaultCaptureI' MyMonad
    'extractI' = 'defaultExtractI' MyMonad

instance 'MonadInnerInvariant' 'IO' MyMonad 'IO' MyMonad where
    'hoistisoI' = 'defaultHoistisoI' MyMonad runMyMonad

instance 'MonadInnerFunctor'   'IO' MyMonad 'IO' MyMonad where
    'hoistI'    = 'defaultHoistI'    MyMonad runMyMonad
@

If you rely on a derived instance of 'MonadInnerControl' on a @newtype@, and
you want your code to work with GHC 7.8 and above, you should use these
operations to manually define an instance (as above) rather than using the
@GeneralizedNewtypeDeriving@ extension.

-}


------------------------------------------------------------------------------
-- | A UG(glasgow_exts.html#the-constraint-kind,constraint synonym) that helps
-- us write the type signature of 'defaultLift'.
#define DMT DefaultMonadTrans t u m
#define CDMT MonadTrans u, Monad m, Iso1 (t m)
#define EDMT Codomain1 (t m) ~ u m
newtypeCE(DMT, CDMT, EDMT)


------------------------------------------------------------------------------
-- | Used when defining an instance of 'MonadTrans' @t@.
--
-- The constraint @'DefaultMonadTrans' t u m@ essentially requires that @t@
-- be G(morphism,isomorphic) to a monad transformer @u@ which is already an
-- instance of 'MonadTrans'. This isomorphism is given by making @t m@ an
-- instance of 'Iso1' for all @m@ such that @'Codomain1' (t m) = u m@.
defaultLift :: CE(DMT, EDMT) => m a -> t m a
defaultLift = from1 . lift
{-# INLINE defaultLift #-}


------------------------------------------------------------------------------
-- | A UG(glasgow_exts.html#the-constraint-kind,constraint synonym) that helps
-- us write the type signature of 'defaultLift2'.
#define DMT2 DefaultMonadTrans2 t u v m
#define CDMT2 MonadTrans u, MonadTrans v\
    , Monad m, Monad (v m)\
    , Iso1 (t m)
#define EDMT2 Codomain1 (t m) ~ u (v m)
newtypeCE(DMT2, CDMT2, EDMT2)


------------------------------------------------------------------------------
-- | Used when defining an instance of 'MonadTrans' @t@.
--
-- The constraint @'DefaultMonadTrans2' t u v m@ essentially requires that @t@
-- be G(morphism,isomorphic) to a composition of two monad transformers @u@
-- and @v@ which are already instances of 'MonadTrans'. This isomorphism is
-- given by making @t m@ an instance of 'Iso1' for all @m@ such that
-- @'Codomain1' (t m) = u (v m)@.
defaultLift2 :: CE(DMT2, EDMT2) => m a -> t m a
defaultLift2 = from1 . lift . lift
{-# INLINE defaultLift2 #-}


------------------------------------------------------------------------------
-- | A UG(glasgow_exts.html#the-constraint-kind,constraint synonym) that helps
-- us write the type signature of 'defaultLift3'.
#define DMT3 DefaultMonadTrans3 t u v w m
#define CDMT3 MonadTrans u, MonadTrans v, MonadTrans w\
    , Monad m, Monad (w m), Monad (v (w m))\
    , Iso1 (t m)
#define EDMT3 Codomain1 (t m) ~ u (v (w m))
newtypeCE(DMT3, CDMT3, EDMT3)


------------------------------------------------------------------------------
-- | Used when defining an instance of 'MonadTrans' @t@.
--
-- The constraint @'DefaultMonadTrans3' t u v w m@ essentially requires that
-- @t@ be G(morphism,isomorphic) to a composition of three monad transformers
-- @u@, @v@ and @w@ which are already instances of 'MonadTrans'. This
-- isomorphism is given by making @t m@ an instance of 'Iso1' for all @m@ such
-- that @'Codomain1' (t m) = u (v (w m))@.
defaultLift3 :: CE(DMT3, EDMT3) => m a -> t m a
defaultLift3 = from1 . lift . lift . lift
{-# INLINE defaultLift3 #-}


------------------------------------------------------------------------------
-- | A UG(glasgow_exts.html#the-constraint-kind,constraint synonym) that helps
-- us write the type signature of 'defaultLift4'.
#define DMT4 DefaultMonadTrans4 t u v w x m
#define CDMT4 MonadTrans u, MonadTrans v, MonadTrans w, MonadTrans x\
    , Monad m, Monad (x m), Monad (w (x m)), Monad (v (w (x m)))\
    , Iso1 (t m)
#define EDMT4 Codomain1 (t m) ~ u (v (w (x m)))
newtypeCE(DMT4, CDMT4, EDMT4)


------------------------------------------------------------------------------
-- | Used when defining an instance of 'MonadTrans' @t@.
--
-- The constraint @'DefaultMonadTrans4' t u v w x m@ essentially requires that
-- @t@ be G(morphism,isomorphic) to a composition of four monad transformers
-- @u@, @v@, @w@ and @x@ which are already instances of 'MonadTrans'. This
-- isomorphism is given by making @t m@ an instance of 'Iso1' for all @m@ such
-- that @'Codomain1' (t m) = u (v (w (x m)))@.
defaultLift4 :: CE(DMT4, EDMT4) => m a -> t m a
defaultLift4 = from1 . lift . lift . lift . lift
{-# INLINE defaultLift4 #-}


------------------------------------------------------------------------------
-- | A UG(glasgow_exts.html#the-constraint-kind,constraint synonym) that helps
-- us write the type signatures of 'defaultSuspend', 'defaultResume',
-- 'defaultCapture' and 'defaultExtract'.
#define DMTC(m) DefaultMonadTransControl t u m
#define CDMTC(m) MonadTransControl u\
    , Monad m, Monad (u m)\
    , Iso1 (t m)
#define EDMTC(m) Codomain1 (t m) ~ u m\
    , LayerResult t ~ DefaultLayerResult u, LayerState t ~ DefaultLayerState u
newtypeCE(DMTC(m), CDMTC(m), EDMTC(m))


------------------------------------------------------------------------------
-- | The G(layerresult,layer result) of @u@.
type DefaultLayerResult u = LayerResult u


------------------------------------------------------------------------------
-- | The G(layerstate,layer state) of @u@.
type DefaultLayerState u = LayerState u


------------------------------------------------------------------------------
-- | Used when defining an instance of 'MonadTransControl' @t@.
--
-- The constraint @'DefaultMonadTransControl' t u m@ essentially requires
-- that @t@ be G(morphism,isomorphic) to a monad transformer @u@ which is
-- already an instance of 'MonadTransControl'. This isomorphism is given by
-- making @t m@ an instance of 'Iso1' for all @m@ such that
-- @'Codomain1' (t m) = u m@.
defaultSuspend :: CE(DMTC(m), EDMTC(m))
    => t m a -> LayerState t -> m (LayerEffects t a)
defaultSuspend m us = suspend (to1 m) us
{-# INLINE defaultSuspend #-}


------------------------------------------------------------------------------
-- | Used when defining an instance of 'MonadTransControl' @t@.
--
-- The constraint @'DefaultMonadTransControl' t u m@ essentially requires
-- that @t@ be G(morphism,isomorphic) to a monad transformer @u@ which is
-- already an instance of 'MonadTransControl'. This isomorphism is given by
-- making @t m@ an instance of 'Iso1' for all @m@ such that
-- @'Codomain1' (t m) = u m@.
defaultResume :: CE(DMTC(m), EDMTC(m)) => LayerEffects t a -> t m a
defaultResume = from1 . resume
{-# INLINE defaultResume #-}


------------------------------------------------------------------------------
-- | Used when defining an instance of 'MonadTransControl' @t@.
--
-- The constraint @'DefaultMonadTransControl' t u m@ essentially requires
-- that @t@ be G(morphism,isomorphic) to a monad transformer @u@ which is
-- already an instance of 'MonadTransControl'. This isomorphism is given by
-- making @t m@ an instance of 'Iso1' for all @m@ such that
-- @'Codomain1' (t m) = u m@.
defaultCapture :: CE(DMTC(m), EDMTC(m)) => t m (LayerState t)
defaultCapture = from1 capture
{-# INLINE defaultCapture #-}


------------------------------------------------------------------------------
-- | Used when defining an instance of 'MonadTransControl' @t@.
--
-- The constraint @'DefaultMonadTransControl' t u m@ essentially requires
-- that @t@ be G(morphism,isomorphic) to a monad transformer @u@ which is
-- already an instance of 'MonadTransControl'. This isomorphism is given by
-- making @t m@ an instance of 'Iso1' for all @m@ such that
-- @'Codomain1' (t m) = u m@.
defaultExtract
    :: forall t u a b proxy. CE(DMTC(Identity), EDMTC(Identity))
    => proxy t -> LayerResult t a -> Either (LayerResult t b) a
defaultExtract _ = extract (Pt :: Pt u)
{-# INLINE defaultExtract #-}


------------------------------------------------------------------------------
-- | A UG(glasgow_exts.html#the-constraint-kind,constraint synonym) that helps
-- us write the type signatures of 'defaultSuspend2', 'defaultResume2',
-- 'defaultCapture2' and 'defaultExtract2'.
#define DMTC2(m) DefaultMonadTransControl2 t u v m
#define CDMTC2(m) MonadTransControl u, MonadTransControl v\
    , Monad m, Monad (v m), Monad (u (v m))\
    , Iso1 (t m)
#define EDMTC2(m) Codomain1 (t m) ~ u (v m)\
    , LayerResult t ~ DefaultLayerResult2 u v\
    , LayerState t ~ DefaultLayerState2 u v
newtypeCE(DMTC2(m), CDMTC2(m), EDMTC2(m))


------------------------------------------------------------------------------
-- | The combined G(layerresult,layer results) of @u@ and @v@.
type DefaultLayerResult2 u v = ComposeResult2 u v


------------------------------------------------------------------------------
-- | The combined G(layerstate,layer states) of @u@ and @v@.
type DefaultLayerState2 u v = (LayerState u, LayerState v)


------------------------------------------------------------------------------
-- | Used when defining an instance of 'MonadTransControl' @t@.
--
-- The constraint @'DefaultMonadTransControl2' t u v m@ essentially requires
-- that @t@ be G(morphism,isomorphic) to a composition of two monad
-- transformers @u@ and @v@ which are already instances of
-- 'MonadTransControl'. This isomorphism is given by making @t m@ an instance
-- of 'Iso1' for all @m@ such that @'Codomain1' (t m) = u (v m)@.
defaultSuspend2 :: CE(DMTC2(m), EDMTC2(m))
    => t m a -> LayerState t -> m (LayerEffects t a)
defaultSuspend2 m (us, vs) = liftM f $ suspend (suspend (to1 m) us) vs
  where
    f (vr, vs') = (ComposeResult2 vr, (us, vs'))
{-# INLINE defaultSuspend2 #-}


------------------------------------------------------------------------------
-- | Used when defining an instance of 'MonadTransControl' @t@.
--
-- The constraint @'DefaultMonadTransControl2' t u v m@ essentially requires
-- that @t@ be G(morphism,isomorphic) to a composition of two monad
-- transformers @u@ and @v@ which are already instances of
-- 'MonadTransControl'. This isomorphism is given by making @t m@ an instance
-- of 'Iso1' for all @m@ such that @'Codomain1' (t m) = u (v m)@.
defaultResume2 :: CE(DMTC2(m), EDMTC2(m))
    => LayerEffects t a -> t m a
defaultResume2 (ComposeResult2 vr, (_, vs)) =
    from1 $ lift (resume (vr, vs)) >>= resume
{-# INLINE defaultResume2 #-}


------------------------------------------------------------------------------
-- | Used when defining an instance of 'MonadTransControl' @t@.
--
-- The constraint @'DefaultMonadTransControl2' t u v m@ essentially requires
-- that @t@ be G(morphism,isomorphic) to a composition of two monad
-- transformers @u@ and @v@ which are already instances of
-- 'MonadTransControl'. This isomorphism is given by making @t m@ an instance
-- of 'Iso1' for all @m@ such that @'Codomain1' (t m) = u (v m)@.
defaultCapture2 :: CE(DMTC2(m), EDMTC2(m)) => t m (LayerState t)
defaultCapture2 = from1 $ liftM2 (,) capture (lift capture)
{-# INLINE defaultCapture2 #-}


------------------------------------------------------------------------------
-- | Used when defining an instance of 'MonadTransControl' @t@.
--
-- The constraint @'DefaultMonadTransControl2' t u v m@ essentially requires
-- that @t@ be G(morphism,isomorphic) to a composition of two monad
-- transformers @u@ and @v@ which are already instances of
-- 'MonadTransControl'. This isomorphism is given by making @t m@ an instance
-- of 'Iso1' for all @m@ such that @'Codomain1' (t m) = u (v m)@.
defaultExtract2
    :: forall t u v a b proxy. CE(DMTC2(Identity), EDMTC2(Identity))
    => proxy t -> LayerResult t a -> Either (LayerResult t b) a
defaultExtract2 _ (ComposeResult2 vr) = case extract (Pt :: Pt v) vr of
    Left vre -> Left (ComposeResult2 vre)
    Right (ur, us) -> case extract (Pt :: Pt u) ur of
        Left ure -> Left $ ComposeResult2 $ fmap (const (ure, us)) vr
        Right a -> Right a
{-# INLINE defaultExtract2 #-}


------------------------------------------------------------------------------
-- | A UG(glasgow_exts.html#the-constraint-kind,constraint synonym) that helps
-- us write the type signatures of 'defaultSuspend3', 'defaultResume3',
-- 'defaultCapture3' and 'defaultExtract3'.
#define DMTC3(m) DefaultMonadTransControl3 t u v w m
#define CDMTC3(m) MonadTransControl u, MonadTransControl v\
    , MonadTransControl w\
    , Monad m, Monad (w m), Monad (v (w m)), Monad (u (v (w m)))\
    , Iso1 (t m)
#define EDMTC3(m) Codomain1 (t m) ~ u (v (w m))\
    , LayerResult t ~ DefaultLayerResult3 u v w\
    , LayerState t ~ DefaultLayerState3 u v w
newtypeCE(DMTC3(m), CDMTC3(m), EDMTC3(m))


------------------------------------------------------------------------------
-- | The combined G(layerresult,layer results) of @u@, @v@ and @w@.
type DefaultLayerResult3 u v w = ComposeResult3 u v w


------------------------------------------------------------------------------
-- | The combined G(layerstate,layer states) of @u@, @v@ and @w@.
type DefaultLayerState3 u v w = (LayerState u, LayerState v, LayerState w)


------------------------------------------------------------------------------
-- | Used when defining an instance of 'MonadTransControl' @t@.
--
-- The constraint @'DefaultMonadTransControl3' t u v w m@ essentially requires
-- that @t@ be G(morphism,isomorphic) to a composition of three monad
-- transformers @u@, @v@ and @w@ which are already instances of
-- 'MonadTransControl'. This isomorphism is given by making @t m@ an instance
-- of 'Iso1' for all @m@ such that @'Codomain1' (t m) = u (v (w m))@.
defaultSuspend3 :: CE(DMTC3(m), EDMTC3(m))
    => t m a -> LayerState t -> m (LayerEffects t a)
defaultSuspend3 m (us, vs, ws) =
    liftM f $ suspend (suspend (suspend (to1 m) us) vs) ws
  where
    f (wr, ws') = (ComposeResult3 wr, (us, vs, ws'))
{-# INLINE defaultSuspend3 #-}


------------------------------------------------------------------------------
-- | Used when defining an instance of 'MonadTransControl' @t@.
--
-- The constraint @'DefaultMonadTransControl3' t u v w m@ essentially requires
-- that @t@ be G(morphism,isomorphic) to a composition of three monad
-- transformers @u@, @v@ and @w@ which are already instances of
-- 'MonadTransControl'. This isomorphism is given by making @t m@ an instance
-- of 'Iso1' for all @m@ such that @'Codomain1' (t m) = u (v (w m))@.
defaultResume3 :: CE(DMTC3(m), EDMTC3(m))
    => LayerEffects t a -> t m a
defaultResume3 (ComposeResult3 wr, (_, _, ws)) =
    from1 $ lift (lift (resume (wr, ws))) >>= lift . resume >>= resume
{-# INLINE defaultResume3 #-}


------------------------------------------------------------------------------
-- | Used when defining an instance of 'MonadTransControl' @t@.
--
-- The constraint @'DefaultMonadTransControl3' t u v w m@ essentially requires
-- that @t@ be G(morphism,isomorphic) to a composition of three monad
-- transformers @u@, @v@ and @w@ which are already instances of
-- 'MonadTransControl'. This isomorphism is given by making @t m@ an instance
-- of 'Iso1' for all @m@ such that @'Codomain1' (t m) = u (v (w m))@.
defaultCapture3 :: CE(DMTC3(m), EDMTC3(m)) => t m (LayerState t)
defaultCapture3 = from1 $
    liftM3 (,,) capture (lift capture) (lift (lift capture))
{-# INLINE defaultCapture3 #-}


------------------------------------------------------------------------------
-- | Used when defining an instance of 'MonadTransControl' @t@.
--
-- The constraint @'DefaultMonadTransControl3' t u v w m@ essentially requires
-- that @t@ be G(morphism,isomorphic) to a composition of three monad
-- transformers @u@, @v@ and @w@ which are already instances of
-- 'MonadTransControl'. This isomorphism is given by making @t m@ an instance
-- of 'Iso1' for all @m@ such that @'Codomain1' (t m) = u (v (w m))@.
defaultExtract3
    :: forall t u v w a b proxy. CE(DMTC3(Identity), EDMTC3(Identity))
    => proxy t -> LayerResult t a -> Either (LayerResult t b) a
defaultExtract3 _ (ComposeResult3 wr) = case extract (Pt :: Pt w) wr of
    Left wre -> Left (ComposeResult3 wre)
    Right (vr, vs) -> case extract (Pt :: Pt v) vr of
        Left vre -> Left $ ComposeResult3 $ fmap (const (vre, vs)) wr
        Right (ur, us) -> case extract (Pt :: Pt u) ur of
            Left ure -> Left $ ComposeResult3 $
                fmap (first (fmap (const (ure, us)))) wr
            Right a -> Right a
{-# INLINE defaultExtract3 #-}


------------------------------------------------------------------------------
-- | A UG(glasgow_exts.html#the-constraint-kind,constraint synonym) that helps
-- us write the type signatures of 'defaultSuspend4', 'defaultResume4',
-- 'defaultCapture4' and 'defaultExtract4'.
#define DMTC4(m) DefaultMonadTransControl4 t u v w x m
#define CDMTC4(m) MonadTransControl u, MonadTransControl v\
    , MonadTransControl w, MonadTransControl x\
    , Monad m, Monad (x m), Monad (w (x m)), Monad (v (w (x m)))\
    , Monad (u (v (w (x m))))\
    , Iso1 (t m)
#define EDMTC4(m) Codomain1 (t m) ~ u (v (w (x m)))\
    , LayerResult t ~ DefaultLayerResult4 u v w x\
    , LayerState t ~ DefaultLayerState4 u v w x
newtypeCE(DMTC4(m), CDMTC4(m), EDMTC4(m))


------------------------------------------------------------------------------
-- | The combined G(layerresult,layer results) of @u@, @v@, @w@ and @x@.
type DefaultLayerResult4 u v w x = ComposeResult4 u v w x


------------------------------------------------------------------------------
-- | The combined G(layerstate,layer states) of @u@, @v@, @w@ and @x@.
type DefaultLayerState4 u v w x =
    (LayerState u, LayerState v, LayerState w, LayerState x)


------------------------------------------------------------------------------
-- | Used when defining an instance of 'MonadTransControl' @t@.
--
-- The constraint @'DefaultMonadTransControl4' t u v w x m@ essentially
-- requires that @t@ be G(morphism,isomorphic) to a composition of four monad
-- transformers @u@, @v@, @w@ and @x@ which are already instances of
-- 'MonadTransControl'. This isomorphism is given by making @t m@ an instance
-- of 'Iso1' for all @m@ such that @'Codomain1' (t m) = u (v (w (x m)))@.
defaultSuspend4 :: CE(DMTC4(m), EDMTC4(m))
    => t m a -> LayerState t -> m (LayerEffects t a)
defaultSuspend4 m (us, vs, ws, xs) =
    liftM f $ suspend (suspend (suspend (suspend (to1 m) us) vs) ws) xs
  where
    f (xr, xs') = (ComposeResult4 xr, (us, vs, ws, xs'))
{-# INLINE defaultSuspend4 #-}


------------------------------------------------------------------------------
-- | Used when defining an instance of 'MonadTransControl' @t@.
--
-- The constraint @'DefaultMonadTransControl4' t u v w x m@ essentially
-- requires that @t@ be G(morphism,isomorphic) to a composition of four monad
-- transformers @u@, @v@, @w@ and @x@ which are already instances of
-- 'MonadTransControl'. This isomorphism is given by making @t m@ an instance
-- of 'Iso1' for all @m@ such that @'Codomain1' (t m) = u (v (w (x m)))@.
defaultResume4 :: CE(DMTC4(m), EDMTC4(m))
    => LayerEffects t a -> t m a
defaultResume4 (ComposeResult4 xr, (_, _, _, xs)) =
    from1 $ lift (lift (lift (resume (xr, xs)))) >>= lift . lift . resume
        >>= lift . resume >>= resume
{-# INLINE defaultResume4 #-}


------------------------------------------------------------------------------
-- | Used when defining an instance of 'MonadTransControl' @t@.
--
-- The constraint @'DefaultMonadTransControl4' t u v w x m@ essentially
-- requires that @t@ be G(morphism,isomorphic) to a composition of four monad
-- transformers @u@, @v@, @w@ and @x@ which are already instances of
-- 'MonadTransControl'. This isomorphism is given by making @t m@ an instance
-- of 'Iso1' for all @m@ such that @'Codomain1' (t m) = u (v (w (x m)))@.
defaultCapture4 :: CE(DMTC4(m), EDMTC4(m)) => t m (LayerState t)
defaultCapture4 = from1 $ liftM4 (,,,)
    capture (lift capture) (lift (lift capture)) (lift (lift (lift capture)))
{-# INLINE defaultCapture4 #-}


------------------------------------------------------------------------------
-- | Used when defining an instance of 'MonadTransControl' @t@.
--
-- The constraint @'DefaultMonadTransControl4' t u v w x m@ essentially
-- requires that @t@ be G(morphism,isomorphic) to a composition of four monad
-- transformers @u@, @v@, @w@ and @x@ which are already instances of
-- 'MonadTransControl'. This isomorphism is given by making @t m@ an instance
-- of 'Iso1' for all @m@ such that @'Codomain1' (t m) = u (v (w (x m)))@.
defaultExtract4
    :: forall t u v w x a b proxy. CE(DMTC4(Identity), EDMTC4(Identity))
    => proxy t -> LayerResult t a -> Either (LayerResult t b) a
defaultExtract4 _ (ComposeResult4 xr) = case extract (Pt :: Pt x) xr of
    Left xre -> Left $ ComposeResult4 xre
    Right (wr, ws) -> case extract (Pt :: Pt w) wr of
        Left wre -> Left $ ComposeResult4 $ fmap (const (wre, ws)) xr
        Right (vr, vs) -> case extract (Pt :: Pt v) vr of
            Left vre -> Left $ ComposeResult4 $
                fmap (first (fmap (const (vre, vs)))) xr
            Right (ur, us) -> case extract (Pt :: Pt u) ur of
                Left ure -> Left $ ComposeResult4 $
                    fmap (first (fmap (first (fmap (const (ure, us)))))) xr
                Right a -> Right a
{-# INLINE defaultExtract4 #-}


------------------------------------------------------------------------------
-- | A UG(glasgow_exts.html#the-constraint-kind,constraint synonym) that helps
-- us write the type signature of 'defaultHoistiso'.
#define DMI DefaultMInvariant t u m n
#define CDMI MInvariant u\
    , Monad m, Monad n\
    , Iso1 (t m), Iso1 (t n)
#define EDMI Codomain1 (t m) ~ u m, Codomain1 (t n) ~ u n
newtypeCE(DMI, CDMI, EDMI)


------------------------------------------------------------------------------
-- | Used defining an instance 'MInvariant' @t@.
--
-- The constraint @'DefaultMInvariant' t u m n@ essentially requires that @t@
-- be G(morphism,isomorphic) to a monad transformers @u@ which is already an
-- instance of 'MInvariant'. This isomorphism is given by making @t m@ an
-- instance of 'Iso1' for all @m@ such that @'Codomain1' (t m) = u m@.
defaultHoistiso :: CE(DMI, EDMI)
    => (forall b. m b -> n b) -> (forall b. n b -> m b) -> t m a -> t n a
defaultHoistiso f g = from1 . hoistiso f g . to1
{-# INLINE defaultHoistiso #-}


------------------------------------------------------------------------------
-- | A UG(glasgow_exts.html#the-constraint-kind,constraint synonym) that helps
-- us write the type signature of 'defaultHoistiso2'.
#define DMI2 DefaultMInvariant2 t u v m n
#define CDMI2 MInvariant u, MInvariant v\
    , Monad m, Monad (v m), Monad n, Monad (v n)\
    , Iso1 (t m), Iso1 (t n)
#define EDMI2 Codomain1 (t m) ~ u (v m), Codomain1 (t n) ~ u (v n)
newtypeCE(DMI2, CDMI2, EDMI2)


------------------------------------------------------------------------------
-- | Used defining an instance 'MInvariant' @t@.
--
-- The constraint @'DefaultMInvariant2' t u v m n@ essentially requires that
-- @t@ be G(morphism,isomorphic) to a composition of two monad transformers
-- @u@ and @v@ which are already instances of 'MInvariant'. This isomorphism
-- is given by making @t m@ an instance of 'Iso1' for all @m@ such that
-- @'Codomain1' (t m) = u (v m)@.
defaultHoistiso2 :: CE(DMI2, EDMI2)
    => (forall b. m b -> n b) -> (forall b. n b -> m b) -> t m a -> t n a
defaultHoistiso2 f g = from1 . hoistiso (hoistiso f g) (hoistiso g f) . to1
{-# INLINE defaultHoistiso2 #-}


------------------------------------------------------------------------------
-- | A UG(glasgow_exts.html#the-constraint-kind,constraint synonym) that helps
-- us write the type signature of 'defaultHoistiso3'.
#define DMI3 DefaultMInvariant3 t u v w m n
#define CDMI3 MInvariant u, MInvariant v, MInvariant w\
    , Monad m, Monad (w m), Monad (v (w m))\
    , Monad n, Monad (w n), Monad (v (w n))\
    , Iso1 (t m), Iso1 (t n)
#define EDMI3 Codomain1 (t m) ~ u (v (w m)), Codomain1 (t n) ~ u (v (w n))
newtypeCE(DMI3, CDMI3, EDMI3)


------------------------------------------------------------------------------
-- | Used defining an instance 'MInvariant' @t@.
--
-- The constraint @'DefaultMInvariant3' t u v w m n@ essentially requires that
-- @t@ be G(morphism,isomorphic) to a composition of three monad transformers
-- @u@, @v@ and @w@ which are already instances of 'MInvariant'. This
-- isomorphism is given by making @t m@ an instance of 'Iso1' for all @m@ such
-- that @'Codomain1' (t m) = u (v m)@.
defaultHoistiso3 :: CE(DMI3, EDMI3)
    => (forall b. m b -> n b) -> (forall b. n b -> m b) -> t m a -> t n a
defaultHoistiso3 f g = from1
    . hoistiso
        (hoistiso (hoistiso f g) (hoistiso g f))
        (hoistiso (hoistiso g f) (hoistiso f g))
    . to1
{-# INLINE defaultHoistiso3 #-}


------------------------------------------------------------------------------
-- | A UG(glasgow_exts.html#the-constraint-kind,constraint synonym) that helps
-- us write the type signature of 'defaultHoistiso4'.
#define DMI4 DefaultMInvariant4 t u v w x m n
#define CDMI4 MInvariant u, MInvariant v, MInvariant w, MInvariant x\
    , Monad m, Monad (x m), Monad (w (x m)), Monad (v (w (x m)))\
    , Monad n, Monad (x n), Monad (w (x n)), Monad (v (w (x n)))\
    , Iso1 (t m), Iso1 (t n)
#define EDMI4 Codomain1 (t m) ~ u (v (w (x m)))\
    , Codomain1 (t n) ~ u (v (w (x n)))
newtypeCE(DMI4, CDMI4, EDMI4)


------------------------------------------------------------------------------
-- | Used defining an instance 'MInvariant' @t@.
--
-- The constraint @'DefaultMInvariant4' t u v w x m n@ essentially requires
-- that @t@ be G(morphism,isomorphic) to a composition of four monad
-- transformers @u@, @v@, @w@ and @x@ which are already instances of
-- 'MInvariant'. This isomorphism is given by making @t m@ an instance of
-- 'Iso1' for all @m@ such that @'Codomain1' (t m) = u (v (x m))@.
defaultHoistiso4 :: CE(DMI4, EDMI4)
    => (forall b. m b -> n b) -> (forall b. n b -> m b) -> t m a -> t n a
defaultHoistiso4 f g = from1
    . hoistiso
        (hoistiso
            (hoistiso (hoistiso f g) (hoistiso g f))
            (hoistiso (hoistiso g f) (hoistiso f g)))
        (hoistiso
            (hoistiso (hoistiso g f) (hoistiso f g))
            (hoistiso (hoistiso f g) (hoistiso g f)))
    . to1
{-# INLINE defaultHoistiso4 #-}


------------------------------------------------------------------------------
-- | A UG(glasgow_exts.html#the-constraint-kind,constraint synonym) that helps
-- us write the type signature of 'defaultHoist2'.
#define DMF DefaultMFunctor t u m n
#define CDMF MFunctor u\
    , Monad m\
    , Iso1 (t m), Iso1 (t n)
#define EDMF Codomain1 (t m) ~ u m, Codomain1 (t n) ~ u n
newtypeCE(DMF, CDMF, EDMF)


------------------------------------------------------------------------------
-- | Used defining an instance 'MFunctor' @t@.
--
-- The constraint @'DefaultMFunctor' t u m n@ essentially requires that @t@ be
-- G(morphism,isomorphic) to a monad transformer @u@ which is already an
-- instance of 'MFunctor'. This isomorphism is given by making @t m@ an
-- instance of 'Iso1' for all @m@ such that @'Codomain1' (t m) = u m@.
defaultHoist :: CE(DMF, EDMF)
    => (forall b. m b -> n b) -> t m a -> t n a
defaultHoist f = from1 . hoist f . to1
{-# INLINE defaultHoist #-}


------------------------------------------------------------------------------
-- | A UG(glasgow_exts.html#the-constraint-kind,constraint synonym) that helps
-- us write the type signature of 'defaultHoist2'.
#define DMF2 DefaultMFunctor2 t u v m n
#define CDMF2 MFunctor u, MFunctor v\
    , Monad m, Monad (v m)\
    , Iso1 (t m), Iso1 (t n)
#define EDMF2 Codomain1 (t m) ~ u (v m), Codomain1 (t n) ~ u (v n)
newtypeCE(DMF2, CDMF2, EDMF2)


------------------------------------------------------------------------------
-- | Used defining an instance 'MFunctor' @t@.
--
-- The constraint @'DefaultMFunctor2' t u v m n@ essentially requires that
-- @t@ be G(morphism,isomorphic) to a composition of two monad transformers
-- @u@ and @v@ which are already instances of 'MFunctor'. This isomorphism is
-- given by making @t m@ an instance of 'Iso1' for all @m@ such that
-- @'Codomain1' (t m) = u (v m)@.
defaultHoist2 :: CE(DMF2, EDMF2)
    => (forall b. m b -> n b) -> t m a -> t n a
defaultHoist2 f = from1 . hoist (hoist f) . to1
{-# INLINE defaultHoist2 #-}


------------------------------------------------------------------------------
-- | A UG(glasgow_exts.html#the-constraint-kind,constraint synonym) that helps
-- us write the type signature of 'defaultHoist3'.
#define DMF3 DefaultMFunctor3 t u v w m n
#define CDMF3 MFunctor u, MFunctor v, MFunctor w\
    , Monad m, Monad (w m), Monad (v (w m))\
    , Iso1 (t m), Iso1 (t n)
#define EDMF3 Codomain1 (t m) ~ u (v (w m)), Codomain1 (t n) ~ u (v (w n))
newtypeCE(DMF3, CDMF3, EDMF3)


------------------------------------------------------------------------------
-- | Used defining an instance 'MFunctor' @t@.
--
-- The constraint @'DefaultMFunctor3' t u v w m n@ essentially requires that
-- @t@ be G(morphism,isomorphic) to a composition of three monad transformers
-- @u@, @v@ and @w@ which are already instances of 'MFunctor'. This
-- isomorphism is given by making @t m@ an instance of 'Iso1' for all @m@ such
-- that @'Codomain1' (t m) = u (v m)@.
defaultHoist3 :: CE(DMF3, EDMF3)
    => (forall b. m b -> n b) -> t m a -> t n a
defaultHoist3 f = from1 . hoist (hoist (hoist f)) . to1
{-# INLINE defaultHoist3 #-}


------------------------------------------------------------------------------
-- | A UG(glasgow_exts.html#the-constraint-kind,constraint synonym) that helps
-- us write the type signature of 'defaultHoist4'.
#define DMF4 DefaultMFunctor4 t u v w x m n
#define CDMF4 MFunctor u, MFunctor v, MFunctor w, MFunctor x\
    , Monad m, Monad (x m), Monad (w (x m)), Monad (v (w (x m)))\
    , Iso1 (t m), Iso1 (t n)
#define EDMF4 Codomain1 (t m) ~ u (v (w (x m)))\
    , Codomain1 (t n) ~ u (v (w (x n)))
newtypeCE(DMF4, CDMF4, EDMF4)


------------------------------------------------------------------------------
-- | Used defining an instance 'MFunctor' @t@.
--
-- The constraint @'DefaultMFunctor4' t u v w x m n@ essentially requires that
-- @t@ be G(morphism,isomorphic) to a composition of four monad transformers
-- @u@, @v@, @w@ and @x@ which are already instances of 'MFunctor'. This
-- isomorphism is given by making @t m@ an instance of 'Iso1' for all @m@ such
-- that @'Codomain1' (t m) = u (v (x m))@.
defaultHoist4 :: CE(DMF4, EDMF4)
    => (forall b. m b -> n b) -> t m a -> t n a
defaultHoist4 f = from1 . hoist (hoist (hoist (hoist f))) . to1
{-# INLINE defaultHoist4 #-}


------------------------------------------------------------------------------
-- | A UG(glasgow_exts.html#the-constraint-kind,constraint synonym) that helps
-- us write the type signature of 'defaultLiftI'.
newtypeC(DefaultMonadInner i m, (Iso1 m, MonadInner i (Codomain1 m)))


------------------------------------------------------------------------------
-- | Used when manually defining an instance of @'MonadInner' i@ for some
-- monad @m@.
--
-- The constraint @'DefaultMonadInner' i m@ essentially requires that @m@ be
-- G(morphism,isomorphic) to some monad @m'@ which is already an instance of
-- @'MonadInner' i@. This isomorphism is given by making @m@ an instance of
-- 'Iso1' such that @'Codomain1' m = m'@.
defaultLiftI :: DefaultMonadInner i m => i a -> m a
defaultLiftI = from1 . liftI
{-# INLINE defaultLiftI #-}


------------------------------------------------------------------------------
-- | A UG(glasgow_exts.html#the-constraint-kind,constraint synonym) that helps
-- us write the type signatures of 'defaultSuspendI', 'defaultResumeI',
-- 'defaultCaptureI' and 'defaultExtractI'.
#ifdef ClosedTypeFamilies
newtypeC(DefaultMonadInnerControl i m,
    ( MonadInner i m
    , DefaultMonadInner i m
    , MonadInnerControl i (Codomain1 m)
    , OuterResult i m ~ OuterResult i (Codomain1 m)
    , OuterState i m ~ OuterState i (Codomain1 m)
    ))
#else
newtypeC(DefaultMonadInnerControl i m,
    ( MonadInner i m
    , DefaultMonadInner i m
    , MonadInnerControl i (Codomain1 m)
    ))
#endif


------------------------------------------------------------------------------
-- | Used when manually defining an instance of @'MonadInnerControl' i@ for
-- some monad @m@.
--
-- The constraint @'DefaultMonadInnerControl' i m@ essentially requires that
-- @m@ be G(morphism,isomorphic) to some monad @m'@ which is already an
-- instance of @'MonadInnerControl ' i@. This isomorphism is given by making
-- @m@ an instance of 'Iso1' such that @'Codomain1' m = m'@.
defaultSuspendI :: DefaultMonadInnerControl i m
    => m a
    -> OuterState i m
    -> i (OuterEffects i m a)
defaultSuspendI m s = liftM (toR *** toS) $ suspendI (to1 m) (fromS s)
{-# INLINE defaultSuspendI #-}


------------------------------------------------------------------------------
-- | Used when manually defining an instance of @'MonadInnerControl' i@ for
-- some monad @m@.
--
-- The constraint @'DefaultMonadInnerControl' i m@ essentially requires that
-- @m@ be G(morphism,isomorphic) to some monad @m'@ which is already an
-- instance of @'MonadInnerControl ' i@. This isomorphism is given by making
-- @m@ an instance of 'Iso1' such that @'Codomain1' m = m'@.
defaultResumeI :: DefaultMonadInnerControl i m
    => proxy i
    -> OuterEffects i m a
    -> m a
defaultResumeI p = from1 . resumeI p . (fromR *** fromS)
{-# INLINE defaultResumeI #-}


------------------------------------------------------------------------------
-- | Used when manually defining an instance of @'MonadInnerControl' i@ for
-- some monad @m@.
--
-- The constraint @'DefaultMonadInnerControl' i m@ essentially requires that
-- @m@ be G(morphism,isomorphic) to some monad @m'@ which is already an
-- instance of @'MonadInnerControl ' i@. This isomorphism is given by making
-- @m@ an instance of 'Iso1' such that @'Codomain1' m = m'@.
defaultCaptureI :: DefaultMonadInnerControl i m
    => proxy i
    -> m (OuterState i m)
defaultCaptureI p = from1 (liftM toS (captureI p))
{-# INLINE defaultCaptureI #-}


------------------------------------------------------------------------------
-- | Used when manually defining an instance of @'MonadInnerControl' i@ for
-- some monad @m@.
--
-- The constraint @'DefaultMonadInnerControl' i m@ essentially requires that
-- @m@ be G(morphism,isomorphic) to some monad @m'@ which is already an
-- instance of @'MonadInnerControl ' i@. This isomorphism is given by making
-- @m@ an of instance 'Iso1' such that @'Codomain1' m = m'@.
defaultExtractI :: forall i m a b proxy proxy'. DefaultMonadInnerControl i m
    => proxy i
    -> proxy' m
    -> OuterResult i m a
    -> Either (OuterResult i m b) a
defaultExtractI p _ r = either (Left . to) Right $
    extractI p (Pm :: Pm (Codomain1 m)) (fromR r)
  where
    to :: OuterResult i (Codomain1 m) b -> OuterResult i m b
    to = toR
{-# INLINE defaultExtractI #-}


------------------------------------------------------------------------------
-- | Used when manually defining an instance of @'MonadInnerControl' i@ for
-- some monad @m@.
--
-- The constraint @'DefaultMonadInnerControl' i m@ essentially requires that
-- @m@ be G(morphism,isomorphic) to some monad @m'@ which is already an
-- instance of @'MonadInnerControl ' i@. This isomorphism is given by making
-- @m@ an of instance 'Iso1' such that @'Codomain1' m = m'@.
defaultMapI :: forall i m a b proxy proxy'. DefaultMonadInnerControl i m
    => proxy i -> proxy' m
    -> (a -> b) -> OuterResult i m a -> OuterResult i m b
defaultMapI i _ f = to . mapI i (Pm :: Pm (Codomain1 m)) f . from
  where
    to :: OuterResult i (Codomain1 m) b -> OuterResult i m b
    to = toR

    from :: OuterResult i m a -> OuterResult i (Codomain1 m) a
    from = fromR
{-# INLINE defaultMapI #-}


------------------------------------------------------------------------------
-- | A UG(glasgow_exts.html#the-constraint-kind,constraint synonym) that helps
-- us write the type signature of 'defaultHoistisoI'.
newtypeC(DefaultMonadInnerInvariant j n i m,
    ( MonadInner i m
    , MonadInner j n
    , DefaultMonadInner i m
    , DefaultMonadInner j n
    , MonadInnerInvariant j (Codomain1 n) i (Codomain1 m)
    ))


------------------------------------------------------------------------------
-- | Used when manually defining an instance @'MonadInnerInvariant' j n i m@
-- for some pair of monads @m@ and @n@.
--
-- The constraint @'DefaultMonadInnerInvariant' j n i m@ essentially requires
-- that @m@ and @n@ be G(morphism,isomorphic) to some pair of monads @m'@ and
-- @n'@ for which there already exists an instance
-- @'MonadInnerInvariant j n' i m'@. These isomorphisms are given by making
-- @m@ and @n@ instances of 'Iso1' such that @'Codomain1' m = m'@ and
-- @'Codomain1' n = n'@.
defaultHoistisoI :: DefaultMonadInnerInvariant j n i m
    => (forall b. i b -> j b)
    -> (forall b. j b -> i b)
    -> m a
    -> n a
defaultHoistisoI f g m = from1 (hoistisoI f g (to1 m))
{-# INLINE defaultHoistisoI #-}


------------------------------------------------------------------------------
-- | A UG(glasgow_exts.html#the-constraint-kind,constraint synonym) that helps
-- us write the type signature of 'defaultHoistI.
newtypeC(DefaultMonadInnerFunctor j n i m,
    ( MonadInnerInvariant j n i m
    , DefaultMonadInnerInvariant j n i m
    , MonadInnerFunctor j (Codomain1 n) i (Codomain1 m)
    ))


------------------------------------------------------------------------------
-- | Used when manually defining an instance @'MonadInnerFunctor' j n i m@
-- for some pair of monads @m@ and @n@.
--
-- The constraint @'DefaultMonadInnerFunctor' j n i m@ essentially requires
-- that @m@ and @n@ be G(morphism,isomorphic) to some pair of monads @m'@ and
-- @n'@ for which there already exists an instance
-- @'MonadInnerFunctor j n' i m'@. These isomorphisms are given by making @m@
-- and @n@ instances of 'Iso1' such that @'Codomain1' m = m'@ and
-- @'Codomain1' n = n'@.
defaultHoistI :: DefaultMonadInnerFunctor j n i m
    => (forall b. i b -> j b)
    -> m a
    -> n a
defaultHoistI f m = from1 (hoistI f (to1 m))
{-# INLINE defaultHoistI #-}
