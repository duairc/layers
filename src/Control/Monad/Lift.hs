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

{-|

This module is the core of the
@<http://hackage.haskell.org/package/layers layers>@ package. It exports
everything you
need for to implement monad transformers compatible with
@<http://hackage.haskell.org/package/layers layers>@' monad interfaces, and
everything that you need to lift computations, operations and morphisms
through arbitrarily complicated stacks of monad transformers.

The @<http://hackage.haskell.org/package/layers layers>@ machinery is built
upon two twin families of interfaces. The 'MonadTrans' family of interfaces is
for lifting computations, operations and morphisms up exactly one level in the
transformer stack. That is, it lifts computations from @m a@ to @t m a@. The
'MonadLift' family of interfaces is for lifting computations, operations and
morphisms from any level of the transformer stack to the top of the
transformer stack. That is, it lifts computations from @i a@ to @m a@ (where
@m@ is a monad built from a stack of transformers and @i@ is the constraint
@'MonadLift' i m@ holds). The 'MonadTrans' family of interfaces is mainly used
by libraries that implement monad transformers and monad interfaces, while the
'MonadLift' family is used by applications make use of (stacks of) these
transformers.

-}

module Control.Monad.Lift
    (
    -- * The @MonadTrans@ family
    -- $transfamily

    -- ** Lifting computations
      MonadTrans (lift)

    -- ** Lifting control operations
    , MonadTransControl
    , Layer
    , LayerResult
    , LayerState
    , peel
    , suspend
    , restore
    , extract
    , result
    , liftControl
    , control
    , liftOp
    , liftOp_
    , liftDiscard

    -- ** Lifting morphisms
    , MInvariant (hoistiso)
    , MFunctor (hoist)

    -- * The @MonadLift@ family
    -- $liftfamily

    -- ** Lifting computations
    , MonadLift (lift')

    -- ** Lifting control operations
    , MonadLiftControl
    , Lift
    , LiftResult
    , LiftState
    , peel'
    , suspend'
    , restore'
    , extract'
    , result'
    , liftControl'
    , control'
    , liftOp'
    , liftOp_'
    , liftDiscard'

    -- ** Lifting morphisms
    , MonadLiftInvariant (hoistiso')
    , MonadLiftFunctor (hoist')
    )
where

-- base ----------------------------------------------------------------------
#if __GLASGOW_HASKELL__ >= 704
import           Control.Arrow (first)
#else
import           Control.Arrow ((***))
#endif
import           Control.Monad (join, liftM)
import           Data.Monoid (Monoid, mempty)
#if __GLASGOW_HASKELL__ < 707
import           GHC.Exts (Any)
#endif
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

The 'MonadTrans' family of interfaces consist of:

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
will get moved there some day soon. 'MonadTransControl' is more complicated:
there is a very similar class (the design of which I copied) defined in the
@<http://hackage.haskell.org/package/monad-control monad-control>@ package,
which is a relatively popular package. However,
@<http://hackage.haskell.org/package/layers layers>@' version has a few
important differences. It is conceivable that
@<http://hackage.haskell.org/package/monad-control monad-control>@ could
incorporate these changes some day, in which case
@<http://hackage.haskell.org/package/layers layers>@ could depend on
@<http://hackage.haskell.org/package/monad-control monad-control>@ and
'MonadTransControl' would be a re-export. This would be
the ideal scenario for @<http://hackage.haskell.org/package/layers layers>@.

-}

{-$liftfamily

The 'MonadLift' family of interfaces consist of:

    * 'MonadLift'

    * 'MonadLiftControl'

    * 'MonadLiftInvariant'

    * 'MonadLiftFunctor'

Each operation in the 'MonadLift' family of interfaces is an analogue of an
operation from the 'MonadTrans' family of interfaces. The naming of these
operations reflect this relationship.

(Note: The 'Lift', 'LiftResult' and 'LiftState' type synonyms and the 'peel'',
'restore'', 'suspend'', 'extract'' and 'result'' operations are only available
when compiled with GHC 7.8 and above. A different implementation is necessary
on older versions because of the lack of support for
<http://ghc.haskell.org/trac/ghc/wiki/NewAxioms/ClosedTypeFamilies closed type families>.)

-}

------------------------------------------------------------------------------
-- | The constraint @'MonadTransControl' t@ holds if @t@ is a monad
-- transformer through which control operations can be lifted.
--
-- There are several ways to lift control operations through monad
-- transformers, depending on the exact type of the operation to be lifted.
-- The thing that all \"control\" operations have in common is that they have
-- take a monadic argument somewhere in a contravariant position (otherwise
-- they could be lifted with just 'lift'). For example, let's say you have a
-- control operation @f :: m a -> m b@, and you want to make a lifted version
-- @f' :: t m a -> t m b@. @f'@ needs to somehow \"lower\" its argument from
-- @t m a@ to @m a@ so that it can be passed to the original @f@. We call this
-- \"peeling\": the @t@ layer is \"peeled\" off the monadic computation. This
-- is the essence of how 'MonadTransControl' works.
--
-- However, 'peel' is not (and cannot) be a simple operation @t m a -> m a@.
-- To see why, let's consider the 'RWST' monad transformer. It is defined like
-- this:
--
-- @
-- newtype 'RWST' r w s m a = 'RWST' { 'Control.Monad.Trans.RWS.Strict.runRWST' :: r -> s -> m (a, s, w) }
-- @
--
-- It wouldn't be possible to make an operation @'RWST' r w s m a -> m a@,
-- because 'RWST'​'s inner function needs an @r@ and an @s@ value before it can
-- return an @m a@. We call these types of values the 'LayerState' of a monad
-- transformer. 'LayerState' is an associated type synonym of the
-- 'MonadTransControl' class. In the 'MonadTransControl' instance for 'RWST':
--
-- @
-- type 'LayerState' ('RWST' r w s) m = (r, s)
-- @
--
-- So does that mean 'peel' has the type @t m a -> 'LayerState' t m -> m a@?
-- Not exactly. We don't want the computation in @m@ returned by 'peel' to
-- \"lose\" the effects in @t@ that the original @t m a@ computation had. And
-- for some monad transformers, we couldn't do this even if we wanted to.
-- Consider 'MaybeT':
--
-- @
-- newtype 'MaybeT' m a = { 'Control.Monad.Trans.Maybe.runMaybeT' :: m ('Maybe' a) }
-- @
-- 
-- If the value inside a 'MaybeT' is a 'Nothing', we can't get an @a@ out of
-- it. The closest to the proposed 'peel' above that we could get would be
-- @'MaybeT' m a -> 'LayerState' 'MaybeT' m -> m ('Maybe' a)@. Similarly, for
-- @'ErrorT' e@, the closest we could get would be
-- @'ErrorT' e m a -> 'LayerState' ('ErrorT' e) m -> m ('Either' e a)@. So, to
-- deal with this, 'MonadTransControl' has another associated type synonym
-- 'LayerResult'. Some examples of instances of 'LayerResult':
--
-- @
-- type 'LayerResult' 'MaybeT' = 'Maybe'
-- type 'LayerResult' ('ErrorT' e) = 'Either' e
-- type 'LayerResult' ('RWST' r w s) = (,) w
-- @
--
-- How exactly is it decided what the type of @'LayerResult' t@ for a
-- particular monad transformer should be? Well, for 'MaybeT' and 'ErrorT',
-- it's simple: a type function @:: * -> *@ such that, when applied to @a@, it
-- returns the type of everything inside the returned @m@ computation inside
-- the newtype wrapper. For example, given the definition of 'MaybeT' above,
-- @'LayerResult' 'MaybeT' = 'Maybe'@, because @a@ applied to 'Maybe' gives
-- @'Maybe' a@, which is the type that goes inside 'MaybeT'​'s @m (_)@.
--
-- For 'RWST', it's almost this, but it's a bit more complicated. The type
-- inside 'RWST'​'s @m (_)@ is @(a, s, w)@, but its 'LayerResult' type function
-- is @(,) w@, which, when applied to @a@, gives @(w, a)@. First of all,
-- @(w, a)@ is isomorphic to @(a, w)@, but because @'LayerResult' t@ has to
-- have a kind @* -> *@, and because it's impossible to write 'flip' at the
-- type level (without using newtypes), we settle for @(w, a)@. But what about
-- the @s@ in the middle? Well, that @s@ is actually part of the 'LayerState'
-- of 'RWST', not the 'LayerResult'. 'peel' takes this into account: the
-- computations in @m@ it returns return both a 'LayerResult' and an updated
-- 'LayerState'. The full type of 'peel' is thus:
--
-- @
-- 'peel' :: ('MonadTransControl' t, 'Monad' m) => t m a -> 'LayerState' t m -> m ('LayerResult' t a, 'LayerState' t m)
-- @
--
-- So really, 'MonadTransControl' is just the class of monad transformers
-- whose behaviour can be modeled by a function accepting some \"state\" value
-- and returning a computation in @m@ that produces a \"result\" and an
-- updated \"state\" value. 'peel' takes a computation @t m a@ and returns
-- such a function for @t@.
--
-- There are two other important operations in the 'MonadTransControl'
-- interface: 'suspend' and 'restore'.
--
-- @
-- 'suspend' :: ('MonadTransControl' t, 'Monad' m) => t m ('LayerState' t m)
-- 'restore' :: ('MonadTransControl' t, 'Monad' m) => ('LayerResult' t a, 'LayerState' t m) -> t m a
-- @
--
-- 'suspend' captures the current @'LayerState' t m@ for the monad @t m@. This
-- is passed to the function returned by 'peel'. 'restore' takes the
-- 'LayerResult' and updated 'LayerState' returned by that function and
-- recovers the monadic state in @t@ that they represent. Taken together, we
-- can use these operations to define @f'@, the lifted version of the @f@
-- operation we talked about earlier.
--
-- @
-- f' :: ('MonadTransControl' t, 'Monad' (t m), 'Monad' m) => t m a -> t m b
-- f' t = 'suspend' '>>=' 'lift' '.' f '.' 'peel' t '>>=' 'restore'
-- @
--
-- The full instance for 'RWST' is given below:
--
-- @
-- instance 'Monoid' w => 'MonadTransControl' ('RWST' r w s) where
--     type 'LayerResult' ('RWST' r w s) = (,) w
--     type 'LayerState' ('RWST' r w s) m = (r, s)
--     'peel' ('RWST' m) (r, s) = 'liftM' (\(a, s', w) -> ((w, a), (r, s'))) (m r s)
--     'restore' ((w, a), (_, s)) = 'RWST' '$' \_ _ -> 'return' (a, s, w)
--     'suspend' = 'RWST' '$' \r s -> 'return' ((r, s), s, 'mempty')
--     'extract' _ (_, a) = 'Just' a
-- @
class MonadTrans t => MonadTransControl t where
    -- | The portion of the result of executing a computation of @t@ that is
    -- independent of @m@ and which is not the new 'LayerState'.
    --
    -- Note: On versions of GHC prior to 7.4, 'LayerResult' is an associated
    -- /data/ type instead of an associated type synonym due to GHC bug
    -- <http://hackage.haskell.org/trac/ghc/ticket/5595 #5595>.
#if __GLASGOW_HASKELL__ >= 704
    type LayerResult t :: * -> *
#else
    data LayerResult t :: * -> *
#endif

    -- | The \"state\" needed to 'peel' a computation of @t@. Running a peeled
    -- computation returns a 'LayerResult' and an updated 'LayerState'.
    --
    -- Note: On versions of GHC prior to 7.4, 'LayerState' is an associated
    -- /data/ type instead of an associated type synonym due to GHC bug
    -- <http://hackage.haskell.org/trac/ghc/ticket/5595 #5595>.
#if __GLASGOW_HASKELL__ >= 704
    type LayerState t (m :: * -> *) :: *
#else
    data LayerState t :: (* -> *) -> *
#endif

    -- | 'peel' takes a computation in the monad @t m a@, and returns a
    -- function that takes a @'LayerState' t m@ (given by 'suspend') and
    -- returns a @'LayerResult' t a@ and an updated @'LayerState' t m@, which
    -- can then be 'restore'd back into @t m a@.
    --
    -- Instances should satisfy the following law:
    --
    -- [Preservation] @'suspend' '>>=' 'lift' '.' 'peel' t '>>=' 'restore' ≡ t@
    peel :: Monad m => t m a -> LayerState t m -> m (Layer t m a)

    -- | Reconstruct a @t m@ computation from the 'LayerResult' and
    -- 'LayerState' values returned from a use of 'peel'.
    --
    -- Instances should satisfy the following law:
    --
    -- [Preservation] @'suspend' '>>=' 'lift' '.' 'peel' t '>>=' 'restore' ≡ t@
    restore :: Monad m => Layer t m a -> t m a

    -- | Captures the current @'LayerState' t m@ of @t@ for the monad @t m@.
    -- This value is passed to the function returned by 'peel'.
    --
    -- [Preservation] @'suspend' '>>=' 'lift' '.' 'peel' t '>>=' 'restore' ≡ t@
    suspend :: Monad m => t m (LayerState t m)

    -- | 'extract' inspects a @'LayerResult' t a@ value and tries to
    -- \"extract\" an @a@ value from it, if possible. This can be used to
    -- detect if the monad transformer @t@ short-circuited when the
    -- 'LayerResult' was captured (if it did, 'extract' returns 'Nothing').
    -- This trick is used to implement the universal pass-through instance of
    -- 'Monad.Try.MonadTry'.
    --
    -- Instances should satisfy the following laws:
    --
    -- [Preservation]
    --     @'liftM' ('extract' ('Proxy' :: 'Proxy' t)) ('result' ('return' a))
    --         ≡ 'return' ('Just' a)@
    --
    -- [Zero]
    --     @('liftM' ('extract' ('Proxy' :: 'Proxy' t)) ('result' m)
    --         ≡ 'return' 'Nothing') ⇒ (m '>>=' f ≡ m)@
    extract :: proxy t -> LayerResult t a -> Maybe a


------------------------------------------------------------------------------
-- | A type synonym that makes some of the type signatures a little bit less
-- scary.
type Layer t m a = (LayerResult t a, LayerState t m)


------------------------------------------------------------------------------
instance Error e => MonadTransControl (ErrorT e) where
#if __GLASGOW_HASKELL__ >= 704
    type LayerResult (ErrorT e) = Either e
    type LayerState (ErrorT e) m = ()
    peel (ErrorT m) _ = liftM (\a -> (a, ())) m
    restore (a, _) = ErrorT $ return a
    suspend = return ()
    extract _ = either (const Nothing) Just
#else
    newtype LayerResult (ErrorT e) a = ER (Either e a)
    newtype LayerState (ErrorT e) m = ES ()
    peel (ErrorT m) _ = liftM (\a -> (ER a, ES ())) m
    restore (ER a, _) = ErrorT $ return a
    suspend = return (ES ())
    extract _ (ER e) = either (const Nothing) Just e
#endif


------------------------------------------------------------------------------
instance MonadTransControl IdentityT where
#if __GLASGOW_HASKELL__ >= 704
    type LayerResult IdentityT = Identity
    type LayerState IdentityT m = ()
    peel (IdentityT m) _ = liftM (\a -> (Identity a, ())) m
    restore (Identity a, _) = IdentityT $ return a
    suspend = return ()
    extract _ (Identity a) = Just a
#else
    newtype LayerResult IdentityT a = IR a
    newtype LayerState IdentityT m = IS ()
    peel (IdentityT m) _ = liftM (\a -> (IR a, IS ())) m
    restore (IR a, _) = IdentityT $ return a
    suspend = return (IS ())
    extract _ (IR a) = Just a
#endif


------------------------------------------------------------------------------
instance MonadTransControl ListT where
#if __GLASGOW_HASKELL__ >= 704
    type LayerResult ListT = []
    type LayerState ListT m = ()
    peel (ListT m) _ = liftM (\a -> (a, ())) m
    restore (a, _) = ListT $ return a
    suspend = return ()
    extract _ = foldr (const . Just) Nothing
#else
    newtype LayerResult ListT a = LR [a]
    newtype LayerState ListT m = LS ()
    peel (ListT m) _ = liftM (\a -> (LR a, LS ())) m
    restore (LR a, _) = ListT $ return a
    suspend = return (LS ())
    extract _ (LR xs) = foldr (const . Just) Nothing xs
#endif


------------------------------------------------------------------------------
instance MonadTransControl MaybeT where
#if __GLASGOW_HASKELL__ >= 704
    type LayerResult MaybeT = Maybe
    type LayerState MaybeT m = ()
    peel (MaybeT m) _ = liftM (\a -> (a, ())) m
    restore (a, _) = MaybeT $ return a
    suspend = return ()
    extract _ = id
#else
    newtype LayerResult MaybeT a = MR (Maybe a)
    newtype LayerState MaybeT m = MS ()
    peel (MaybeT m) _ = liftM (\a -> (MR a, MS ())) m
    restore (MR a, _) = MaybeT $ return a
    suspend = return (MS ())
    extract _ (MR a) = a
#endif


------------------------------------------------------------------------------
instance MonadTransControl (ReaderT r) where
#if __GLASGOW_HASKELL__ >= 704
    type LayerResult (ReaderT r) = Identity
    type LayerState (ReaderT r) m = r
    peel (ReaderT m) r = liftM (\a -> (Identity a, r)) (m r)
    restore (Identity a, _) = ReaderT $ \_ -> return a
    suspend = ReaderT $ \r -> return r
    extract _ (Identity a) = Just a
#else
    newtype LayerResult (ReaderT r) a = RR a
    newtype LayerState (ReaderT r) m = RS r
    peel (ReaderT m) (RS r) = liftM (\a -> (RR a, RS r)) (m r)
    restore (RR a, _) = ReaderT $ \_ -> return a
    suspend = ReaderT $ \r -> return (RS r)
    extract _ (RR a) = Just a
#endif


------------------------------------------------------------------------------
instance MonadTransControl (StateT s) where
#if __GLASGOW_HASKELL__ >= 704
    type LayerResult (StateT s) = Identity
    type LayerState (StateT s) m = s
    peel (StateT m) s = liftM (first Identity) (m s)
    restore (Identity a, s) = StateT $ \_ -> return (a, s)
    suspend = StateT $ \s -> return (s, s)
    extract _ (Identity a) = Just a
#else
    newtype LayerResult (StateT s) a = SR a
    newtype LayerState (StateT s) m = SS s
    peel (StateT m) (SS s) = liftM (SR *** SS) (m s)
    restore (SR a, SS s) = StateT $ \_ -> return (a, s)
    suspend = StateT $ \s -> return (SS s, s)
    extract _ (SR a) = Just a
#endif


------------------------------------------------------------------------------
instance MonadTransControl (L.StateT s) where
#if __GLASGOW_HASKELL__ >= 704
    type LayerResult (L.StateT s) = Identity
    type LayerState (L.StateT s) m = s
    peel (L.StateT m) s = liftM (first Identity) (m s)
    restore (Identity a, s) = L.StateT $ \_ -> return (a, s)
    suspend = L.StateT $ \s -> return (s, s)
    extract _ (Identity a) = Just a
#else
    newtype LayerResult (L.StateT s) a = SR' a
    newtype LayerState (L.StateT s) m = SS' s
    peel (L.StateT m) (SS' s) = liftM (SR' *** SS') (m s)
    restore (SR' a, SS' s) = L.StateT $ \_ -> return (a, s)
    suspend = L.StateT $ \s -> return (SS' s, s)
    extract _ (SR' a) = Just a
#endif


------------------------------------------------------------------------------
instance Monoid w => MonadTransControl (RWST r w s) where
#if __GLASGOW_HASKELL__ >= 704
    type LayerResult (RWST r w s) = (,) w
    type LayerState (RWST r w s) m = (r, s)
    peel (RWST m) (r, s) = liftM (\(a, s', w) -> ((w, a), (r, s'))) (m r s)
    restore ((w, a), (_, s)) = RWST $ \_ _ -> return (a, s, w)
    suspend = RWST $ \r s -> return ((r, s), s, mempty)
    extract _ (_, a) = Just a
#else
    newtype LayerResult (RWST r w s) a = RWSR (a, w)
    newtype LayerState (RWST r w s) m = RWSS (r, s)
    peel (RWST m) (RWSS (r, s)) =
        liftM (\(a, s', w) -> (RWSR (a, w), RWSS (r, s'))) (m r s)
    restore (RWSR (a, w),  RWSS (_, s)) = RWST $ \_ _ -> return (a, s, w)
    suspend = RWST $ \r s -> return (RWSS (r, s), s, mempty)
    extract _ (RWSR (a, _)) = Just a
#endif


------------------------------------------------------------------------------
instance Monoid w => MonadTransControl (L.RWST r w s) where
#if __GLASGOW_HASKELL__ >= 704
    type LayerResult (L.RWST r w s) = (,) w
    type LayerState (L.RWST r w s) m = (r, s)
    peel (L.RWST m) (r, s) = liftM (\(a, s', w) -> ((w, a), (r, s'))) (m r s)
    restore ((w, a), (_, s)) = L.RWST $ \_ _ -> return (a, s, w)
    suspend = L.RWST $ \r s -> return ((r, s), s, mempty)
    extract _ (_, a) = Just a
#else
    newtype LayerResult (L.RWST r w s) a = RWSR' (a, w)
    newtype LayerState (L.RWST r w s) m = RWSS' (r, s)
    peel (L.RWST m) (RWSS' (r, s)) =
        liftM (\(a, s', w) -> (RWSR' (a, w), RWSS' (r, s'))) (m r s)
    restore (RWSR' (a, w), RWSS' (_, s)) = L.RWST $ \_ _ -> return (a, s, w)
    suspend = L.RWST $ \r s -> return (RWSS' (r, s), s, mempty)
    extract _ (RWSR' (a, _)) = Just a
#endif


------------------------------------------------------------------------------
instance Monoid w => MonadTransControl (WriterT w) where
#if __GLASGOW_HASKELL__ >= 704
    type LayerResult (WriterT w) = (,) w
    type LayerState (WriterT w) m = ()
    peel (WriterT m) _ = liftM (\(a, w) -> ((w, a), ())) m
    restore ((w, a), _) = WriterT $ return (a, w)
    suspend = return ()
    extract _ (_, a) = Just a
#else
    newtype LayerResult (WriterT w) a = WR (a, w)
    newtype LayerState (WriterT w) m = WS ()
    peel (WriterT m) _ = liftM (\a -> (WR a, WS ())) m
    restore (WR a, _) = WriterT $ return a
    suspend = return (WS ())
    extract _ (WR (a, _)) = Just a
#endif


------------------------------------------------------------------------------
instance Monoid w => MonadTransControl (L.WriterT w) where
#if __GLASGOW_HASKELL__ >= 704
    type LayerResult (L.WriterT w) = (,) w
    type LayerState (L.WriterT w) m = ()
    peel (L.WriterT m) _ = liftM (\(a, w) -> ((w, a), ())) m
    restore ((w, a), _) = L.WriterT $ return (a, w)
    suspend = return ()
    extract _ (_, a) = Just a
#else
    newtype LayerResult (L.WriterT w) a = WR' (a, w)
    newtype LayerState (L.WriterT w) m = WS' ()
    peel (L.WriterT m) _ = liftM (\a -> (WR' a, WS' ())) m
    restore ((WR' a), _) = L.WriterT $ return a
    suspend = return (WS' ())
    extract _ (WR' (a, _)) = Just a
#endif


------------------------------------------------------------------------------
-- | Given a computation in @t m a@, return the @'LayerResult' t a@ given by
-- 'peel'ing off the @t@ layer.
--
-- In practice, this operation is probably not very useful, but it simplifies
-- the definition of the laws of 'extract'.
result :: forall t m a. (MonadTransControl t, Monad (t m), Monad m)
    => t m a
    -> t m (LayerResult t a)
result t = suspend >>= lift . liftM fst . peel t


------------------------------------------------------------------------------
-- | 'liftControl' is a composition of 'suspend', 'peel' and 'lift' provided
-- for convenience (and compability with
-- @<http://hackage.haskell.org/package/monad-control monad-control>@).
--
-- It takes a continuation, to which it passes a version of 'peel', which can
-- be used to effectively \"lower\" a computation in the monad @t m a@ to
-- @m a@ (storing the captured state of @t@ in the return value).
liftControl :: (MonadTransControl t, Monad (t m), Monad m)
    => ((forall b. t m b -> m (Layer t m b)) -> m a)
    -> t m a
liftControl f = suspend >>= \s -> lift $ f (flip peel s)
{-# INLINABLE liftControl #-}


------------------------------------------------------------------------------
-- | A version of 'liftControl' that automatically restores to the outer monad
-- the captured state returned from the continuation.
--
-- @
-- catch' :: ('Control.Exception.Exception' e, 'MonadTransControl' t, 'Monad' (t 'IO')) => t m b -> (e -> t m b) -> t m b
-- catch' m h = 'control' (\run -> 'Control.Exception.catch' (run m) (run '.' h))
-- @
control :: (MonadTransControl t, Monad (t m), Monad m)
    => ((forall b. t m b -> m (Layer t m b)) -> m (Layer t m a))
    -> t m a
control f = liftControl f >>= restore
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
    => ((a -> m (Layer t m b)) -> m (Layer t m c))
    -> (a -> t m b)
    -> t m c
liftOp f = \g -> control (\run -> f $ run . g)
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
    => (m (Layer t m a) -> m (Layer t m b))
    -> t m a
    -> t m b
liftOp_ f = \m -> control (\run -> f $ run m)
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
-- Note: While the computation (@t m ()@) passed to the resulting operation
-- has access to the 'LayerState' of @t@, it is run only for its side-effects
-- in @m@. Its side-effects in @t@ are discarded.
liftDiscard :: (MonadTransControl t, Monad (t m), Monad m)
    => (m () -> m a)
    -> t m ()
    -> t m a
liftDiscard f m = liftControl $ \run -> f $ liftM (const ()) $ run m
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
    -- @f@ and @g@ form a valid isomorphism, i.e., @f '.' g ≡ id@ and
    -- @g '.' f ≡ id@.
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
-- | The constraint @'MonadLift' i m@ holds when @i@ is an inner monad of @m@
-- such that it is possible to lift computations from @i@ into @m@ using
-- 'lift''. If @m@ is a monad built from a monad transformer stack, then it
-- supports lifting operations from any monad @i@ anywhere in the stack.
class (Monad i, Monad m) => MonadLift i m where
    -- | 'lift' takes a computation from an inner monad @i@ and lifts it into
    -- the \"outer\" monad @m@.
    --
    -- The following laws hold for valid instances of 'MonadLift':
    --
    --     [Identity] @'lift'' '.' 'return' ≡ 'return'@
    --
    --     [Composition] @'lift'' m '>>=' 'lift'' '.' f ≡ 'lift'' (m '>>=' f)@
    --
    -- The difference between 'lift'' and 'lift' is that 'lift' only lifts
    -- from the monad directly beneath the top of the stack, while 'lift'' can
    -- lift from /any/ monad anywhere in the stack (including @m@ itself).
    --
    -- If you know that you want to lift from the monad directly beneath the
    -- top of the stack, it's often better to use 'lift' than 'lift''. This
    -- improves type inference because 'lift' is less polymorphic than
    -- 'lift''. Similarly, you might also consider using
    -- 'Control.Monad.Lift.IO.liftIO' or 'Control.Monad.Lift.Base.liftBase'
    -- if you know that the monad from which you want to lift is 'IO' or the
    -- <Control-Monad-Lift-Base.html base monad> of a transformer stack.
    lift' :: Monad i => i a -> m a


------------------------------------------------------------------------------
instance Monad m => MonadLift m m where
    lift' = id


------------------------------------------------------------------------------
instance (Monad m, Monad (t m)) => MonadLift (t m) (t m) where
    lift' = id


------------------------------------------------------------------------------
instance (MonadTrans t, Monad (t m), MonadLift i m) => MonadLift i (t m) where
    lift' = lift . lift'
    {-# INLINE lift' #-}


------------------------------------------------------------------------------
-- | The constraint @'MonadLiftControl' i m@ holds when @i@ is an inner monad
-- of @m@ such that it is possible to lift control operations from @i@ to @m@
-- using 'liftControl''.
class MonadLift i m => MonadLiftControl i m where
    -- | Given the current \"state\" of the monad @m@ relative to @i@ (given
    -- by 'suspend''), 'peel'' unwraps the @m@ and returns the result and the
    -- new state of @m@ (relative to @i@) wrapped in @i@. 'liftControl'', a
    -- more often used operation, is defined in terms of 'peel'' and
    -- 'suspend''. See "Documentation.Layers.Overview" for more information.
    --
    -- Instances should satisfy thw following laws:
    --
    -- [Identity] @'liftControl'' . const . return ≡ return@
    --
    -- [Composition]
    --     @'liftControl'' (const m) >>= 'liftControl'' . const . f ≡
    --         'liftControl'' (const (m >>= f))@
    peel' :: m a -> LiftState i m -> i (Lift i m a)

    -- | Reconstruct an @m@ computation from the monadic state of @m@
    -- (realtive to @i@) that is returned from the 'peel'' operation.
    --
    -- Instances should satisfy the following law:
    --
    -- [Preservation]
    --     @'liftControl'' (\\run -> run t) >>= 'restore'' (Proxy :: Proxy i)
    --         ≡ t@
    restore' :: proxy i -> Lift i m a -> m a

    -- | Captures the current \"state\" of the monad @m@ relative to @i@. See
    -- the explanation in "Documentation.Layers.Overview" for what this
    -- actually means.
    suspend' :: proxy i -> m (LiftState i m)

    -- | 'extract'' inspects a @'LiftResult' i m a@ value and tries to
    -- \"extract\" an @a@ value from it, if possible. This can be used to
    -- detect if any of the monad layers betweem @i@ and @m@ short-circuited
    -- when the 'LiftResult' was captured (if they did, 'extract'' returns
    -- @Nothing@).
    --
    -- Instances should satisfy the following laws:
    --
    -- [Preservation]
    --     @'liftM' ('extract'' ('Proxy' :: 'Proxy' i) ('Proxy' :: 'Proxy' m))
    --         ('result'' ('Proxy' :: 'Proxy' i) ('return' a))
    --             ≡ 'return' ('Just' a)@
    --
    -- [Zero]
    --     @('liftM' ('extract'' ('Proxy' :: 'Proxy' i) ('Proxy' :: 'Proxy' m))
    --         ('result' ('Proxy' :: 'Proxy' i) m)
    --             ≡ 'return' 'Nothing') ⇒ (m '>>=' _ ≡ m)@
    extract' :: proxy i -> proxy' m -> LiftResult i m a -> Maybe a


------------------------------------------------------------------------------
-- | A type synonym that makes some of the type signatures a little bit less
-- scary.
type Lift i m a = (LiftResult i m a, LiftState i m)


------------------------------------------------------------------------------
-- | The portion of the result of executing a computation of @m@ relative to
-- @i@ that is independent of @m@ (and all layers down to @i@) and which is
-- not the new 'LiftState'.
type family LiftResult (i :: * -> *) (m :: * -> *) :: * -> *
#ifdef __GLASGOW_HASKELL__
#if __GLASGOW_HASKELL__ >= 707
  where
    LiftResult m m = Identity
    LiftResult i (t m) = ComposeResult i t m
#else
type instance LiftResult i m = Any'
#endif
#else
type LiftResult i m = Any'
#endif


#if __GLASGOW_HASKELL__ < 707
------------------------------------------------------------------------------
newtype Any' (a :: *) = Any' Any


#endif
------------------------------------------------------------------------------
-- | The \"state\" needed to 'peel'' a computation of @m@ back to @i@. Running
-- a peeled computation returns a 'LiftResult' and an updated 'LiftState'.
#ifdef __GLASGOW_HASKELL__
type family LiftState (i :: * -> *) (m :: * -> *) :: *
#if __GLASGOW_HASKELL__ >= 707
  where
    LiftState m m = ()
    LiftState i (t m) = (LayerState t m, LiftState i m)
#else
type instance LiftState i m = Any
#endif
#else
type LiftState i m = Any
#endif


------------------------------------------------------------------------------
newtype ComposeResult i t m a
    = ComposeResult (LiftResult i m (LayerResult t a, LayerState t m))


------------------------------------------------------------------------------
#if __GLASGOW_HASKELL__ >= 707
coerce :: a -> a
coerce = id
#else
coerce :: a -> b
coerce = unsafeCoerce
#endif
{-# INLINE coerce #-}


------------------------------------------------------------------------------
instance MonadLift m m => MonadLiftControl m m where
    peel' m _ = liftM (\a -> (coerce $ Identity a, coerce ())) m
    restore' _ (r, _) = let Identity a = coerce r in return a
    suspend' _ = return $ coerce ()
    extract' _ _ r = let Identity a = coerce r in Just a


------------------------------------------------------------------------------
instance (Monad m, MonadLift (t m) (t m)) => MonadLiftControl (t m) (t m)
  where
    peel' m _ = liftM (\a -> (coerce $ Identity a, coerce ())) m
    restore' _ (r, _) = let Identity a = coerce r in return a
    suspend' _ = return $ coerce ()
    extract' _ _ r = let Identity a = coerce r in Just a


------------------------------------------------------------------------------
instance
    ( MonadTransControl t
    , Monad (t m)
    , MonadLift i (t m)
    , MonadLiftControl i m
#if __GLASGOW_HASKELL__ >= 707
    , LiftResult i (t m) ~ ComposeResult i t m
    , LiftState i (t m) ~ (LayerState t m, LiftState i m)
#endif
    )
  =>
    MonadLiftControl i (t m)
  where
    peel' (m :: t m a) s = do
        let (lys, lis) = coerce s
        let compose lir = ComposeResult lir :: ComposeResult i t m a
        let f (lir, lis') = (coerce (compose lir), coerce (lys, lis'))
        liftM f $ peel' (peel m lys) lis
    {-# INLINE peel' #-}

    restore' p ((r, s) :: Lift i (t m) a) = do
        let ComposeResult r' = (coerce r :: ComposeResult i t m a)
        let (_, s') = coerce s
        lift (restore' p (r', s')) >>= restore
    {-# INLINE restore' #-}

    suspend' p = suspend >>= \a -> lift (suspend' p) >>= \b ->
        return $ coerce (a, b)
    {-# INLINE suspend' #-}

    extract' _ _ (r :: LiftResult i (t m) a) =
        let ComposeResult r' = (coerce r :: ComposeResult i t m a) in join $
            fmap (extract (Pt :: Pt t) . fst) $
                extract' (Pm :: Pm i) (Pm :: Pm m) r'
    {-# INLINE extract' #-}


------------------------------------------------------------------------------
data Pm (m :: * -> *) = Pm


------------------------------------------------------------------------------
data Pt (t :: (* -> *) -> * -> *) = Pt


------------------------------------------------------------------------------
-- | Given a computation in @m a@, return the @'LiftResult' i m a@ given by
-- 'peel'ing off all of the layers between @m@ and @i@.
--
-- In practice, this operation is probably not very useful, but it simplifies
-- the definition of the laws of 'extract''.
result' :: forall proxy m i a. MonadLiftControl i m
    => proxy i
    -> m a
    -> m (LiftResult i m a)
result' p m
    = suspend' p >>= \s -> lift' (liftM fst (peel' m s :: i (Lift i m a)))


------------------------------------------------------------------------------
-- | 'liftControl'' is a version of 'lift'' that makes it possible to lift
-- control operations from the inner monad @i@ to the outer monad @m@. It
-- takes a continuation, to which it passes a version of 'peel'', which is
-- kind of an \"inverse\" of 'lift''.
--
-- The difference between 'liftControl'' and 'liftControl' is that
-- 'liftControl' only lifts from the monad directly beneath the top of the
-- stack, while 'liftControl'' can lift from /any/ monad anywhere in the stack
-- (including @m@ itself).
--
-- If you know that you want to lift from the monad directly beneath the top
-- of the stack, it's often better to use 'liftControl' than 'liftControl''.
-- This improves type inference because 'liftControl' is less polymorphic than
-- 'liftControl''. Similarly, you might also consider using
-- 'Control.Monad.Lift.IO.liftControlIO' or
-- 'Control.Monad.Lift.Base.liftBaseControl' if you know that the monad from
-- which you want to lift is 'IO' or the <Control-Monad-Lift-Base.html base>
-- monad of a transformer stack.
liftControl' :: forall i m a. MonadLiftControl i m
    => ((forall b. m b -> i (Lift i m b)) -> i a)
    -> m a
liftControl' f = suspend' (Pm :: Pm i) >>= \s -> lift' $
    f (flip peel' s)
{-# INLINABLE liftControl' #-}


------------------------------------------------------------------------------
-- | A version of 'liftControl'' that automatically restores to the outer
-- monad the captured state returned from the continuation.
--
-- @
-- catch' :: ('Control.Exception.Exception' e, 'MonadLiftControl' 'IO' m) => m b -> (e -> m b) -> m b
-- catch' m h = 'control'' (\run -> 'Control.Exception.catch' (run m) (run '.' h))
-- @
--
-- The difference between 'control'' and 'control' is that 'control' only
-- lifts from the monad directly beneath the top of the stack, while
-- 'control'' can lift from /any/ monad anywhere in the stack (including @m@
-- itself).
--
-- If you know that you want to lift from the monad directly beneath the top
-- of the stack, it's often better to use 'control' than 'control''. This
-- improves type inference because 'control' is less polymorphic than
-- 'control''. Similarly, you might also consider using
-- 'Control.Monad.Lift.IO.controlIO' or 'Control.Monad.Lift.Base.controlBase'
-- if you know that the monad from which you want to lift is 'IO' or the
-- <Control-Monad-Lift-Base.html base monad> of a transformer stack.

control' :: forall i m a. MonadLiftControl i m
    => ((forall b. m b -> i (Lift i m b)) -> i (Lift i m a))
    -> m a
control' f = (liftControl' f :: m (Lift i m a)) >>= restore' (Pm :: Pm i)


------------------------------------------------------------------------------
-- | A particular application of 'liftControl'' that lifts control operations
-- from @(a -> i b) -> i b@ to @(a -> m b) -> m b@.
--
-- @
-- withMVar' :: 'MonadLiftControl' 'IO' m => 'Control.Concurrent.MVar.MVar' a -> (a -> m b) -> m b
-- withMVar' = 'liftOp'' '.' 'Control.Concurrent.MVar.withMVar'
-- @
--
-- The difference between 'liftOp'' and 'liftOp' is that 'liftOp' only lifts
-- from the monad directly beneath the top of the stack, while 'liftOp''
-- can lift from /any/ monad anywhere in the stack (including @m@ itself).
--
-- If you know that you want to lift from the monad directly beneath the top
-- of the stack, it's often better to use 'liftOp' than 'liftOp''. This
-- improves type inference because 'liftOp' is less polymorphic than
-- 'liftOp''. Similarly, you might also consider using
-- 'Control.Monad.Lift.IO.liftIOOp' or 'Control.Monad.Lift.Base.liftBaseOp'
-- if you know that the monad from which you want to lift is 'IO' or the
-- <Control-Monad-Lift-Base.html base monad> of a transformer stack.
liftOp' :: MonadLiftControl i m
     => ((a -> i (Lift i m b)) -> i (Lift i m c))
     -> (a -> m b)
     -> m c
liftOp' f = \g -> control' $ \run -> f $ run . g
{-# INLINABLE liftOp' #-}


------------------------------------------------------------------------------
-- | A particular application of 'liftControl'' that lifts control operations
-- from @i a -> i b@ to @m a -> m b@.
--
-- @
-- mask_' :: 'MonadLiftControl' 'IO' m => m a -> m a
-- mask_' = 'liftOp_'' 'Control.Exception.mask_'
-- @
--
-- The difference between 'liftOp_'' and 'liftOp_' is that 'liftOp_' only
-- lifts from the monad directly beneath the top of the stack, while
-- 'liftOp_'' can lift from /any/ monad anywhere in the stack (including @m@
-- itself).
--
-- If you know that you want to lift from the monad directly beneath the top
-- of the stack, it's often better to use 'liftOp_' than 'liftOp_''. This
-- improves type inference because 'liftOp_' is less polymorphic than
-- 'liftOp_''. Similarly, you might also consider using
-- 'Control.Monad.Lift.IO.liftIOOp_' or 'Control.Monad.Lift.Base.liftBaseOp_'
-- if you know that the monad from which you want to lift is 'IO' or the
-- <Control-Monad-Lift-Base.html base monad> of a transformer stack.
liftOp_' :: MonadLiftControl i m
    => (i (Lift i m a) -> i (Lift i m b))
     -> m a
     -> m b
liftOp_' f = \m -> control' $ \run -> f $ run m
{-# INLINABLE liftOp_' #-}


------------------------------------------------------------------------------
-- | A particular application of 'liftControl'' that lifts control operations
-- from @i () -> i a@ to @m () -> m a@.
--
-- @
-- forkIO' :: 'MonadLiftControl' 'IO' m => m () -> m 'Control.Concurrent.ThreadId'
-- forkIO' = 'liftDiscard'' 'Control.Concurrent.forkIO'
-- @
--
-- The difference between 'liftDiscard'' and 'liftDiscard' is that
-- 'liftDiscard' only lifts from the monad directly beneath the top of the
-- stack, while 'liftDiscard'' can lift from /any/ monad anywhere in the stack
-- (including @m@ itself).
--
-- If you know that you want to lift from the monad directly beneath the top
-- of the stack, it's often better to use 'liftDiscard' than 'liftDiscard''.
-- This improves type inference because 'liftDiscard' is less polymorphic than
-- 'liftDiscard''. Similarly, you might also consider using
-- 'Control.Monad.Lift.IO.liftIODiscard' or
-- 'Control.Monad.Lift.Base.liftBaseDiscard' if you know that the monad from
-- which you want to lift is 'IO' or the <Control-Monad-Lift-Base.html base>
-- monad of a transformer stack.
--
-- Note: While the computation (@m ()@) passed to the resulting operation has
-- access to the @'LiftState' i@ of @m@, it is run only for its side-effects
-- in @i@. Its side-effects in @m@ are discarded.
liftDiscard' :: MonadLiftControl i m => (i () -> i a) -> m () -> m a
liftDiscard' f = \m -> liftControl' $ \run -> f $ liftM (const ()) $ run m
{-# INLINABLE liftDiscard' #-}


------------------------------------------------------------------------------
-- | The constraint @'MonadLiftInvariant' i m@ holds when @i@ is an inner
-- monad of @m@ such that it is possible to lift monad automorphisms of @i@ to
-- monad endomorphisms of @m@ using 'hoistiso''.
class Monad i => MonadLiftInvariant i m where
    -- | 'hoistiso'' represents an invariant endofunctor in the category of
    -- monads. It takes a transformation @f@ of an inner monad @i@ and its
    -- inverse @g@ (such that @g . f = id@) and returns transformation of @m@
    -- analogous to @f@. (i.e., @hoistiso'@ lifts an automorphism in @i@
    -- to an endomorphism in @m@).
    --
    -- The following laws hold for valid instances of 'MonadLiftInvariant':
    --
    --     [Identity] @'hoistiso'' 'id' 'id' ≡ 'id'@
    --
    --     [Composition]
    --         @'hoistiso'' f g '.' 'hoistiso'' f' g' ≡
    --             'hoistiso'' (f '.' f') (g' '.' g)@
    --
    -- Note: The endomorphism produced by @'hoistiso'' f g@ is only valid if
    -- @f@ and @g@ form a valid isomorphism, i.e., @f '.' g ≡ id@ and
    -- @g '.' f ≡ id@.
    --
    -- There are two main differences between 'hoistiso'' and 'hoistiso'. The
    -- first is that 'hoistiso' only lifts from the monad directly beneath the
    -- top of the stack, while 'hoistiso'' can lift from /any/ monad anywhere
    -- in the stack (including @m@ itself). The second is that 'hoistiso'' can
    -- only accept an automorphism (producing an endomorphism), while
    -- 'hoistiso' can accept any isomorphism, producing a homomorphism. In
    -- other words, the morphism passed to 'hoistiso' can be used to change
    -- the type of the inner monad, but this is not possible with 'hoistiso''.
    --
    -- If you know that you want to lift from the monad directly beneath the
    -- top of the stack, it's often better to use 'hoistiso' than 'hoistiso''.
    -- This improves type inference because 'hoistiso' is less polymorphic
    -- than 'hoistiso''.  Similarly, you might also consider using
    -- 'Control.Monad.Lift.IO.hoistisoIO' or
    -- 'Control.Monad.Lift.Base.hoistisoBase' if you know that the monad from
    -- which you want to lift is 'IO' or the
    -- <Control-Monad-Lift-Base.html base monad> of a transformer stack.
    hoistiso'
        :: (forall b. i b -> i b)
        -> (forall b. i b -> i b)
        -> m a
        -> m a


------------------------------------------------------------------------------
instance Monad m => MonadLiftInvariant m m where
    hoistiso' f _ = f


------------------------------------------------------------------------------
instance (Monad m, Monad (t m)) => MonadLiftInvariant (t m) (t m) where
    hoistiso' f _ = f


------------------------------------------------------------------------------
instance (MInvariant t, Monad m, MonadLiftInvariant i m) =>
    MonadLiftInvariant i (t m)
  where
    hoistiso' f g = hoistiso (hoistiso' f g) (hoistiso' g f)
    {-# INLINE hoistiso' #-}


------------------------------------------------------------------------------
-- | The constraint @'MonadLiftFunctor' i m@ holds when @i@ is an inner monad
-- of @m@ such that it is possible to lift monad morphisms of @i@ to monad
-- morphisms of @m@ using 'hoist''. 'hoist'' is more powerful than
-- 'hoistiso'' because 'hoist'' can lift morphisms which do not have an
-- inverse, while 'hoistiso'' can only lift isomorphisms.
class MonadLiftInvariant i m => MonadLiftFunctor i m where
    -- | 'hoist'' represents an endofunctor in the category of monads. It
    -- takes a transformation @f@ of an inner monad @i@ returns a
    -- transformation of @m@ analogous to @f@. (i.e., @hoist@ lifts an
    -- endomorphism of @i@ to an endomorphism of @m@).
    --
    -- The following laws hold for valid instances of 'MonadLiftFunctor':
    --
    --     [Identity] @'hoist'' 'id' ≡ 'id'@
    --
    --     [Composition] @'hoist'' f '.' 'hoist'' g ≡ 'hoist'' (f '.' g)@
    --
    -- There are two main differences between 'hoist'' and 'hoist'. The first
    -- is that 'hoist' only lifts from the monad directly beneath the top of
    -- the stack, while 'hoist'' can lift from /any/ monad anywhere in the
    -- stack (including @m@ itself). The second is that 'hoist'' can only
    -- accept an endomorphism, while 'hoist' can accept any homomorphism. In
    -- other words, the morphism passed to 'hoist' can be used to change the
    -- type of the inner monad, but this is not possible with 'hoist''.
    --
    -- If you know that you want to lift from the monad directly beneath the
    -- top of the stack, it's often better to use 'hoist' than 'hoist''. This
    -- improves type inference because 'hoist' is less polymorphic than
    -- 'hoist''. Similarly, you might also consider using
    -- 'Control.Monad.Lift.IO.hoistIO' or 'Control.Monad.Lift.Base.hoistBase'
    -- if you know that the monad from which you want to lift is 'IO' or the
    -- <Control-Monad-Lift-Base.html base monad> of a transformer stack.
    hoist' :: (forall b. i b -> i b) -> m a -> m a


------------------------------------------------------------------------------
instance MonadLiftInvariant m m => MonadLiftFunctor m m where
    hoist' f = f


------------------------------------------------------------------------------
instance (Monad m, MonadLiftInvariant (t m) (t m)) =>
    MonadLiftFunctor (t m) (t m)
  where
    hoist' f = f


------------------------------------------------------------------------------
instance
    ( MInvariant t
    , MFunctor t
    , Monad m
    , MonadLiftInvariant i (t m)
    , MonadLiftFunctor i m
    )
  =>
    MonadLiftFunctor i (t m)
  where
    hoist' f = hoist (hoist' f)
    {-# INLINE hoist' #-}
