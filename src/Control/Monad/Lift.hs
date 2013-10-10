{-# LANGUAGE CPP #-}
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

{-# OPTIONS_GHC -fno-warn-orphans #-}

{-|

This module is the core of the @layers@ package. It exports everything you
need for to implement monad transformers that take advantage of @layers@'
machinery (including re-exports from @transformers@ and @mmorph@). It exports:

    1. The 'MonadTrans' class and its operation 'lift'. This is re-exported
    from @transformers@.

    2. The 'MonadTransControl' class, its associated types 'LayerState' and
    'LayerResult', and its operations 'peel', 'suspend', 'restore', 'extract'.
    Also, the convenience operations 'liftControl', 'control', 'liftOp',
    'liftOp_' and 'liftDiscard'. These are inspired by similarly named
    functions from the @monad-control@ package.

    3. The 'MInvariant', 'MFunctor' and 'MMonad' type classes and their
    respective operations 'hoistiso', 'hoist' and 'embed', for lifting
    morphisms in the category of monads through monad transformers. Currently,
    @MInvariant@ is defined by @layers@, but I'm
    <https://github.com/Gabriel439/Haskell-MMorph-Library/pull/1 hoping> to
    get it moved to @mmorph@ where the other classes are defined.

It also defines the @'MonadLift'*@ family of interfaces, which consist of:

    * 'MonadLift' :: @(* -> *) -> (* -> *) -> Constraint@

    * 'MonadLiftControl' :: @(* -> *) -> (* -> *) -> Constraint@

    * 'MonadLiftInvariant' :: @(* -> *) -> (* -> *) -> Constraint@

    * 'MonadLiftFunctor' :: @(* -> *) -> (* -> *) -> Constraint@

    * 'lift'' :: @MonadLift i m => i a -> m a@

    * 'liftControl'' :: @MonadLiftControl i m =>
        ((forall b. m b -> i (m b)) -> i a) -> m a@

    * 'control'' :: @MonadLiftControl i m =>
        ((forall b. m b -> i (m b)) -> i (m a)) -> m a@

    * 'liftOp'' :: @MonadLiftControl i m => ((a -> i (m b)) -> i (m c)) ->
        (a -> m b) -> m c@

    * 'liftOp_'' :: @MonadLiftControl i m => (i (m a) -> i (m b)) -> m a ->
        m b@

    * 'liftDiscard'' :: @MonadLiftControl i m => (i () -> i a) -> m () -> m a@

    * 'hoistiso'' :: @MonadLiftInvariant i m => (forall b. i b -> i b) ->
        (forall b. i b -> i b) -> m a -> m a@

    * 'hoist'' :: @MonadLiftFunctor i m => (forall b. i b -> i b) -> m a ->
        m a@

Each operation in the @'MonadLift'*@ family of interfaces is an analogue of an
operation from the @MonadTrans*@/@MMorph@ family of interfaces. The naming of
these operations reflect this relationship.

-}

module Control.Monad.Lift
    (
    -- * The @MonadTrans@/@MMorph@ family
      MonadTrans
    , lift
    , MonadTransControl
    , LayerState
    , LayerResult
    , peel
    , suspend
    , restore
    , extract
    , liftControl
    , control
    , liftOp
    , liftOp_
    , liftDiscard

    -- ** Monad morphisms
    , MInvariant
    , hoistiso
    , MFunctor
    , hoist
    , MMonad
    , embed

    -- * The @MonadLift@ family
    , MonadLift
    , lift'
    , MonadLiftControl
#if __GLASGOW_HASKELL__ >= 707
    , LiftState
    , LiftResult
    , peel'
    , suspend'
    , restore'
#endif
    , liftControl'
    , control'
    , liftOp'
    , liftOp_'
    , liftDiscard'

    -- ** Monad lift morphisms
    , MonadLiftInvariant
    , hoistiso'
    , MonadLiftFunctor
    , hoist'
    )
where

-- base ----------------------------------------------------------------------
import           Control.Arrow (first)
#if __GLASGOW_HASKELL__ < 704
import           Control.Arrow ((***))
#endif
import           Control.Monad (liftM)
#if __GLASGOW_HASKELL__ < 707
import           Control.Monad (join)
#endif
import           Data.Monoid (Monoid, mempty)


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
import           Control.Monad.Morph (MFunctor (hoist), MMonad (embed))


------------------------------------------------------------------------------
-- | 'MonadTransControl' represents the class of monad transformers through
-- which it is possible to lift control operations. See
-- "Documentation.Layers.Overview" for a more complete discussion of how it
-- works.
class MonadTrans t => MonadTransControl t where
    -- | The portion of the result of executing a computation of @t@ that is
    -- independent of @m@ and which is not the new 'LayerState'.
#if __GLASGOW_HASKELL__ >= 704
    type LayerResult t :: * -> *
#else
    data LayerResult t :: * -> *
#endif

    -- | The \"state\" needed to 'peel' a computation of @t@. Running a peeled
    -- computation returns a 'LayerResult' and an updated 'LayerState'.
#if __GLASGOW_HASKELL__ >= 704
    type LayerState t (m :: * -> *) :: *
#else
    data LayerState t :: (* -> *) -> *
#endif

    -- | Given the current \"state\" of the monad transformer @t@, 'peel'
    -- unwraps the @t@ and returns the result and the new state of @t@.
    -- 'liftControl', a more often used operation, is defined in terms of
    -- @peel@ and 'suspend'. See "Documentation.Layers.Overview" for more
    -- information.
    --
    -- Instances should satisfy similar laws as the monad transformer laws:
    --
    -- [Identity] @liftControl . const . return = return@
    --
    -- [Composition]
    --     @liftControl (const m) >>= liftControl . const . f@ = @liftControl (const (m >>= f))@
    peel :: Monad m
        => LayerState t m
        -> t m a
        -> m (LayerResult t a, LayerState t m)


    -- | Construct a @t m@ computation from the monadic state of @m@ that is
    -- returned from the 'peel' operation.
    --
    -- Instances should satisfy the following law:
    --
    -- [Preservation] @liftControl (\\run -> run t) >>= uncurry restore = t@
    restore :: Monad m => LayerResult t a -> LayerState t m -> t m a

    -- | Captures the current \"state\" of the monad transformer @t@. See the
    -- explanation in "Documentation.Layers.Overview" for what this actually
    -- means.
    suspend :: Monad m => t m (LayerState t m)

    -- | 'extract' inspects a @'LayerResult' t a@ value and tries to
    -- \"extract\" an @a@ value from it, if possible. This can be used to
    -- detect if the monad transformer @t@ has \"short-circuited\". This trick
    -- is used to implement the universal pass-through instance of
    -- 'Control.Monad.Interface.Try.MonadTry'.
    extract :: proxy t -> LayerResult t a -> Maybe a


------------------------------------------------------------------------------
instance Error e => MonadTransControl (ErrorT e) where
#if __GLASGOW_HASKELL__ >= 704
    type LayerResult (ErrorT e) = Either e
    type LayerState (ErrorT e) m = ()
    peel _ (ErrorT m) = liftM (\a -> (a, ())) m
    restore a _ = ErrorT $ return a
    suspend = return ()
    extract _ = either (const Nothing) Just
#else
    newtype LayerResult (ErrorT e) a = ER (Either e a)
    newtype LayerState (ErrorT e) m = ES ()
    peel _ (ErrorT m) = liftM (\a -> (ER a, ES ())) m
    restore (ER a) _ = ErrorT $ return a
    suspend = return (ES ())
    extract _ (ER e) = either (const Nothing) Just e
#endif


------------------------------------------------------------------------------
instance MonadTransControl IdentityT where
#if __GLASGOW_HASKELL__ >= 704
    type LayerResult IdentityT = Identity
    type LayerState IdentityT m = ()
    peel _ (IdentityT m) = liftM (\a -> (Identity a, ())) m
    restore (Identity a) _ = IdentityT $ return a
    suspend = return ()
    extract _ (Identity a) = Just a
#else
    newtype LayerResult IdentityT a = IR a
    newtype LayerState IdentityT m = IS ()
    peel _ (IdentityT m) = liftM (\a -> (IR a, IS ())) m
    restore (IR a) _ = IdentityT $ return a
    suspend = return (IS ())
    extract _ (IR a) = Just a
#endif


------------------------------------------------------------------------------
instance MonadTransControl ListT where
#if __GLASGOW_HASKELL__ >= 704
    type LayerResult ListT = []
    type LayerState ListT m = ()
    peel _ (ListT m) = liftM (\a -> (a, ())) m
    restore a _ = ListT $ return a
    suspend = return ()
    extract _ = foldr (const . Just) Nothing
#else
    newtype LayerResult ListT a = LR [a]
    newtype LayerState ListT m = LS ()
    peel _ (ListT m) = liftM (\a -> (LR a, LS ())) m
    restore (LR a) _ = ListT $ return a
    suspend = return (LS ())
    extract _ (LR xs) = foldr (const . Just) Nothing xs
#endif


------------------------------------------------------------------------------
instance MonadTransControl MaybeT where
#if __GLASGOW_HASKELL__ >= 704
    type LayerResult MaybeT = Maybe
    type LayerState MaybeT m = ()
    peel _ (MaybeT m) = liftM (\a -> (a, ())) m
    restore a _ = MaybeT $ return a
    suspend = return ()
    extract _ = id
#else
    newtype LayerResult MaybeT a = MR (Maybe a)
    newtype LayerState MaybeT m = MS ()
    peel _ (MaybeT m) = liftM (\a -> (MR a, MS ())) m
    restore (MR a) _ = MaybeT $ return a
    suspend = return (MS ())
    extract _ (MR a) = a
#endif


------------------------------------------------------------------------------
instance MonadTransControl (ReaderT r) where
#if __GLASGOW_HASKELL__ >= 704
    type LayerResult (ReaderT r) = Identity
    type LayerState (ReaderT r) m = r
    peel r (ReaderT m) = liftM (\a -> (Identity a, r)) (m r)
    restore (Identity a) _ = ReaderT $ \_ -> return a
    suspend = ReaderT $ \r -> return r
    extract _ (Identity a) = Just a
#else
    newtype LayerResult (ReaderT r) a = RR a
    newtype LayerState (ReaderT r) m = RS r
    peel (RS r) (ReaderT m) = liftM (\a -> (RR a, RS r)) (m r)
    restore (RR a) _ = ReaderT $ \_ -> return a
    suspend = ReaderT $ \r -> return (RS r)
    extract _ (RR a) = Just a
#endif


------------------------------------------------------------------------------
instance MonadTransControl (StateT s) where
#if __GLASGOW_HASKELL__ >= 704
    type LayerResult (StateT s) = Identity
    type LayerState (StateT s) m = s
    peel s (StateT m) = liftM (first Identity) (m s)
    restore (Identity a) s = StateT $ \_ -> return (a, s)
    suspend = StateT $ \s -> return (s, s)
    extract _ (Identity a) = Just a
#else
    newtype LayerResult (StateT s) a = SR a
    newtype LayerState (StateT s) m = SS s
    peel (SS s) (StateT m) = liftM (SR *** SS) (m s)
    restore (SR a) (SS s) = StateT $ \_ -> return (a, s)
    suspend = StateT $ \s -> return (SS s, s)
    extract _ (SR a) = Just a
#endif


------------------------------------------------------------------------------
instance MonadTransControl (L.StateT s) where
#if __GLASGOW_HASKELL__ >= 704
    type LayerResult (L.StateT s) = Identity
    type LayerState (L.StateT s) m = s
    peel s (L.StateT m) = liftM (first Identity) (m s)
    restore (Identity a) s = L.StateT $ \_ -> return (a, s)
    suspend = L.StateT $ \s -> return (s, s)
    extract _ (Identity a) = Just a
#else
    newtype LayerResult (L.StateT s) a = SR' a
    newtype LayerState (L.StateT s) m = SS' s
    peel (SS' s) (L.StateT m) = liftM (SR' *** SS') (m s)
    restore (SR' a) (SS' s) = L.StateT $ \_ -> return (a, s)
    suspend = L.StateT $ \s -> return (SS' s, s)
    extract _ (SR' a) = Just a
#endif


------------------------------------------------------------------------------
instance Monoid w => MonadTransControl (RWST r w s) where
#if __GLASGOW_HASKELL__ >= 704
    type LayerResult (RWST r w s) = (,) w
    type LayerState (RWST r w s) m = (r, s)
    peel (r, s) (RWST m) = liftM (\(a, s', w) -> ((w, a), (r, s'))) (m r s)
    restore (w, a) (_, s) = RWST $ \_ _ -> return (a, s, w)
    suspend = RWST $ \r s -> return ((r, s), s, mempty)
    extract _ (_, a) = Just a
#else
    newtype LayerResult (RWST r w s) a = RWSR (a, w)
    newtype LayerState (RWST r w s) m = RWSS (r, s)
    peel (RWSS (r, s)) (RWST m) =
        liftM (\(a, s', w) -> (RWSR (a, w), RWSS (r, s'))) (m r s)
    restore (RWSR (a, w)) (RWSS (_, s)) = RWST $ \_ _ -> return (a, s, w)
    suspend = RWST $ \r s -> return (RWSS (r, s), s, mempty)
    extract _ (RWSR (a, w)) = Just a
#endif


------------------------------------------------------------------------------
instance Monoid w => MonadTransControl (L.RWST r w s) where
#if __GLASGOW_HASKELL__ >= 704
    type LayerResult (L.RWST r w s) = (,) w
    type LayerState (L.RWST r w s) m = (r, s)
    peel (r, s) (L.RWST m) = liftM (\(a, s', w) -> ((w, a), (r, s'))) (m r s)
    restore (w, a) (_, s) = L.RWST $ \_ _ -> return (a, s, w)
    suspend = L.RWST $ \r s -> return ((r, s), s, mempty)
    extract _ (_, a) = Just a
#else
    newtype LayerResult (L.RWST r w s) a = RWSR' (a, w)
    newtype LayerState (L.RWST r w s) m = RWSS' (r, s)
    peel (RWSS' (r, s)) (L.RWST m) =
        liftM (\(a, s', w) -> (RWSR' (a, w), RWSS' (r, s'))) (m r s)
    restore (RWSR' (a, w)) (RWSS' (_, s)) = L.RWST $ \_ _ -> return (a, s, w)
    suspend = L.RWST $ \r s -> return (RWSS' (r, s), s, mempty)
    extract _ (RWSR' (a, w)) = Just a
#endif


------------------------------------------------------------------------------
instance Monoid w => MonadTransControl (WriterT w) where
#if __GLASGOW_HASKELL__ >= 704
    type LayerResult (WriterT w) = (,) w
    type LayerState (WriterT w) m = ()
    peel _ (WriterT m) = liftM (\(a, w) -> ((w, a), ())) m
    restore (w, a) _ = WriterT $ return (a, w)
    suspend = return ()
    extract _ (_, a) = Just a
#else
    newtype LayerResult (WriterT w) a = WR (a, w)
    newtype LayerState (WriterT w) m = WS ()
    peel _ (WriterT m) = liftM (\a -> (WR a, WS ())) m
    restore (WR a) _ = WriterT $ return a
    suspend = return (WS ())
    extract _ (WR (a, _)) = Just a
#endif


------------------------------------------------------------------------------
instance Monoid w => MonadTransControl (L.WriterT w) where
#if __GLASGOW_HASKELL__ >= 704
    type LayerResult (L.WriterT w) = (,) w
    type LayerState (L.WriterT w) m = ()
    peel _ (L.WriterT m) = liftM (\(a, w) -> ((w, a), ())) m
    restore (w, a) _ = L.WriterT $ return (a, w)
    suspend = return ()
    extract _ (_, a) = Just a
#else
    newtype LayerResult (L.WriterT w) a = WR' (a, w)
    newtype LayerState (L.WriterT w) m = WS' ()
    peel _ (L.WriterT m) = liftM (\a -> (WR' a, WS' ())) m
    restore (WR' a) _ = L.WriterT $ return a
    suspend = return (WS' ())
    extract _ (WR' (a, _)) = Just a
#endif


------------------------------------------------------------------------------
-- | 'liftControl' is a version of 'lift' that makes it possible to lift
-- control operations from the inner monad @m@ to the transformed monad @t m@.
-- It takes a continuation, to which it passes a version of 'peel', which is
-- kind of an \"inverse\" of @lift@.
liftControl :: (MonadTransControl t, Monad (t m), Monad m)
    => ((forall b. t m b -> m (LayerResult t b, LayerState t m)) -> m a)
    -> t m a
liftControl f = suspend >>= \s -> lift $ f (peel s)


------------------------------------------------------------------------------
-- | An often used composition: @'control' f = 'liftControl' f >>= uncurry 'restore'@
control :: (MonadTransControl t, Monad (t m), Monad m)
    => ((forall b. t m b -> m (LayerResult t b, LayerState t m))
        -> m (LayerResult t a, LayerState t m))
    -> t m a
control f = liftControl f >>= uncurry restore


------------------------------------------------------------------------------
-- | @liftOp@ is a particular application of 'liftControl' that allows lifting
-- control operations of type: @(a -> m b) -> m c@ to @(a -> t m b) -> t m c@.
--
-- For example:
--
-- @liftOp . withMVar :: ('MonadTransControl' t, Monad (t IO))
--     => MVar a -> (a -> t IO b) -> t IO b@
liftOp :: (MonadTransControl t, Monad (t m), Monad m)
    => ((a -> m (LayerResult t b, LayerState t m))
        -> m (LayerResult t c, LayerState t m))
    -> (a -> t m b)
    -> t m c
liftOp f = \g -> control (\run -> f $ run . g)


------------------------------------------------------------------------------
-- | @liftOp_@ is a particular application of 'liftControl' that allows
-- lifting control operations of type: @m a -> m b@ to @t m a -> t m b@.
--
-- For example:
--
-- @liftOp_ mask_ :: ('MonadTransControl' t, Monad (t IO)) => t IO a -> t IO a@
liftOp_ :: (MonadTransControl t, Monad (t m), Monad m)
    => (m (LayerResult t a, LayerState t m)
        -> m (LayerResult t b, LayerState t m))
    -> t m a
    -> t m b
liftOp_ f = \m -> control (\run -> f $ run m)


------------------------------------------------------------------------------
-- | @liftDiscard@ is a particular application of 'liftControl' that allows
-- lifting control operations of type: @m () -> m a@ to @t m () -> t m a@.
--
-- Note that, while the argument computation @t m ()@ has access to the
-- captured state, all its side-effects in @t@ are discarded. It is run only
-- for its side-effects in the inner monad @m@.
--
-- For example:
--
-- @layerDiscard forkIO :: ('MonadTransControl' t, Monad (t IO))
--     => t m () -> t m ThreadId@
liftDiscard :: (MonadTransControl t, Monad (t m), Monad m)
    => (m () -> m a)
    -> t m ()
    -> t m a
liftDiscard f m = liftControl $ \run -> f $ liftM (const ()) $ run m


------------------------------------------------------------------------------
-- | An invariant functor in the category of monads, using 'hoistiso' as the
-- analog of @invmap@:
class MInvariant t where
    -- | 'hoistiso' lifts a monad isomorphism between @m@ and @n@ into a monad
    -- morphism from @(t m)@ to @(t n)@.
    --
    -- The following laws hold for valid instances of 'MInvariant':
    --
    --     [Identity] @hoistiso id id = id@
    --
    --     [Composition]
    --         @hoistiso f g . hoistiso f' g' = hoistiso (f . f') (g' . g)@
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
    hoistiso f _ = hoist f


------------------------------------------------------------------------------
instance MFunctor ListT where
    hoist f (ListT m) = ListT $ f m


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
-- | 'MonadLift' is a multi-parameter type class parameterised by two monads
-- @i@ and @m@. If the constraint @MonadLift i m@ is satisfied, this means
-- that @m@ supports lifting operations from @i@. If @m@ is a monad built from
-- a monad transformer stack, then it supports lifting operations from any
-- monad @i@ anywhere in the stack. We call such a relationship between @i@
-- and @m@ a \"monad lift\". For a more details, read the in-depth
-- documentation provided in "Documentation.Layers.Overview".
class (Monad i, Monad m) => MonadLift i m where
    -- | 'lift' takes a computation from an inner monad @i@ and lifts it into
    -- the \"outer\" monad @m@.
    --
    -- The following laws hold for valid instances of 'MonadLift':
    --
    --     [Identity] @lift' . return = return@
    --
    --     [Composition] @lift' m >>= lift' . f = lift' (m >>= f)@
    --
    -- These laws are equivalent to the monad transformer laws of the
    -- 'Control.Monad.Trans.Class.MonadTrans' class from the @transformers@
    -- package.
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


#if __GLASGOW_HASKELL__ >= 707
------------------------------------------------------------------------------
-- | 'MonadLiftControl' represents the class of monad lifts that support
-- lifting control operations. See "Documentation.Layers.Overview" for a more
-- complete discussion.
class MonadLift i m => MonadLiftControl i m where
    peel' :: LiftState i m -> m a -> i (LiftResult i m a, LiftState i m)
    restore' :: proxy i -> LiftResult i m a -> LiftState i m -> m a
    suspend' :: proxy i -> m (LiftState i m)


------------------------------------------------------------------------------
type family LiftState (i :: * -> *) (m :: * -> *) :: * where
    LiftState m m = ()
    LiftState i (t m) = (LayerState t m, LiftState i m)


------------------------------------------------------------------------------
type family LiftResult (i :: * -> *) (m :: * -> *) :: * -> * where
    LiftResult m m = Identity
    LiftResult i (t m) = ComposeResult i t m


------------------------------------------------------------------------------
newtype ComposeResult i t m a
    = ComposeResult (LiftResult i m (LayerResult t a, LayerState t m))


------------------------------------------------------------------------------
instance MonadLift m m => MonadLiftControl m m where
    peel' _ = liftM (\a -> (Identity a, ()))
    restore' _ (Identity a) _ = return a
    suspend' _ = return ()


------------------------------------------------------------------------------
instance (Monad m, MonadLift (t m) (t m)) => MonadLiftControl (t m) (t m)
  where
    peel' _ = liftM (\a -> (Identity a, ()))
    restore' _ (Identity a) _ = return a
    suspend' _ = return ()


------------------------------------------------------------------------------
instance
    ( MonadTransControl t
    , Monad (t m)
    , MonadLift i (t m)
    , MonadLiftControl i m
    , LiftResult i (t m) ~ ComposeResult i t m
    , LiftState i (t m) ~ (LayerState t m, LiftState i m)
    )
  =>
    MonadLiftControl i (t m)
  where
    peel' (lys, lis) m =
        liftM (\(lir, lis') -> (ComposeResult lir, (lys, lis'))) $
            peel' lis (peel lys m)
    restore' p (ComposeResult lir) (_, lis) =
        lift (restore' p lir lis) >>= uncurry restore
    suspend' p = suspend >>= \a -> lift (suspend' p) >>= \b -> return (a, b)


------------------------------------------------------------------------------
data P (p :: * -> *) = P


------------------------------------------------------------------------------
liftControl' :: forall i m a. MonadLiftControl i m
    => ((forall b. m b -> i (LiftResult i m b, LiftState i m)) -> i a)
    -> m a
liftControl' f = suspend' (P :: P i) >>= \s -> lift' $ f (peel' s)


------------------------------------------------------------------------------
control' :: forall i m a. MonadLiftControl i m
    => ((forall b. m b -> i (LiftResult i m b, LiftState i m))
        -> i (LiftResult i m a, LiftState i m))
    -> m a
control' f = liftControl' f >>= uncurry (restore' (P :: P i))


------------------------------------------------------------------------------
-- | @liftOp'@ is a particular application of 'liftControl'' that allows
-- lifting control operations of type: @(a -> i b) -> i b@ to
-- @'MonadLiftControl' i m => (a -> m b) -> m b@.
--
-- For example:
--
-- @liftOp' . withMVar :: 'MonadLiftControl' 'IO' m => MVar a -> (a -> m b) -> m b@
liftOp' :: MonadLiftControl i m =>
     ((a -> i (LiftResult i m b, LiftState i m))
      -> i (LiftResult i m c, LiftState i m))
     -> (a -> m b)
     -> m c
liftOp' f = \g -> control' $ \run -> f $ run . g


------------------------------------------------------------------------------
-- | @liftOp_'@ is a particular application of 'liftControl'' that allows
-- lifting control operations of type: @i a -> i b@ to
-- @'MonadLiftControl' i m => m a -> m b@.
--
-- For example:
--
-- @liftOp_' mask_ :: 'MonadLiftControl' 'IO' m => m a -> m a@
liftOp_' :: MonadLiftControl i m
    => (i (LiftResult i m a, LiftState i m)
        -> i (LiftResult i m b, LiftState i m))
     -> m a
     -> m b
liftOp_' f = \m -> control' $ \run -> f $ run m


------------------------------------------------------------------------------
-- | @liftDiscard'@ is a particular application of 'liftControl'' that allows
-- lifting control operations of type: @i () -> i a@ to
-- @'MonadLiftControl' i m => m () -> m a@.
--
-- Note that, while the argument computation @m ()@ has access to the captured
-- state, all its side-effects in @m@ are discarded. It is run only for its
-- side-effects in the inner monad @i@.
--
-- For example:
--
-- @liftDiscard' forkIO :: 'MonadLiftControl' 'IO' m => m () -> m ThreadId@
liftDiscard' :: MonadLiftControl i m => (i () -> i a) -> m () -> m a
liftDiscard' f = \m -> liftControl' $ \run -> f $ liftM (const ()) $ run m


#else
------------------------------------------------------------------------------
-- | 'MonadLiftControl' represents the class of monad lifts that support
-- lifting control operations. See "Documentation.Layers.Overview" for a more
-- complete discussion.
class MonadLift i m => MonadLiftControl i m where
    -- | 'liftControl'' is a version of 'lift'' that makes it possible to lift
    -- control operations from an inner monad @i@ to the \"outer\" monad @m@.
    -- It takes a continuation, to which it passes an operation we call @run@,
    -- which is a kind of \"inverse\" of @lift'@.
    --
    -- Instances should satisfy the following laws:
    --
    -- [Identity] @liftControl' . const . return = return@
    --
    -- [Composition]
    --     @liftControl' (const m) >>= liftControl' . const . f@ = @liftControl' (const (m >>= f))@
    --
    -- [Preservation] @join (liftControl' (\\run -> run t)) = t@
    liftControl' :: ((forall b. m b -> i (m b)) -> i a) -> m a


------------------------------------------------------------------------------
instance MonadLift m m => MonadLiftControl m m where
    liftControl' f = f (liftM return)


------------------------------------------------------------------------------
instance (Monad m, MonadLift (t m) (t m)) => MonadLiftControl (t m) (t m)
  where
    liftControl' f = f (liftM return) 


------------------------------------------------------------------------------
instance
    ( MonadTransControl t
    , Monad (t m)
    , MonadLift i (t m)
    , MonadLiftControl i m
    )
  =>
    MonadLiftControl i (t m)
  where
    liftControl' f = liftControl $ \run -> liftControl' $ \run' ->
        f $ liftM (\m -> lift (lift' m) >>= uncurry restore) . run' . run


------------------------------------------------------------------------------
-- | An often used composition: @'control'' f = join . 'liftControl'' f@
control' :: MonadLiftControl i m
    => ((forall b. m b -> i (m b)) -> i (m a))
   -> m a
control' = join . liftControl'


------------------------------------------------------------------------------
-- | @liftOp'@ is a particular application of 'liftControl'' that allows
-- lifting control operations of type: @(a -> i b) -> i b@ to
-- @'MonadLiftControl' i m => (a -> m b) -> m b@.
--
-- For example:
--
-- @liftOp' . withMVar :: 'MonadLiftControl' 'IO' m => MVar a -> (a -> m b) -> m b@
liftOp' :: MonadLiftControl i m
    => ((a -> i (m b)) -> i (m c))
    -> (a -> m b)
    -> m c
liftOp' f = \g -> control' $ \run -> f $ run . g


------------------------------------------------------------------------------
-- | @liftOp_'@ is a particular application of 'liftControl'' that allows
-- lifting control operations of type: @i a -> i b@ to
-- @'MonadLiftControl' i m => m a -> m b@.
--
-- For example:
--
-- @liftOp_' mask_ :: 'MonadLiftControl' 'IO' m => m a -> m a@
liftOp_' :: MonadLiftControl i m => (i (m a) -> i (m b)) -> m a -> m b
liftOp_' f = \m -> control' $ \run -> f $ run m


------------------------------------------------------------------------------
-- | @liftDiscard'@ is a particular application of 'liftControl'' that allows
-- lifting control operations of type: @i () -> i a@ to
-- @'MonadLiftControl' i m => m () -> m a@.
--
-- Note that, while the argument computation @m ()@ has access to the captured
-- state, all its side-effects in @m@ are discarded. It is run only for its
-- side-effects in the inner monad @i@.
--
-- For example:
--
-- @liftDiscard' forkIO :: 'MonadLiftControl' 'IO' m => m () -> m ThreadId@
liftDiscard' :: MonadLiftControl i m => (i () -> i a) -> m () -> m a
liftDiscard' f = \m -> liftControl' $ \run -> f $ liftM (const ()) $ run m
#endif


------------------------------------------------------------------------------
-- | The constraint @'MonadLiftInvariant' i m@ holds if it is possible to lift
-- a monad (auto)morphism of @i@ to a monad (endo)morphism of @m@.
class Monad i => MonadLiftInvariant i m where
    -- | 'hoistiso'' represents an invariant (endo)functor in the category
    -- of monads. It takes a transformation @f@ of an inner monad @i@ and its
    -- inverse @g@ (such that @g . f = id@) and returns transformation of @m@
    -- analogous to @f@. (i.e., @hoistiso'@ lifts an automorphism of @i@
    -- to an endomorphism of @m@).
    --
    -- The following laws hold for valid instances of 'MonadLift':
    --
    --     [Identity] @hoistiso' id id = id@
    --
    --     [Composition]
    --         @hoistiso' f g . hoistiso' f' g' = hoistiso' (f . f') (g' . g)@
    --
    -- The difference between 'hoistiso'' and 'hoistiso' is that
    -- @hoistiso@ only lifts from the monad directly beneath the top of the
    -- stack, while @hoistiso'@ can lift from /any/ monad anywhere in the
    -- stack (including @m@ itself). (@hoistiso@ is used to implement
    -- @hoistiso'@)
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


------------------------------------------------------------------------------
-- | The constraint @'MonadLiftFunctor' i m@ holds it is possible to lift a
-- monad (auto)morphism of @i@ to a monad (endo)morphism of @m@.
-- The 'hoist'' which does this is more powerful than the 'hoistiso''
-- operation which is of the 'MonadLiftInvariant' type class.
class MonadLiftInvariant i m => MonadLiftFunctor i m where
    -- | 'hoist'' represents an (endo)functor in the category of monads. It
    -- takes a transformation @f@ of an inner monad @i@ returns a
    -- transformation of @m@ analogous to @f@. (i.e., @hoist@ lifts an
    -- endomorphism of @i@ to an endomorphism of @m@).
    --
    -- The following laws hold for valid instances of 'MonadLiftFunctor':
    --
    --     [Identity] @hoist' id = id@
    --
    --     [Composition] @hoist' f . hoist' g = hoist' (f . g)@
    --
    -- The difference between 'hoist'' and 'hoist' is that @hoist@ only
    -- lifts from the monad directly beneath the top of the stack, while
    -- @hoist'@ can lift from /any/ monad anywhere in the stack (including
    -- @m@ itself). (@hoist@ is used to implement @hoist'@)
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

{-







newtype NIO a = NIO {runNIO :: IO a} deriving (Functor, Monad, MonadLift IO, MonadLiftInvariant IO, MonadLiftFunctor IO)

instance MonadLiftControl' IO NIO where
    peel' = newtypePeel' runNIO
    restore' = newtypeRestore' NIO
    suspend' = newtypeSuspend' NIO

newtype NSIO a = NSIO {runNSIO :: IdentityT (StateT Int IO) a} deriving (Functor, Monad, MonadLift (StateT Int IO), MonadLiftInvariant (StateT Int IO), MonadLiftFunctor (StateT Int IO), MonadLift IO, MonadLiftInvariant IO, MonadLiftFunctor IO, MonadLift (IdentityT (StateT Int IO)), MonadLiftInvariant (IdentityT (StateT Int IO)), MonadLiftFunctor (IdentityT (StateT Int IO)))

instance MonadLiftControl' (StateT Int IO) NSIO where
    peel' = newtypePeel' runNSIO
    restore' = newtypeRestore' NSIO
    suspend' = newtypeSuspend' NSIO

instance MonadLiftControl' (IdentityT (StateT Int IO)) NSIO where
    peel' = newtypePeel' runNSIO
    restore' = newtypeRestore' NSIO
    suspend' = newtypeSuspend' NSIO

instance MonadLiftControl' IO NSIO where
    peel' = newtypePeel' runNSIO
    restore' = newtypeRestore' NSIO
    suspend' = newtypeSuspend' NSIO
-}
