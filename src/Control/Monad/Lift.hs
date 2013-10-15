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

{-|

This module is the core of the @layers@ package. It exports everything you
need for to implement monad transformers that take advantage of @layers@'
machinery and everything that you need to lift computations, operations and
morphisms through arbitrarily complicated stacks of monad transformers.

The @layers@ machinery is built upon two twin families of interfaces. The
'MonadTrans' family of interfaces is for lifting computations, operations and
morphisms up exactly one level in the transformer stack. That is, it lifts
computations from @m a@ to @t m a@. The 'MonadLift' family of interfaces is
for lifting computations, operations and morphisms from any level of the
transformer stack to the top of the transformer stack (including from the top
to itself). That is, it lifts computations from @i a@ to @m a@ (where the
constraint @'MonadLift' i m@ holds). The 'MonadTrans' family of interfaces is
mainly used by libraries that implement monad transformers and monad
interfaces, while the 'MonadLift' family is used by applications make use of
(stacks of) these transformers.

-}

module Control.Monad.Lift
    (
    -- * The @MonadTrans@ family
    -- $transfamily

    -- ** Lifting computations
      MonadTrans
    , lift

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
    , MInvariant
    , hoistiso
    , MFunctor
    , hoist

    -- * The @MonadLift@ family
    -- $liftfamily
    -- ** Lifting computations
    , MonadLift
    , lift'

    -- ** Lifting control operations
    , MonadLiftControl
#if __GLASGOW_HASKELL__ >= 707
    , Lift
    , LiftResult
    , LiftState
    , peel'
    , suspend'
    , restore'
    , extract'
    , result'
#endif
    , liftControl'
    , control'
    , liftOp'
    , liftOp_'
    , liftDiscard'

    -- ** Lifting morphisms
    , MonadLiftInvariant
    , hoistiso'
    , MonadLiftFunctor
    , hoist'
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
#if __GLASGOW_HASKELL__ >= 707
import           Data.Proxy (Proxy (Proxy))
#endif


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
#if __GLASGOW_HASKELL__ > 704
import           Data.Functor.Identity (Identity (Identity))
#endif


-- mmorph --------------------------------------------------------------------
import           Control.Monad.Morph (MFunctor (hoist))

{-$transfamily

The 'MonadTrans' family of interfaces consist of:

    * 'MonadTrans': 'lift';

    * 'MonadTransControl': @type 'Layer'@, @type 'LayerResult'@,
          @type 'LayerState'@; 'peel', 'restore', 'suspend', 'extract',
          'result'; 'liftControl', 'control', 'liftOp', 'liftOp_',
          'liftDiscard';

    * 'MInvariant': 'hoistiso';

    * 'MFunctor': 'hoist';

The 'MonadTrans' class and the 'lift' operation are re-exported from the
@transformers@ package.

The 'MonadTransControl' class and its associated operations are defined by
@layers@.

The 'MInvariant' class and its 'hoistiso' operation is defined by @layers@.

The 'MFunctor' class and its 'hoist' operation are re-exported from the
@mmorph@ package.

Ideally, all of these classes would be re-exports from more popular packages,
because then it would be possible to write monad transformers compatible with
@layers@' monad interfaces without ever incurring a dependency on @layers@.
As 'MonadTrans' and 'MFunctor' come from @transformers@ and @mmorph@
respectively, we're already half way there. @mmorph@ is also the most sensible
home for 'MInvariant', and
<https://github.com/Gabriel439/Haskell-MMorph-Library/pull/1 hopefully> it
will get moved there some day soon. 'MonadTransControl' is more complicated:
there is a very similar class (the design of which I copied) defined in the
@monad-control@ package, which is a relatively popular package. However,
@layers@' version has a few important differences. It is conceivable that
@monad-control@ could incorporate these changes some day, but it wouldn't be
trivial. This would be the ideal scenario for @layers@.

-}

{-$liftfamily

The 'MonadLift' family of interfaces consist of:

    * 'MonadLift': 'lift'';

    * 'MonadLiftControl': @type 'Lift'@, @type 'LiftResult'@,
          @type 'LiftState'@; 'peel'', 'restore'', 'suspend'', 'extract'',
          'result''; 'liftControl'', 'control'', 'liftOp'', 'liftOp_'',
          'liftDiscard'';

    * 'MonadLiftInvariant': 'hoistiso'';

    * 'MonadLiftFunctor': 'hoist'';

Each operation in the 'MonadLift' family of interfaces is an analogue of an
operation from the 'MonadTrans' family of interfaces. The naming of these
operations reflect this relationship.

(Note: The 'Lift', 'LiftResult' and 'LiftState' type synonyms and the 'peel'',
'restore'', 'suspend'', 'extract'' and 'result'' operations are only available
when compiled with GHC 7.8 and above. A different implementation is necessary
on older versions (see "Documentation.Layers.Overview") because of the lack of
support for closed type families.)

-}

------------------------------------------------------------------------------
-- | 'MonadTransControl' represents the class of monad transformers through
-- which it is possible to lift control operations. See
-- "Documentation.Layers.Overview" for a more complete discussion of how it
-- works.
class MonadTrans t => MonadTransControl t where
    -- | The portion of the result of executing a computation of @t@ that is
    -- independent of @m@ and which is not the new 'LayerState'.
    --
    -- Note: On versions of GHC prior to 7.4, 'LayerResult' is an associated
    -- /data/ type instead of a type synonym due to GHC bug
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
    -- /data/ type instead of a type synonym due to GHC bug
    -- <http://hackage.haskell.org/trac/ghc/ticket/5595 #5595>.
#if __GLASGOW_HASKELL__ >= 704
    type LayerState t (m :: * -> *) :: *
#else
    data LayerState t :: (* -> *) -> *
#endif

    -- | Given the current \"state\" of the monad transformer @t@ (given by
    -- 'suspend'), 'peel' unwraps the @t@ and returns the result and the new
    -- state of @t@ wrapped in @m@. 'liftControl', a more often used
    -- operation, is defined in terms of 'peel' and 'suspend'. See
    -- "Documentation.Layers.Overview" for more information.
    --
    -- Instances should satisfy similar laws as the monad transformer laws:
    --
    -- [Identity] @'liftControl' . const . return ≡ return@
    --
    -- [Composition]
    --     @'liftControl' (const m) >>=
    --         'liftControl' . const . f ≡ 'liftControl' (const (m >>= f))@
    peel :: Monad m => LayerState t m -> t m a -> m (Layer t m a)

    -- | Reconstruct a @t m@ computation from the monadic state of @m@ that is
    -- returned from the 'peel' operation.
    --
    -- Instances should satisfy the following law:
    --
    -- [Preservation] @'liftControl' (\\run -> run t) >>= 'restore' ≡ t@
    restore :: Monad m => Layer t m a -> t m a

    -- | Captures the current \"state\" of the monad transformer @t@. See the
    -- explanation in "Documentation.Layers.Overview" for what this actually
    -- means.
    suspend :: Monad m => t m (LayerState t m)

    -- | 'extract' inspects a @'LayerResult' t a@ value and tries to
    -- \"extract\" an @a@ value from it, if possible. This can be used to
    -- detect if the monad transformer @t@ short-circuited when the
    -- 'LayerResult' was captured (if it did, 'extract' returns @Nothing@).
    -- This trick is used to implement the universal pass-through instance of
    -- 'Monad.Try.MonadTry'.
    --
    -- Instances should satisfy the following laws:
    --
    -- [Preservation]
    --     @liftM ('extract' (Proxy :: Proxy t)) ('result' (return a))
    --         ≡ return (Just a)@
    --
    -- [Zero]
    --     @(liftM ('extract' (Proxy :: Proxy t)) ('result' m)
    --         ≡ return Nothing) ⇒ (m >>= f ≡ m)@
    extract :: proxy t -> LayerResult t a -> Maybe a


------------------------------------------------------------------------------
-- | A type synonym to make the type signatures slightly less scary.
type Layer t m a = (LayerResult t a, LayerState t m)


------------------------------------------------------------------------------
instance Error e => MonadTransControl (ErrorT e) where
#if __GLASGOW_HASKELL__ >= 704
    type LayerResult (ErrorT e) = Either e
    type LayerState (ErrorT e) m = ()
    peel _ (ErrorT m) = liftM (\a -> (a, ())) m
    restore (a, _) = ErrorT $ return a
    suspend = return ()
    extract _ = either (const Nothing) Just
#else
    newtype LayerResult (ErrorT e) a = ER (Either e a)
    newtype LayerState (ErrorT e) m = ES ()
    peel _ (ErrorT m) = liftM (\a -> (ER a, ES ())) m
    restore (ER a, _) = ErrorT $ return a
    suspend = return (ES ())
    extract _ (ER e) = either (const Nothing) Just e
#endif


------------------------------------------------------------------------------
instance MonadTransControl IdentityT where
#if __GLASGOW_HASKELL__ >= 704
    type LayerResult IdentityT = Identity
    type LayerState IdentityT m = ()
    peel _ (IdentityT m) = liftM (\a -> (Identity a, ())) m
    restore (Identity a, _) = IdentityT $ return a
    suspend = return ()
    extract _ (Identity a) = Just a
#else
    newtype LayerResult IdentityT a = IR a
    newtype LayerState IdentityT m = IS ()
    peel _ (IdentityT m) = liftM (\a -> (IR a, IS ())) m
    restore (IR a, _) = IdentityT $ return a
    suspend = return (IS ())
    extract _ (IR a) = Just a
#endif


------------------------------------------------------------------------------
instance MonadTransControl ListT where
#if __GLASGOW_HASKELL__ >= 704
    type LayerResult ListT = []
    type LayerState ListT m = ()
    peel _ (ListT m) = liftM (\a -> (a, ())) m
    restore (a, _) = ListT $ return a
    suspend = return ()
    extract _ = foldr (const . Just) Nothing
#else
    newtype LayerResult ListT a = LR [a]
    newtype LayerState ListT m = LS ()
    peel _ (ListT m) = liftM (\a -> (LR a, LS ())) m
    restore (LR a, _) = ListT $ return a
    suspend = return (LS ())
    extract _ (LR xs) = foldr (const . Just) Nothing xs
#endif


------------------------------------------------------------------------------
instance MonadTransControl MaybeT where
#if __GLASGOW_HASKELL__ >= 704
    type LayerResult MaybeT = Maybe
    type LayerState MaybeT m = ()
    peel _ (MaybeT m) = liftM (\a -> (a, ())) m
    restore (a, _) = MaybeT $ return a
    suspend = return ()
    extract _ = id
#else
    newtype LayerResult MaybeT a = MR (Maybe a)
    newtype LayerState MaybeT m = MS ()
    peel _ (MaybeT m) = liftM (\a -> (MR a, MS ())) m
    restore (MR a, _) = MaybeT $ return a
    suspend = return (MS ())
    extract _ (MR a) = a
#endif


------------------------------------------------------------------------------
instance MonadTransControl (ReaderT r) where
#if __GLASGOW_HASKELL__ >= 704
    type LayerResult (ReaderT r) = Identity
    type LayerState (ReaderT r) m = r
    peel r (ReaderT m) = liftM (\a -> (Identity a, r)) (m r)
    restore (Identity a, _) = ReaderT $ \_ -> return a
    suspend = ReaderT $ \r -> return r
    extract _ (Identity a) = Just a
#else
    newtype LayerResult (ReaderT r) a = RR a
    newtype LayerState (ReaderT r) m = RS r
    peel (RS r) (ReaderT m) = liftM (\a -> (RR a, RS r)) (m r)
    restore (RR a, _) = ReaderT $ \_ -> return a
    suspend = ReaderT $ \r -> return (RS r)
    extract _ (RR a) = Just a
#endif


------------------------------------------------------------------------------
instance MonadTransControl (StateT s) where
#if __GLASGOW_HASKELL__ >= 704
    type LayerResult (StateT s) = Identity
    type LayerState (StateT s) m = s
    peel s (StateT m) = liftM (first Identity) (m s)
    restore (Identity a, s) = StateT $ \_ -> return (a, s)
    suspend = StateT $ \s -> return (s, s)
    extract _ (Identity a) = Just a
#else
    newtype LayerResult (StateT s) a = SR a
    newtype LayerState (StateT s) m = SS s
    peel (SS s) (StateT m) = liftM (SR *** SS) (m s)
    restore (SR a, SS s) = StateT $ \_ -> return (a, s)
    suspend = StateT $ \s -> return (SS s, s)
    extract _ (SR a) = Just a
#endif


------------------------------------------------------------------------------
instance MonadTransControl (L.StateT s) where
#if __GLASGOW_HASKELL__ >= 704
    type LayerResult (L.StateT s) = Identity
    type LayerState (L.StateT s) m = s
    peel s (L.StateT m) = liftM (first Identity) (m s)
    restore (Identity a, s) = L.StateT $ \_ -> return (a, s)
    suspend = L.StateT $ \s -> return (s, s)
    extract _ (Identity a) = Just a
#else
    newtype LayerResult (L.StateT s) a = SR' a
    newtype LayerState (L.StateT s) m = SS' s
    peel (SS' s) (L.StateT m) = liftM (SR' *** SS') (m s)
    restore (SR' a, SS' s) = L.StateT $ \_ -> return (a, s)
    suspend = L.StateT $ \s -> return (SS' s, s)
    extract _ (SR' a) = Just a
#endif


------------------------------------------------------------------------------
instance Monoid w => MonadTransControl (RWST r w s) where
#if __GLASGOW_HASKELL__ >= 704
    type LayerResult (RWST r w s) = (,) w
    type LayerState (RWST r w s) m = (r, s)
    peel (r, s) (RWST m) = liftM (\(a, s', w) -> ((w, a), (r, s'))) (m r s)
    restore ((w, a), (_, s)) = RWST $ \_ _ -> return (a, s, w)
    suspend = RWST $ \r s -> return ((r, s), s, mempty)
    extract _ (_, a) = Just a
#else
    newtype LayerResult (RWST r w s) a = RWSR (a, w)
    newtype LayerState (RWST r w s) m = RWSS (r, s)
    peel (RWSS (r, s)) (RWST m) =
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
    peel (r, s) (L.RWST m) = liftM (\(a, s', w) -> ((w, a), (r, s'))) (m r s)
    restore ((w, a), (_, s)) = L.RWST $ \_ _ -> return (a, s, w)
    suspend = L.RWST $ \r s -> return ((r, s), s, mempty)
    extract _ (_, a) = Just a
#else
    newtype LayerResult (L.RWST r w s) a = RWSR' (a, w)
    newtype LayerState (L.RWST r w s) m = RWSS' (r, s)
    peel (RWSS' (r, s)) (L.RWST m) =
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
    peel _ (WriterT m) = liftM (\(a, w) -> ((w, a), ())) m
    restore ((w, a), _) = WriterT $ return (a, w)
    suspend = return ()
    extract _ (_, a) = Just a
#else
    newtype LayerResult (WriterT w) a = WR (a, w)
    newtype LayerState (WriterT w) m = WS ()
    peel _ (WriterT m) = liftM (\a -> (WR a, WS ())) m
    restore (WR a, _) = WriterT $ return a
    suspend = return (WS ())
    extract _ (WR (a, _)) = Just a
#endif


------------------------------------------------------------------------------
instance Monoid w => MonadTransControl (L.WriterT w) where
#if __GLASGOW_HASKELL__ >= 704
    type LayerResult (L.WriterT w) = (,) w
    type LayerState (L.WriterT w) m = ()
    peel _ (L.WriterT m) = liftM (\(a, w) -> ((w, a), ())) m
    restore ((w, a), _) = L.WriterT $ return (a, w)
    suspend = return ()
    extract _ (_, a) = Just a
#else
    newtype LayerResult (L.WriterT w) a = WR' (a, w)
    newtype LayerState (L.WriterT w) m = WS' ()
    peel _ (L.WriterT m) = liftM (\a -> (WR' a, WS' ())) m
    restore ((WR' a), _) = L.WriterT $ return a
    suspend = return (WS' ())
    extract _ (WR' (a, _)) = Just a
#endif


------------------------------------------------------------------------------
-- | Given a computation in @t@, return the 'LayerResult' of 'peel'ing back
-- @t@.
--
-- In practice, this operation is probably not very useful, but it simplifies
-- the definition of the laws of 'extract'.
result :: forall t m a. (MonadTransControl t, Monad (t m), Monad m)
    => t m a
    -> t m (LayerResult t a)
result m = suspend >>= \s -> lift (liftM fst (peel s m))


------------------------------------------------------------------------------
-- | 'liftControl' is a version of 'lift' that makes it possible to lift
-- control operations from the inner monad @m@ to the transformed monad @t m@.
-- It takes a continuation, to which it passes a version of 'peel', which is
-- kind of an \"inverse\" of @lift@.
liftControl :: (MonadTransControl t, Monad (t m), Monad m)
    => ((forall b. t m b -> m (Layer t m b)) -> m a)
    -> t m a
liftControl f = suspend >>= \s -> lift $ f (peel s)
{-# INLINABLE liftControl #-}


------------------------------------------------------------------------------
-- | A version of 'liftControl' that automatically restores the captured
-- state returned from the continuation to the outer monad.
--
-- @
-- catch' :: (Exception e, 'MonadTransControl' t, Monad (t IO)) => t m b -> (e -> t m b) -> t m b
-- catch' m h = 'control' (\run -> catch (run m) (run . h))
-- @
control :: (MonadTransControl t, Monad (t m), Monad m)
    => ((forall b. t m b -> m (Layer t m b)) -> m (Layer t m a))
    -> t m a
control f = liftControl f >>= restore
{-# INLINABLE control #-}


------------------------------------------------------------------------------
-- | 'liftOp' is a particular application of 'liftControl' that allows lifting
-- control operations of type: @(a -> m b) -> m c@ to @(a -> t m b) -> t m c@.
--
-- @
-- withMVar' :: ('MonadTransControl' t, Monad (t IO)) => MVar a -> (a -> t IO b) -> t IO b
-- withMVar' = 'liftOp' . withMVar
-- @
liftOp :: (MonadTransControl t, Monad (t m), Monad m)
    => ((a -> m (Layer t m b)) -> m (Layer t m c))
    -> (a -> t m b)
    -> t m c
liftOp f = \g -> control (\run -> f $ run . g)
{-# INLINABLE liftOp #-}


------------------------------------------------------------------------------
-- | 'liftOp_' is a particular application of 'liftControl' that allows
-- lifting control operations of type: @m a -> m b@ to @t m a -> t m b@.
--
-- @
-- mask_' :: ('MonadTransControl' t, Monad (t IO)) => t IO a -> t IO a
-- mask_' = 'liftOp_' mask_
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
-- forkIO' :: ('MonadTransControl' t, Monad (t IO)) => t m () -> t m ThreadId
-- forkIO' = 'liftDiscard' forkIO
-- @
--
-- Note that, while the argument computation @t m ()@ has access to the
-- captured state, all its side-effects in @t@ are discarded. It is run only
-- for its side-effects in @m@.
liftDiscard :: (MonadTransControl t, Monad (t m), Monad m)
    => (m () -> m a)
    -> t m ()
    -> t m a
liftDiscard f m = liftControl $ \run -> f $ liftM (const ()) $ run m
{-# INLINABLE liftDiscard #-}


------------------------------------------------------------------------------
-- | An invariant functor in the category of monads, using 'hoistiso' as the
-- analog of @invmap@:
class MInvariant t where
    -- | 'hoistiso' lifts a monad isomorphism between @m@ and @n@ into a monad
    -- morphism from @(t m)@ to @(t n)@.
    --
    -- The following laws hold for valid instances of 'MInvariant':
    --
    --     [Identity] @'hoistiso' id id ≡ id@
    --
    --     [Composition]
    --         @'hoistiso' f g . 'hoistiso' f' g' ≡
    --             'hoistiso' (f . f') (g' . g)@
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
-- | The constraint @'MonadLift' i m@ is satisfied when @i@ is an inner monad
-- of @m@ such that it is possible to lift actions from @i@ into @m@. If @m@
-- is a monad built from a monad transformer stack, then it supports lifting
-- operations from any monad @i@ anywhere in the stack. We call such a
-- relationship between @i@ and @m@ a \"monad lift\". For a more details, read
-- the in-depth documentation provided in "Documentation.Layers.Overview".
class (Monad i, Monad m) => MonadLift i m where
    -- | 'lift' takes a computation from an inner monad @i@ and lifts it into
    -- the \"outer\" monad @m@.
    --
    -- The following laws hold for valid instances of 'MonadLift':
    --
    --     [Identity] @'lift'' . return ≡ return@
    --
    --     [Composition] @'lift'' m >>= 'lift'' . f ≡ 'lift'' (m >>= f)@
    --
    -- The difference between 'lift'' and 'lift' is that 'lift' only lifts
    -- from the monad directly beneath the top of the stack, while @lift'@ can
    -- lift from /any/ monad anywhere in the stack (including @m@ itself).
    --
    -- Note: If you know that you want to lift from the monad directly beneath
    -- the top of the stack, it's often better to use 'lift' than 'lift''.
    -- This improves type inference because 'lift' is less polymorphic than
    -- 'lift''.
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


#if __GLASGOW_HASKELL__ >= 707
------------------------------------------------------------------------------
-- | 'MonadLiftControl' represents the class of monad lifts that support
-- lifting control operations. See "Documentation.Layers.Overview" for a more
-- complete discussion.
class MonadLift i m => MonadLiftControl i m where
    -- | Given the current \"state\" of the monad @m@ relative to @i@ (given
    -- by 'suspend''), 'peel'' unwraps the @m@ and returns the result and the
    -- new state of @m@ (relative to @i@) wrapped in @i@. 'liftControl'', a
    -- more often used operation, is defined in terms of 'peel'' and
    -- 'suspend''. See "Documentation.Layers.Overview" for more information.
    --
    -- Instances should satisfy similar laws as the monad transformer laws:
    --
    -- [Identity] @liftControl' . const . return ≡ return@
    --
    -- [Composition]
    --     @'liftControl'' (const m) >>= 'liftControl'' . const . f ≡
    --         'liftControl'' (const (m >>= f))@
    peel' :: LiftState i m -> m a -> i (Lift i m a)

    -- | Reconstruct an @m@ computation from the monadic state of @m@
    -- (realtive to @i@) that is returned from the 'peel\'' operation.
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
    --     @liftM ('extract'' (Proxy :: Proxy i) (Proxy :: Proxy m))
    --         ('result'' (Proxy :: Proxy i) (return a))
    --             ≡ return (Just a)@
    --
    -- [Zero]
    --     @(liftM ('extract' (Proxy :: Proxy i) (Proxy :: Proxy m))
    --         ('result' (Proxy :: Proxy i) m)
    --             ≡ return Nothing) ⇒ (m >>= f ≡ m)@
    extract' :: proxy i -> proxy m -> LiftResult i m a -> Maybe a


------------------------------------------------------------------------------
-- | A type synonym to make the type signatures slightly less scary.
type Lift i m a = (LiftResult i m a, LiftState i m)


------------------------------------------------------------------------------
-- | The portion of the result of executing a computation of @m@ relative to
-- @i@ that is independent of @m@ (and all layers down to @i@) and which is
-- not the new 'LiftState'.
type family LiftResult (i :: * -> *) (m :: * -> *) :: * -> * where
    LiftResult m m = Identity
    LiftResult i (t m) = ComposeResult i t m


------------------------------------------------------------------------------
-- | The \"state\" needed to 'peel'' a computation of @m@ back to @i@. Running
-- a peeled computation returns a 'LiftResult' and an updated 'LiftState'.
type family LiftState (i :: * -> *) (m :: * -> *) :: * where
    LiftState m m = ()
    LiftState i (t m) = (LayerState t m, LiftState i m)


------------------------------------------------------------------------------
newtype ComposeResult i t m a
    = ComposeResult (LiftResult i m (LayerResult t a, LayerState t m))


------------------------------------------------------------------------------
instance MonadLift m m => MonadLiftControl m m where
    peel' _ = liftM (\a -> (Identity a, ()))
    restore' _ (Identity a, _) = return a
    suspend' _ = return ()
    extract' _ _ (Identity a) = Just a


------------------------------------------------------------------------------
instance (Monad m, MonadLift (t m) (t m)) => MonadLiftControl (t m) (t m)
  where
    peel' _ = liftM (\a -> (Identity a, ()))
    restore' _ (Identity a, _) = return a
    suspend' _ = return ()
    extract' _ _ (Identity a) = Just a


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
    {-# INLINE peel' #-}

    restore' p (ComposeResult lir, (_, lis)) =
        lift (restore' p (lir, lis)) >>= restore
    {-# INLINE restore' #-}

    suspend' p = suspend >>= \a -> lift (suspend' p) >>= \b -> return (a, b)
    {-# INLINE suspend' #-}

    extract' (_ :: proxy i) (_ :: proxy (t m)) (ComposeResult r) = join $
        fmap (\(r', _) -> extract (Proxy :: Proxy t) r') $
            extract' (Proxy :: Proxy i) (Proxy :: Proxy m) r
    {-# INLINE extract' #-}


------------------------------------------------------------------------------
-- | Given a computation in @t@, return the 'LayerResult' of 'peel'ing back
-- @t@.
--
-- In practice, this operation is probably not very useful, but it simplifies
-- the definition of the laws of 'extract'.
result' :: forall proxy m i a. MonadLiftControl i m
    => proxy i
    -> m a
    -> m (LiftResult i m a)
result' p m
    = suspend' p >>= \s -> lift' (liftM fst (peel' s m :: i (Lift i m a)))


------------------------------------------------------------------------------
-- | 'liftControl'' is a version of 'lift'' that makes it possible to lift
-- control operations from the inner monad @i@ to the outer monad @m@. It
-- takes a continuation, to which it passes a version of 'peel'', which is
-- kind of an \"inverse\" of 'lift''.
liftControl' :: forall i m a. MonadLiftControl i m
    => ((forall b. m b -> i (Lift i m b)) -> i a)
    -> m a
liftControl' f = suspend' (Proxy :: Proxy i) >>= \s -> lift' $ f (peel' s)
{-# INLINABLE liftControl' #-}


------------------------------------------------------------------------------
-- | A version of 'liftControl'' that automatically restores the captured
-- state returned from the continuation to the outer monad.
--
-- @
-- catch' :: (Exception e, 'MonadLiftControl' IO m) => m b -> (e -> m b) -> m b
-- catch' m h = 'control'' (\run -> catch (run m) (run . h))
-- @
control' :: forall i m a. MonadLiftControl i m
    => ((forall b. m b -> i (Lift i m b)) -> i (Lift i m a))
    -> m a
control' f = liftControl' f >>= restore' (Proxy :: Proxy i)
{-# INLINABLE control' #-}


------------------------------------------------------------------------------
liftOp' :: MonadLiftControl i m
     => ((a -> i (Lift i m b)) -> i (Lift i m c))
     -> (a -> m b)
     -> m c


------------------------------------------------------------------------------
liftOp_' :: MonadLiftControl i m
    => (i (Lift i m a) -> i (Lift i m b))
     -> m a
     -> m b
#else
------------------------------------------------------------------------------
-- | 'MonadLiftControl' represents the class of monad lifts that support
-- lifting control operations. See "Documentation.Layers.Overview" for a more
-- complete discussion.
class MonadLift i m => MonadLiftControl i m where
    -- | 'liftControl'' is a version of 'lift'' that makes it possible to lift
    -- control operations from an inner monad @i@ to the \"outer\" monad @m@.
    -- It takes a continuation, to which it passes an operation we call @run@,
    -- which is a kind of \"inverse\" of 'lift''.
    --
    -- Instances should satisfy the following laws:
    --
    -- [Identity] @'liftControl'' . const . return ≡ return@
    --
    -- [Composition]
    --     @'liftControl'' (const m) >>= 'liftControl'' . const . f ≡
    --         'liftControl'' (const (m >>= f))@
    --
    -- [Preservation] @join ('liftControl'' (\\run -> run t)) ≡ t@
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
    liftControl' = \f -> liftControl $ \run -> liftControl' $ \run' ->
        f $ liftM (\m -> lift m >>= restore) . run' . run
    {-# INLINE liftControl' #-}


------------------------------------------------------------------------------
-- | A version of 'liftControl'' that automatically restores the captured
-- state returned from the continuation to the outer monad.
--
-- @
-- catch' :: (Exception e, 'MonadLiftControl' IO m) => m b -> (e -> m b) -> m b
-- catch' m h = 'control'' (\run -> catch (run m) (run . h))
-- @
control' :: MonadLiftControl i m
    => ((forall b. m b -> i (m b)) -> i (m a))
   -> m a
control' = join . liftControl'
{-# INLINABLE control' #-}


------------------------------------------------------------------------------
liftOp' :: MonadLiftControl i m
    => ((a -> i (m b)) -> i (m c))
    -> (a -> m b)
    -> m c


------------------------------------------------------------------------------
liftOp_' :: MonadLiftControl i m => (i (m a) -> i (m b)) -> m a -> m b
#endif


------------------------------------------------------------------------------
-- | A particular application of 'liftControl'' that lifts control operations
-- from @(a -> i b) -> i b@ to @(a -> m b) -> m b@.
--
-- @
-- withMVar' :: 'MonadLiftControl' IO m => MVar a -> (a -> m b) -> m b
-- withMVar' = 'liftOp'' . withMVar
-- @
liftOp' f = \g -> control' $ \run -> f $ run . g
{-# INLINABLE liftOp' #-}


------------------------------------------------------------------------------
-- | A particular application of 'liftControl'' that lifts control operations
-- from @i a -> i b@ to @m a -> m b@.
--
-- @
-- mask_' :: 'MonadLiftControl' IO m => m a -> m a
-- mask_' = 'liftOp_'' mask_
-- @
liftOp_' f = \m -> control' $ \run -> f $ run m
{-# INLINABLE liftOp_' #-}


------------------------------------------------------------------------------
-- | A particular application of 'liftControl'' that lifts control operations
-- from @i () -> i a@ to @m () -> m a@.
--
-- @
-- forkIO' :: 'MonadLiftControl' 'IO' m => m () -> m ThreadId
-- forkIO' = 'liftDiscard'' forkIO 
-- @
--
-- Note that, while the argument computation @m ()@ has access to the captured
-- state, all its side-effects in @m@ (relative to @i@) are discarded. It is
-- run only for its side-effects in the inner monad @i@.
liftDiscard' :: MonadLiftControl i m => (i () -> i a) -> m () -> m a
liftDiscard' f = \m -> liftControl' $ \run -> f $ liftM (const ()) $ run m
{-# INLINABLE liftDiscard' #-}


------------------------------------------------------------------------------
-- | The constraint @'MonadLiftInvariant' i m@ holds if it is possible to lift
-- a monad automorphism in @i@ to a monad endomorphism in @m@.
class Monad i => MonadLiftInvariant i m where
    -- | 'hoistiso'' represents an invariant endofunctor in the category of
    -- monads. It takes a transformation @f@ of an inner monad @i@ and its
    -- inverse @g@ (such that @g . f = id@) and returns transformation of @m@
    -- analogous to @f@. (i.e., @hoistiso'@ lifts an automorphism in @i@
    -- to an endomorphism in @m@).
    --
    -- The following laws hold for valid instances of 'MonadLiftInvariant':
    --
    --     [Identity] @'hoistiso'' id id ≡ id@
    --
    --     [Composition]
    --         @'hoistiso'' f g . 'hoistiso'' f' g' ≡
    --             'hoistiso'' (f . f') (g' . g)@
    --
    -- There are two main differences between 'hoistiso'' and 'hoistiso'. The
    -- first is that 'hoistiso' only lifts from the monad directly beneath the
    -- top of the stack, while @hoistiso'@ can lift from /any/ monad anywhere
    -- in the stack (including @m@ itself). The second is that 'hoistiso'' can
    -- only accept an automorphism (producing an endomorphism), while
    -- 'hoistiso' can accept any isomorphism, producing a homomorphism. In
    -- other words, the morphism passed to 'hoistiso' can be used to change
    -- the type of the inner monad, but this is not possible with 'hoistiso''.
    --
    -- Note: If you know that you want to lift from the monad directly beneath
    -- the top of the stack, it's often better to use 'hoistiso' than
    -- 'hoistiso''. This improves type inference because 'hoistiso' is less
    -- polymorphic than 'hoistiso''.
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
    --     [Identity] @'hoist'' id ≡ id@
    --
    --     [Composition] @'hoist'' f . 'hoist'' g ≡ 'hoist'' (f . g)@
    --
    -- There are two main differences between 'hoist'' and 'hoist'. The first
    -- is that 'hoist' only lifts from the monad directly beneath the top of
    -- the stack, while @hoist'@ can lift from /any/ monad anywhere in the
    -- stack (including @m@ itself). The second is that 'hoist'' can only
    -- accept an endomorphism, while 'hoist' can accept any homomorphism. In
    -- other words, the morphism passed to 'hoist' can be used to change the
    -- type of the inner monad, but this is not possible with 'hoist''.
    --
    -- Note: If you know that you want to lift from the monad directly beneath
    -- the top of the stack, it's often better to use 'hoist' than 'hoist''.
    -- This improves type inference because 'hoist' is less polymorphic than
    -- 'hoist''.
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
