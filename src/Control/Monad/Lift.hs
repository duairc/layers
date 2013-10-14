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

#if __GLASGOW_HASKELL__ >= 707

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

The 'MonadTrans' family of interfaces consist of:

    * 'MonadTrans': 'lift';

    * 'MonadTransControl': @type 'Layer'@, @type 'LayerResult'@
          , @type 'LayerState'@; 'peel', 'restore', 'suspend', 'extract'
          ; 'liftControl', 'control', 'liftOp', 'liftOp_', 'liftDiscard';

    * 'MInvariant': 'hoistiso';

    * 'MFunctor': 'hoist';

    * 'MMonad': 'embed';

The 'MonadTrans' class and the 'lift' operation are re-exported from
@transformers@. This makes @layers@ at least partially compatible with every
monad transformer in the wild out of the box.

The 'MFunctor' and 'MMonad' type classes and their respective operations
'hoist' and 'embed' are re-exported from the @mmorph@ package. Despite being a
relatively new package it has already been pretty widely adapted, so @layers@
is even more compatible (out of the box) with monad transformers that use
@mmorph@. Unfortunately, 'MInvariant' (which is more important to @layers@
than 'MFunctor') is not in @mmorph@
<https://github.com/Gabriel439/Haskell-MMorph-Library/pull/1 yet>, so we
define it here instead.

The 'MonadTransControl' class and its associated operations are defined by
@layers@, despite there being a similar class in the fairly widely used
@monad-control@ package. There are a couple of important differences between
@layers@' 'MonadTransControl' and @monad-control@'s (by which it is inspired),
but perhaps these changes could be incorporated into @monad-control@ at some
point.

The 'MonadLift' family of interfaces consist of:

    * 'MonadLift': 'lift'';

    * 'MonadLiftControl': @type 'Lift'@, @type 'LiftResult'@
          , @type 'LiftState'@; 'peel'', 'restore'', 'suspend'', 'extract''
          ; 'liftControl'', 'control'', 'liftOp'', 'liftOp_'', 'liftDiscard'';

    * 'MonadLiftInvariant': 'hoistiso'';

    * 'MonadLiftFunctor': 'hoist'';

Each operation in the 'MonadLift' family of interfaces is an analogue of an
operation from the 'MonadTrans' family of interfaces. The naming of these
operations reflect this relationship.

-}

#else

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

The 'MonadTrans' family of interfaces consist of:

    * 'MonadTrans': 'lift';

    * 'MonadTransControl': @type 'Layer'@, @type 'LayerResult'@
          , @type 'LayerState'@; 'peel', 'restore', 'suspend', 'extract'
          ; 'liftControl', 'control', 'liftOp', 'liftOp_', 'liftDiscard';

    * 'MInvariant': 'hoistiso';

    * 'MFunctor': 'hoist';

    * 'MMonad': 'embed';

The 'MonadTrans' class and the 'lift' operation are re-exported from
@transformers@. This makes @layers@ at least partially compatible with every
monad transformer in the wild out of the box.

The 'MFunctor' and 'MMonad' type classes and their respective operations
'hoist' and 'embed' are re-exported from the @mmorph@ package. Despite being a
relatively new package it has already been pretty widely adapted, so @layers@
is even more compatible (out of the box) with monad transformers that use
@mmorph@. Unfortunately, 'MInvariant' (which is more important to @layers@
than 'MFunctor') is not in @mmorph@
<https://github.com/Gabriel439/Haskell-MMorph-Library/pull/1 yet>, so we
define it here instead.

The 'MonadTransControl' class and its associated operations are defined by
@layers@, despite there being a similar class in the fairly widely used
@monad-control@ package. There are a couple of important differences between
@layers@' 'MonadTransControl' and @monad-control@'s (by which it is inspired),
but perhaps these changes could be incorporated into @monad-control@ at some
point.

The 'MonadLift' family of interfaces consist of:

    * 'MonadLift': 'lift'';

    * 'MonadLiftControl': 'liftControl'', 'control'', 'liftOp'', 'liftOp_'',
          'liftDiscard'';

    * 'MonadLiftInvariant': 'hoistiso'';

    * 'MonadLiftFunctor': 'hoist'';

Each operation in the 'MonadLift' family of interfaces is an analogue of an
operation from the 'MonadTrans' family of interfaces. The naming of these
operations reflect this relationship.

-}

#endif

module Control.Monad.Lift
    (
    -- * The @MonadTrans@/@MMorph@ family
      MonadTrans
    , lift
    , MonadTransControl
    , Layer
    , LayerResult
    , LayerState
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
    , Lift
    , LiftResult
    , LiftState
    , peel'
    , suspend'
    , restore'
    , extract'
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
#if __GLASGOW_HASKELL__ >= 704
import           Control.Arrow (first)
#else
import           Control.Arrow ((***))
#endif
import           Control.Monad (join, liftM)
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
#if __GLASGOW_HASKELL__ > 704
import           Data.Functor.Identity (Identity (Identity))
#endif


-- mmorph --------------------------------------------------------------------
import           Control.Monad.Morph (MFunctor (hoist), MMonad (embed))


------------------------------------------------------------------------------
-- | 'MonadTransControl' represents the class of monad transformers through
-- which it is possible to lift control operations. See
-- "Documentation.Layers.Overview" for a more complete discussion of how it
-- works.
class (MonadTrans t, Functor (LayerResult t)) => MonadTransControl t where
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
    -- [Preservation] @'liftControl' (\\run -> run t) >>= 'restore'
    --     ≡ t@
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


------------------------------------------------------------------------------
instance Functor (LayerResult (ErrorT e)) where
    fmap f = \(ER r) -> ER (fmap f r)
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


------------------------------------------------------------------------------
instance Functor (LayerResult IdentityT) where
    fmap f = \(IR r) -> IR (f r)
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


------------------------------------------------------------------------------
instance Functor (LayerResult ListT) where
    fmap f = \(LR r) -> LR (fmap f r)
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


------------------------------------------------------------------------------
instance Functor (LayerResult MaybeT) where
    fmap f = \(MR r) -> MR (fmap f r)
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


------------------------------------------------------------------------------
instance Functor (LayerResult (ReaderT r)) where
    fmap f = \(RR r) -> RR (f r)
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


------------------------------------------------------------------------------
instance Functor (LayerResult (StateT s)) where
    fmap f = \(SR r) -> SR (f r)
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


------------------------------------------------------------------------------
instance Functor (LayerResult (L.StateT s)) where
    fmap f = \(SR' r) -> SR' (f r)
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


------------------------------------------------------------------------------
instance Functor (LayerResult (RWST r w s)) where
    fmap f = \(RWSR (a, w)) -> RWSR (f a, w)
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


------------------------------------------------------------------------------
instance Functor (LayerResult (L.RWST r w s)) where
    fmap f = \(RWSR' (a, w)) -> RWSR' (f a, w)
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


------------------------------------------------------------------------------
instance Functor (LayerResult (WriterT w)) where
    fmap f = \(WR (a, w)) -> WR (f a, w)
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


------------------------------------------------------------------------------
instance Functor (LayerResult (L.WriterT w)) where
    fmap f = \(WR' (a, w)) -> WR' (f a, w)
#endif


------------------------------------------------------------------------------
-- | 'liftControl' is a version of 'lift' that makes it possible to lift
-- control operations from the inner monad @m@ to the transformed monad @t m@.
-- It takes a continuation, to which it passes a version of 'peel', which is
-- kind of an \"inverse\" of @lift@.
liftControl :: (MonadTransControl t, Monad (t m), Monad m)
    => ((forall b. t m b -> m (Layer t m b)) -> m a)
    -> t m a
liftControl f = suspend >>= \s -> lift $ f (peel s)
#if __GLASGOW_HASKELL >= 700
{-# INLINABLE liftControl #-}
#else
{-# INLINE liftControl #-}
#endif


------------------------------------------------------------------------------
-- | An often used composition:
-- @'control' f = 'liftControl' f >>= 'restore'@
control :: (MonadTransControl t, Monad (t m), Monad m)
    => ((forall b. t m b -> m (Layer t m b)) -> m (Layer t m a))
    -> t m a
control f = liftControl f >>= restore
#if __GLASGOW_HASKELL >= 700
{-# INLINABLE control #-}
#else
{-# INLINE control #-}
#endif


------------------------------------------------------------------------------
-- | 'liftOp' is a particular application of 'liftControl' that allows lifting
-- control operations of type: @(a -> m b) -> m c@ to @(a -> t m b) -> t m c@.
--
-- For example:
--
-- @'liftOp' . withMVar :: ('MonadTransControl' t, Monad (t IO)) => MVar a -> (a -> t IO b) -> t IO b@
liftOp :: (MonadTransControl t, Monad (t m), Monad m)
    => ((a -> m (Layer t m b)) -> m (Layer t m c))
    -> (a -> t m b)
    -> t m c
liftOp f = \g -> control (\run -> f $ run . g)
#if __GLASGOW_HASKELL >= 700
{-# INLINABLE liftOp #-}
#else
{-# INLINE liftOp #-}
#endif


------------------------------------------------------------------------------
-- | 'liftOp_' is a particular application of 'liftControl' that allows
-- lifting control operations of type: @m a -> m b@ to @t m a -> t m b@.
--
-- For example:
--
-- @'liftOp_' mask_ :: ('MonadTransControl' t, Monad (t IO)) => t IO a -> t IO a@
liftOp_ :: (MonadTransControl t, Monad (t m), Monad m)
    => (m (Layer t m a) -> m (Layer t m b))
    -> t m a
    -> t m b
liftOp_ f = \m -> control (\run -> f $ run m)
#if __GLASGOW_HASKELL >= 700
{-# INLINABLE liftOp_ #-}
#else
{-# INLINE liftOp_ #-}
#endif


------------------------------------------------------------------------------
-- | 'liftDiscard' is a particular application of 'liftControl' that allows
-- lifting control operations of type: @m () -> m a@ to @t m () -> t m a@.
--
-- Note that, while the argument computation @t m ()@ has access to the
-- captured state, all its side-effects in @t@ are discarded. It is run only
-- for its side-effects in @m@.
--
-- For example:
--
-- @'liftDiscard' forkIO :: ('MonadTransControl' t, Monad (t IO)) => t m () -> t m ThreadId@
liftDiscard :: (MonadTransControl t, Monad (t m), Monad m)
    => (m () -> m a)
    -> t m ()
    -> t m a
liftDiscard f m = liftControl $ \run -> f $ liftM (const ()) $ run m
#if __GLASGOW_HASKELL >= 700
{-# INLINABLE liftDiscard #-}
#else
{-# INLINE liftDiscard #-}
#endif


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
-- that lifting operations from @i@ into @m@ is supposed. If @m@ is a monad
-- built from a monad transformer stack, then it supports lifting operations
-- from any monad @i@ anywhere in the stack. We call such a relationship
-- between @i@ and @m@ a \"monad lift\". For a more details, read the in-depth
-- documentation provided in "Documentation.Layers.Overview".
class (Monad i, Monad m) => MonadLift i m where
    -- | 'lift' takes a computation from an inner monad @i@ and lifts it into
    -- the \"outer\" monad @m@.
    --
    -- The following laws hold for valid instances of 'MonadLift':
    --
    --     [Identity] @'lift'' . return ≡ return@
    --
    --     [Composition] @'lift'' m >>= 'lift''' . f ≡ 'lift'' (m >>= f)@
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
    {-# INLINE lift' #-}


#if __GLASGOW_HASKELL__ >= 707
------------------------------------------------------------------------------
-- | 'MonadLiftControl' represents the class of monad lifts that support
-- lifting control operations. See "Documentation.Layers.Overview" for a more
-- complete discussion.
class (MonadLift i m, Functor (LiftResult i m)) => MonadLiftControl i m where
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

    -- | 'extract' inspects a @'LiftResult' i m a@ value and tries to
    -- \"extract\" an @a@ value from it, if possible. This can be used to
    -- detect if any of the layers betweem @i@ and @m@ short-circuited when
    -- the 'LiftResult' was captured (if it did, 'extract'' returns
    -- @Nothing@).
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
instance (Functor (LiftResult i m), Functor (LayerResult t)) =>
    Functor (ComposeResult i t m)
  where
    fmap f (ComposeResult r) = ComposeResult $
        fmap (\(r', s) -> (fmap f r', s)) r
    {-# INLINE fmap #-}


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

    extract' (_ :: proxy i) (_ :: proxy (t m)) (ComposeResult r) =
        join $ extract' (P :: P i) (P :: P m) $
            (fmap (\(r', _) -> extract (P' :: P' t) r') r)
    {-# INLINE extract' #-}


------------------------------------------------------------------------------
data P' (p :: (* -> *) -> * -> *) = P'


------------------------------------------------------------------------------
data P (p :: * -> *) = P


------------------------------------------------------------------------------
-- | 'liftControl'' is a version of 'lift'' that makes it possible to lift
-- control operations from the inner monad @i@ to the outer monad @m@. It
-- takes a continuation, to which it passes a version of 'peel'', which is
-- kind of an \"inverse\" of 'lift''.
liftControl' :: forall i m a. MonadLiftControl i m
    => ((forall b. m b -> i (Lift i m b)) -> i a)
    -> m a
liftControl' f = suspend' (P :: P i) >>= \s -> lift' $ f (peel' s)
{-# INLINABLE liftControl' #-}


------------------------------------------------------------------------------
-- | An often used composition:
--     @'control'' f = 'liftControl'' f >>= 'restore'' (Proxy :: Proxy i)@
control' :: forall i m a. MonadLiftControl i m
    => ((forall b. m b -> i (Lift i m b)) -> i (Lift i m a))
    -> m a
control' f = liftControl' f >>= restore' (P :: P i)
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
-- | An often used composition: @'control'' f = join . 'liftControl'' f@
control' :: MonadLiftControl i m
    => ((forall b. m b -> i (m b)) -> i (m a))
   -> m a
control' = join . liftControl'
#if __GLASGOW_HASKELL >= 700
{-# INLINABLE control' #-}
#else
{-# INLINE control' #-}
#endif


------------------------------------------------------------------------------
liftOp' :: MonadLiftControl i m
    => ((a -> i (m b)) -> i (m c))
    -> (a -> m b)
    -> m c


------------------------------------------------------------------------------
liftOp_' :: MonadLiftControl i m => (i (m a) -> i (m b)) -> m a -> m b
#endif


------------------------------------------------------------------------------
-- | 'liftOp'' is a particular application of 'liftControl'' that allows
-- lifting control operations of type: @(a -> i b) -> i b@ to
-- @'MonadLiftControl' i m => (a -> m b) -> m b@.
--
-- For example:
--
-- @'liftOp'' . withMVar :: 'MonadLiftControl' 'IO' m => MVar a -> (a -> m b) -> m b@
liftOp' f = \g -> control' $ \run -> f $ run . g
#if __GLASGOW_HASKELL >= 700
{-# INLINABLE liftOp' #-}
#else
{-# INLINE liftOp' #-}
#endif


------------------------------------------------------------------------------
-- | 'liftOp_'' is a particular application of 'liftControl'' that allows
-- lifting control operations of type: @i a -> i b@ to
-- @'MonadLiftControl' i m => m a -> m b@.
--
-- For example:
--
-- @'liftOp_'' mask_ :: 'MonadLiftControl' 'IO' m => m a -> m a@
liftOp_' f = \m -> control' $ \run -> f $ run m
#if __GLASGOW_HASKELL >= 700
{-# INLINABLE liftOp_' #-}
#else
{-# INLINE liftOp_' #-}
#endif


------------------------------------------------------------------------------
-- | 'liftDiscard'' is a particular application of 'liftControl'' that allows
-- lifting control operations of type: @i () -> i a@ to
-- @'MonadLiftControl' i m => m () -> m a@.
--
-- Note that, while the argument computation @m ()@ has access to the captured
-- state, all its side-effects in @m@ (relative to @i@) are discarded. It is
-- run only for its
-- side-effects in the inner monad @i@.
--
-- For example:
--
-- @'liftDiscard'' forkIO :: 'MonadLiftControl' 'IO' m => m () -> m ThreadId@
liftDiscard' :: MonadLiftControl i m => (i () -> i a) -> m () -> m a
liftDiscard' f = \m -> liftControl' $ \run -> f $ liftM (const ()) $ run m
#if __GLASGOW_HASKELL >= 700
{-# INLINABLE liftDiscard' #-}
#else
{-# INLINE liftDiscard' #-}
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
    -- The following laws hold for valid instances of 'MonadLiftInvariant':
    --
    --     [Identity] @'hoistiso'' id id ≡ id@
    --
    --     [Composition]
    --         @'hoistiso'' f g . 'hoistiso'' f' g' ≡
    --             'hoistiso'' (f . f') (g' . g)@
    --
    -- The difference between 'hoistiso'' and 'hoistiso' is that
    -- 'hoistiso' only lifts from the monad directly beneath the top of the
    -- stack, while @hoistiso'@ can lift from /any/ monad anywhere in the
    -- stack (including @m@ itself). ('hoistiso' is used to implement
    -- 'hoistiso'').
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
    -- The difference between 'hoist'' and 'hoist' is that 'hoist' only lifts
    -- from the monad directly beneath the top of the stack, while 'hoist''
    -- can lift from /any/ monad anywhere in the stack (including @m@ itself).
    -- ('hoist' is used to implement 'hoist'').
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
