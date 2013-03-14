{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

{-|

This module is the core of the @layers@ package. It exports:

    1. The type classes 'MonadLayer', 'MonadLayerFunctor' and
    'MonadLayerControl' and instances of these classes for the transformers in
    the @transformers@ package.

    2. The type classes 'MonadTrans', 'MonadTransFunctor' and
    'MonadTransControl' and instances of these classes for the transformers in
    the @transformers@ package.

    3. The type classes 'MonadLift', 'MonadLiftFunctor' and
    'MonadLiftControl'.

    4. Two sets of helper functions inspired by similarly named functions in
    the @monad-control@ package: one for instances of @MonadLayerControl@ and
    the other for instances of @MonadLiftControl@. These operations are:
    'controlLayer', 'layerOp', 'layerOp_' and 'layerDiscard' and 'control',
    'liftOp', 'liftOp_' and 'liftDiscard' respectively.

-}

module Control.Monad.Layer
    (
      -- * The @MonadLayer@ family
      MonadLayer (type Inner, layer, layerInvmap)
    , MonadLayerFunctor (layerMap)
    , MonadLayerControl (type LayerState, zero, restore, layerControl)

      -- * The @MonadTrans@ family
    , MonadTrans (type Outer, transInvmap)
    , MonadTransFunctor (transMap)
    , MonadTransControl (transControl)

      -- * The @MonadLift@ family
    , MonadLift (lift, liftInvmap)
    , MonadLiftFunctor (liftMap)
    , MonadLiftControl (liftControl)

      -- * Helper functions
      -- ** @MonadLayerControl@
      -- | These functions are mainly used for writing universal pass-through
      -- instances for monad interfaces. If you are not doing this, you're
      -- probably looking for the analogous functions for 'MonadLiftControl'
      -- (see below).
    , controlLayer
    , layerOp
    , layerOp_
    , layerDiscard

      -- ** @MonadLiftControl@
    , control
    , liftOp
    , liftOp_
    , liftDiscard
    )
where

-- base ----------------------------------------------------------------------
import           Control.Monad (join, liftM)
import           Data.Maybe (isNothing)
import           Data.Monoid (Monoid (mempty))


-- transformers --------------------------------------------------------------
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



------------------------------------------------------------------------------
-- | A monad @m@ can be an instance of 'MonadLayer' if it is built
-- (\"layered\") on top of some inner monad (@'Inner' m@) and can provide the
-- operations 'layer' and 'layerInvmap'.
--
-- Monad layers are a generalisation of monad transformers, with the
-- difference being that monad layers are not necessarily parametric in their
-- inner monad. For more details, read the the in-depth documentation
-- provided in "Documentation.Layers.Overview".
class (Monad m, Monad (Inner m)) => MonadLayer m where
    -- | The inner monad on top of which @m@ is built.
    type Inner m :: * -> *

    -- | 'layer' takes a computation from the inner monad @'Inner' m@ and
    -- lifts it into the \"outer\" monad @m@.
    --
    -- The following laws hold for valid instances of 'MonadLayer':
    --
    --     [Identity] @layer . return = return@
    --
    --     [Composition] @layer m >>= layer . f = layer (m >>= f)@
    --
    -- These laws are equivalent to the monad transformer laws of the
    -- 'Control.Monad.Trans.Class.MonadTrans' class from the @transformers@,
    -- where @layer@ is analgous to the 'Control.Monad.Trans.Class.lift'
    -- operation from @MonadTrans@.
    layer :: Inner m a -> m a

    -- | 'layerInvmap' represents an invariant (endo)functor in the category
    -- of monads. It takes a transformation @f@ of the inner monad and its
    -- inverse @g@ (such that @g . f = id@) and returns transformation of @m@
    -- analogous to @f@. (i.e., @layerInvmap@ lifts an automorphism of
    -- @'Inner' m@ to an endomorphism of @m@).
    --
    -- The following laws hold for valid instances of 'MonadLayer':
    --
    --     [Identity] @layerInvmap id id = id@
    --
    --     [Composition]
    --         @layerInvmap f g . layerInvmap f' g' = layerInvmap (f . f') (g' . g)@
    layerInvmap
        :: (forall b. Inner m b -> Inner m b) -- ^ f
        -> (forall b. Inner m b -> Inner m b) -- ^ g
        -> m a
        -> m a


------------------------------------------------------------------------------
-- | The type class 'MonadLayerFunctor' represents is the subclass of
-- monad layers that support the 'layerMap' operation, which is more powerful
-- than the 'layerInvmap' operation of the 'MonadLayer' type class.
class MonadLayer m => MonadLayerFunctor m where
    -- | 'layerMap' represents an (endo)functor in the category of monads. It
    -- takes a transformation @f@ of the inner monad returns a transformation
    -- of @m@ analogous to @f@. (i.e., @layerMap@ lifts an endomorphism of
    -- @'Inner' m@ to a endomorphism of @m@).
    --
    -- The following laws hold for valid instances of 'MonadLayerFunctor':
    --
    --     [Identity] @layerMap id = id@
    --
    --     [Composition]
    --         @layerMap f . layerMap g = layerMap (f . g)@
    layerMap :: (forall b. Inner m b -> Inner m b) -> m a -> m a


------------------------------------------------------------------------------
-- | 'MonadLayerControl' represents the class of monad layers through which it
-- is possible to lift control operations. See "Documentation.Layers.Overview"
-- for a more complete discussion.
class MonadLayerFunctor m => MonadLayerControl m where
    -- | The portion of the monadic state of @m@ that is independent of
    -- @'Inner' m@.
    data LayerState m :: * -> *

    -- | 'zero' inspects a 'LayerState' value and determines whether or not it
    -- is a \"zero\" value in the monad @m@ (i.e., if @m@ had short-circuited
    -- when the @LayerState@ was captured). (This is used to implement the
    -- universal pass-through instance of
    -- 'Control.Monad.Interface.Try.MonadTry'.)
    zero :: LayerState m a -> Bool

    -- | Construct a @m@ computation from the monadic state of @m@ that is
    -- returned from a @run@ function given by @layerControl@.
    --
    -- Instances should satisfy the following law:
    --
    -- [Preservation] @layerControl (\\run -> run t) >>= restore = t@
    restore :: LayerState m a -> m a
 
    -- | 'layerControl' is a version of 'layer' that makes it possible to lift
    -- control operations from the inner monad @'Inner' m@ to the \"outer\"
    -- monad @m@. It takes a continuation, to which it passes an operation we
    -- call @run@, which is a kind of \"inverse\" of @layer@.
    --
    -- Instances should satisfy similar laws as the monad transformer laws:
    --
    -- [Identity] @layerControl . const . return = return@
    --
    -- [Composition]
    --     @layerControl (const m) >>= layerControl . const . f@ = @layerControl (const (m >>= f))@
    layerControl
        :: ((forall b. m b -> Inner m (LayerState m b)) -> Inner m a)
        -> m a

    zero _ = False
    {-# INLINE zero #-}


------------------------------------------------------------------------------
-- | Monad transformers are a subclass of monad layers which are parametric in
-- their inner monad.
class (MonadLayer m, m ~ Outer m (Inner m)) => MonadTrans m where
    -- | @'Outer' m@ is the monad constructor from which @m@ is built (by
    -- applying it to @'Inner' m@).
    type Outer m :: (* -> *) -> * -> *

    -- | 'transInvmap' represents an invariant functor in the category of
    -- monads. It takes a transformation @f@ from the inner monad and its
    -- inverse @g@ (such that @g . f = id@) and returns transformation of @m@
    -- analogous to @f@. (i.e., @layerInvmap@ lifts an isomorphism from
    -- @'Inner' m@ to a homomorphism of @m@).
    --
    -- The following laws hold for valid instances of 'MonadTrans':
    --
    --     [Identity] @transInvmap id id = id@
    --
    --     [Composition]
    --         @transInvmap f g . transInvmap f' g' = transInvmap (f . f') (g' . g)@
    --
    -- Note: this is more powerful than 'layerInvmap' from the 'MonadLayer'
    -- type class because the transformation it produces is a homomorphism
    -- rather than just an endomorphism.
    transInvmap :: (MonadTrans n, Outer n ~ Outer m)
        => (forall b. Inner m b -> Inner n b)
        -> (forall b. Inner n b -> Inner m b)
        -> m a
        -> n a


------------------------------------------------------------------------------
-- | The type class 'MonadTransFunctor' represents is the subclass of
-- monad layers that support the 'transMap' operation, which is more powerful
-- than the 'transInvmap' operation of the 'MonadTrans' type class.
class (MonadLayerFunctor m, MonadTrans m) => MonadTransFunctor m where
    -- | 'transMap' represents a functor in the category of monads. It takes
    -- a transformation @f@ from the inner monad returns a transformation from
    -- @m@ analogous to @f@. (i.e., @transMap@ lifts a homomorphism from
    -- @'Inner' m@ to a homomorphism from @m@).
    --
    -- The following laws hold for valid instances of 'MonadTransFunctor':
    --
    --     [Identity] @transMap id = id@
    --
    --     [Composition] @transMap f . transMap g = transMap (f . g)@
    --
    -- Note: this is more powerful than 'layerMap' from the 
    -- 'MonadLayerFunctor' type class because the transformation it produces
    -- is a homomorphism rather than just an endomorphism.
    transMap :: (MonadTrans n, Outer n ~ Outer m)
        => (forall b. Inner m b -> Inner n b)
        -> m a
        -> n a


------------------------------------------------------------------------------
-- | 'MonadTransControl' is a variant of 'MonadLayerControl' for monad
-- transformers, i.e., monad layers polymorphic in their inner monad. This
-- extra polymorphism allows us to specify more type safety in the @run@
-- operation of 'transControl', but in practice there is no reason to ever use
-- this over @MonadLayerControl@. See "Documentation.Layers.Overview" for a
-- discussion of why this class is included despite not being strictly
-- necessary.
class (MonadLayerControl m, MonadTransFunctor m) => MonadTransControl m where
    -- | 'transControl' is a version of 'layerControl' whose @run@ operation
    -- is more polymorphic in the returned monad. This provides a static
    -- guarantee that there are no remaining side effects in @m@ in the action
    -- returned by @run@ which is not possible to express with the types of
    -- 'MonadLayerControl'. In practice though there should be no reason to
    -- use this over 'layerControl'.
    transControl
        :: (forall n. (MonadTrans n, Outer n ~ Outer m)
            => (forall b. n b -> Inner n (LayerState n b))
            -> Inner n a)
        -> m a


------------------------------------------------------------------------------
instance Monad m => MonadLayer (ContT r m) where
    type Inner (ContT r m) = m
    layer = ContT . (>>=)
    {-# INLINE layer #-}
    layerInvmap f g (ContT m) = ContT $ f . m . (g .)
    {-# INLINE layerInvmap #-}


------------------------------------------------------------------------------
instance Monad m => MonadTrans (ContT r m) where
    type Outer (ContT r m) = ContT r
    transInvmap f g (ContT m) = ContT $ f . m . (g .)
    {-# INLINE transInvmap #-}


------------------------------------------------------------------------------
instance (Error e, Monad m) => MonadLayer (ErrorT e m) where
    type Inner (ErrorT e m) = m
    layer = ErrorT . liftM Right
    {-# INLINE layer #-}
    layerInvmap = const . layerMap
    {-# INLINE layerInvmap #-}


------------------------------------------------------------------------------
instance (Error e, Monad m) => MonadLayerFunctor (ErrorT e m) where
    layerMap f (ErrorT m) = ErrorT $ f m
    {-# INLINE layerMap #-}


------------------------------------------------------------------------------
instance (Error e, Monad m) => MonadLayerControl (ErrorT e m) where
    newtype LayerState (ErrorT e m) a = E {unE :: Either e a}
    zero = either (const True) (const False) . unE
    {-# INLINE zero #-}
    restore = ErrorT . return . unE
    {-# INLINE restore #-}
    layerControl f = layer $ f (\(ErrorT m) -> liftM E m)
    {-# INLINE layerControl #-}


------------------------------------------------------------------------------
instance (Error e, Monad m) => MonadTrans (ErrorT e m) where
    type Outer (ErrorT e m) = ErrorT e
    transInvmap = const . transMap
    {-# INLINE transInvmap #-}


------------------------------------------------------------------------------
instance (Error e, Monad m) => MonadTransFunctor (ErrorT e m) where
    transMap f (ErrorT m) = ErrorT $ f m
    {-# INLINE transMap #-}


------------------------------------------------------------------------------
instance (Error e, Monad m) => MonadTransControl (ErrorT e m) where
    transControl f = layer $ f (\(ErrorT m) -> liftM E m)
    {-# INLINE transControl #-}


------------------------------------------------------------------------------
instance Monad m => MonadLayer (IdentityT m) where
    type Inner (IdentityT m) = m
    layer = IdentityT
    {-# INLINE layer #-}
    layerInvmap = const . layerMap
    {-# INLINE layerInvmap #-}


------------------------------------------------------------------------------
instance Monad m => MonadLayerFunctor (IdentityT m) where
    layerMap f (IdentityT m) = IdentityT $ f m
    {-# INLINE layerMap #-}


------------------------------------------------------------------------------
instance Monad m => MonadLayerControl (IdentityT m) where
    newtype LayerState (IdentityT m) a = I {unI :: a}
    restore = IdentityT . return . unI
    {-# INLINE restore #-}
    layerControl f = layer $ f (\(IdentityT m) -> liftM I m)
    {-# INLINE layerControl #-}


------------------------------------------------------------------------------
instance Monad m => MonadTrans (IdentityT m) where
    type Outer (IdentityT m) = IdentityT
    transInvmap = const . transMap
    {-# INLINE transInvmap #-}


------------------------------------------------------------------------------
instance Monad m => MonadTransFunctor (IdentityT m) where
    transMap f (IdentityT m) = IdentityT $ f m
    {-# INLINE transMap #-}


------------------------------------------------------------------------------
instance Monad m => MonadTransControl (IdentityT m) where
    transControl f = layer $ f (\(IdentityT m) -> liftM I m)
    {-# INLINE transControl #-}


------------------------------------------------------------------------------
instance Monad m => MonadLayer (ListT m) where
    type Inner (ListT m) = m
    layer = ListT . liftM (:[])
    {-# INLINE layer #-}
    layerInvmap = const . layerMap
    {-# INLINE layerInvmap #-}


------------------------------------------------------------------------------
instance Monad m => MonadLayerFunctor (ListT m) where
    layerMap f (ListT m) = ListT $ f m
    {-# INLINE layerMap #-}


------------------------------------------------------------------------------
instance Monad m => MonadLayerControl (ListT m) where
    newtype LayerState (ListT m) a = L {unL :: [a]}
    zero = null . unL
    {-# INLINE zero #-}
    restore = ListT . return . unL
    {-# INLINE restore #-}
    layerControl f = layer $ f (\(ListT m) -> liftM L m)
    {-# INLINE layerControl #-}


------------------------------------------------------------------------------
instance Monad m => MonadTrans (ListT m) where
    type Outer (ListT m) = ListT
    transInvmap = const . transMap
    {-# INLINE transInvmap #-}


------------------------------------------------------------------------------
instance Monad m => MonadTransFunctor (ListT m) where
    transMap f (ListT m) = ListT $ f m
    {-# INLINE transMap #-}


------------------------------------------------------------------------------
instance Monad m => MonadTransControl (ListT m) where
    transControl f = layer $ f (\(ListT m) -> liftM L m)
    {-# INLINE transControl #-}


------------------------------------------------------------------------------
instance Monad m => MonadLayer (MaybeT m) where
    type Inner (MaybeT m) = m
    layer = MaybeT . liftM Just
    {-# INLINE layer #-}
    layerInvmap = const . layerMap
    {-# INLINE layerInvmap #-}


------------------------------------------------------------------------------
instance Monad m => MonadLayerFunctor (MaybeT m) where
    layerMap f (MaybeT m) = MaybeT $ f m
    {-# INLINE layerMap #-}


------------------------------------------------------------------------------
instance Monad m => MonadLayerControl (MaybeT m) where
    newtype LayerState (MaybeT m) a = M {unM :: Maybe a}
    zero = isNothing . unM
    {-# INLINE zero #-}
    restore = MaybeT . return . unM
    {-# INLINE restore #-}
    layerControl f = layer $ f (\(MaybeT m) -> liftM M m)
    {-# INLINE layerControl #-}


------------------------------------------------------------------------------
instance Monad m => MonadTrans (MaybeT m) where
    type Outer (MaybeT m) = MaybeT
    transInvmap = const . transMap
    {-# INLINE transInvmap #-}


------------------------------------------------------------------------------
instance Monad m => MonadTransFunctor (MaybeT m) where
    transMap f (MaybeT m) = MaybeT $ f m
    {-# INLINE transMap #-}


------------------------------------------------------------------------------
instance Monad m => MonadTransControl (MaybeT m) where
    transControl f = layer $ f (\(MaybeT m) -> liftM M m)
    {-# INLINE transControl #-}


------------------------------------------------------------------------------
instance Monad m => MonadLayer (ReaderT r m) where
    type Inner (ReaderT r m) = m
    layer = ReaderT . const
    {-# INLINE layer #-}
    layerInvmap = const . layerMap
    {-# INLINE layerInvmap #-}


------------------------------------------------------------------------------
instance Monad m => MonadLayerFunctor (ReaderT r m) where
    layerMap f (ReaderT m) = ReaderT $ f . m
    {-# INLINE layerMap #-}


------------------------------------------------------------------------------
instance Monad m => MonadLayerControl (ReaderT r m) where
    newtype LayerState (ReaderT r m) a = R {unR :: a}
    restore = return . unR
    {-# INLINE restore #-}
    layerControl f = ReaderT $ \r -> f (\(ReaderT m) -> liftM R (m r))
    {-# INLINE layerControl #-}


------------------------------------------------------------------------------
instance Monad m => MonadTrans (ReaderT r m) where
    type Outer (ReaderT r m) = ReaderT r
    transInvmap = const . transMap
    {-# INLINE transInvmap #-}


------------------------------------------------------------------------------
instance Monad m => MonadTransFunctor (ReaderT r m) where
    transMap f (ReaderT m) = ReaderT $ f . m
    {-# INLINE transMap #-}


------------------------------------------------------------------------------
instance Monad m => MonadTransControl (ReaderT r m) where
    transControl f = ReaderT $ \r -> f (\(ReaderT m) -> liftM R (m r))
    {-# INLINE transControl #-}


------------------------------------------------------------------------------
instance (Monad m, Monoid w) => MonadLayer (L.RWST r w s m) where
    type Inner (L.RWST r w s m) = m
    layer m = L.RWST $ \_ s -> liftM (\a -> (a, s, mempty)) m
    {-# INLINE layer #-}
    layerInvmap = const . layerMap
    {-# INLINE layerInvmap #-}


------------------------------------------------------------------------------
instance (Monad m, Monoid w) => MonadLayerFunctor (L.RWST r w s m) where
    layerMap f (L.RWST m) = L.RWST $ (f .) . m
    {-# INLINE layerMap #-}


------------------------------------------------------------------------------
instance (Monad m, Monoid w) => MonadLayerControl (L.RWST r w s m) where
    newtype LayerState (L.RWST r w s m) a = RWSL {unRWSL :: (a, s, w)}
    restore = L.RWST . const . const . return . unRWSL
    {-# INLINE restore #-}
    layerControl f = L.RWST $ \r s -> liftM (\a -> (a, s, mempty)) $
        f (\(L.RWST m) -> liftM RWSL (m r s))
    {-# INLINE layerControl #-}


------------------------------------------------------------------------------
instance (Monad m, Monoid w) => MonadTransControl (L.RWST r w s m) where
    transControl f = L.RWST $ \r s -> liftM (\a -> (a, s, mempty)) $
        f (\(L.RWST m) -> liftM RWSL (m r s))
    {-# INLINE transControl #-}


------------------------------------------------------------------------------
instance (Monad m, Monoid w) => MonadTrans (L.RWST r w s m) where
    type Outer (L.RWST r w s m) = L.RWST r w s
    transInvmap = const . transMap
    {-# INLINE transInvmap #-}


------------------------------------------------------------------------------
instance (Monad m, Monoid w) => MonadTransFunctor (L.RWST r w s m) where
    transMap f (L.RWST m) = L.RWST $ (f .) . m
    {-# INLINE transMap #-}


------------------------------------------------------------------------------
instance (Monad m, Monoid w) => MonadLayer (RWST r w s m) where
    type Inner (RWST r w s m) = m
    layer m = RWST $ \_ s -> liftM (\a -> (a, s, mempty)) m
    {-# INLINE layer #-}
    layerInvmap = const . layerMap
    {-# INLINE layerInvmap #-}


------------------------------------------------------------------------------
instance (Monad m, Monoid w) => MonadLayerFunctor (RWST r w s m) where
    layerMap f (RWST m) = RWST $ (f .) . m
    {-# INLINE layerMap #-}


------------------------------------------------------------------------------
instance (Monad m, Monoid w) => MonadLayerControl (RWST r w s m) where
    newtype LayerState (RWST r w s m) a = RWS {unRWS :: (a, s, w)}
    restore = RWST . const . const . return . unRWS
    {-# INLINE restore #-}
    layerControl f = RWST $ \r s -> liftM (\a -> (a, s, mempty)) $
        f (\(RWST m) -> liftM RWS (m r s))
    {-# INLINE layerControl #-}


------------------------------------------------------------------------------
instance (Monad m, Monoid w) => MonadTrans (RWST r w s m) where
    type Outer (RWST r w s m) = RWST r w s
    transInvmap = const . transMap
    {-# INLINE transInvmap #-}


------------------------------------------------------------------------------
instance (Monad m, Monoid w) => MonadTransFunctor (RWST r w s m) where
    transMap f (RWST m) = RWST $ (f .) . m
    {-# INLINE transMap #-}


------------------------------------------------------------------------------
instance (Monad m, Monoid w) => MonadTransControl (RWST r w s m) where
    transControl f = RWST $ \r s -> liftM (\a -> (a, s, mempty)) $
        f (\(RWST m) -> liftM RWS (m r s))
    {-# INLINE transControl #-}


------------------------------------------------------------------------------
instance Monad m => MonadLayer (L.StateT s m) where
    type Inner (L.StateT s m) = m
    layer m = L.StateT $ \s -> liftM (\a -> (a, s)) m
    {-# INLINE layer #-}
    layerInvmap = const . layerMap
    {-# INLINE layerInvmap #-}


------------------------------------------------------------------------------
instance Monad m => MonadLayerFunctor (L.StateT s m) where
    layerMap f (L.StateT m) = L.StateT $ f . m
    {-# INLINE layerMap #-}


------------------------------------------------------------------------------
instance Monad m => MonadLayerControl (L.StateT s m) where
    newtype LayerState (L.StateT s m) a = SL {unSL :: (a, s)}
    restore = L.StateT . const . return . unSL
    {-# INLINE restore #-}
    layerControl f = L.StateT $ \s -> liftM (\a -> (a, s)) $
        f (\(L.StateT m) -> liftM SL (m s))
    {-# INLINE layerControl #-}


------------------------------------------------------------------------------
instance Monad m => MonadTrans (L.StateT s m) where
    type Outer (L.StateT s m) = L.StateT s
    transInvmap = const . transMap
    {-# INLINE transInvmap #-}


------------------------------------------------------------------------------
instance Monad m => MonadTransFunctor (L.StateT s m) where
    transMap f (L.StateT m) = L.StateT $ f . m
    {-# INLINE transMap #-}


------------------------------------------------------------------------------
instance Monad m => MonadTransControl (L.StateT s m) where
    transControl f = L.StateT $ \s -> liftM (\a -> (a, s)) $
        f (\(L.StateT m) -> liftM SL (m s))
    {-# INLINE transControl #-}


------------------------------------------------------------------------------
instance Monad m => MonadLayer (StateT s m) where
    type Inner (StateT s m) = m
    layer m = StateT $ \s -> liftM (\a -> (a, s)) m
    {-# INLINE layer #-}
    layerInvmap = const . layerMap
    {-# INLINE layerInvmap #-}


------------------------------------------------------------------------------
instance Monad m => MonadLayerFunctor (StateT s m) where
    layerMap f (StateT m) = StateT $ f . m
    {-# INLINE layerMap #-}


------------------------------------------------------------------------------
instance Monad m => MonadLayerControl (StateT s m) where
    newtype LayerState (StateT s m) a = S {unS :: (a, s)}
    restore = StateT . const . return . unS
    {-# INLINE restore #-}
    layerControl f = StateT $ \s -> liftM (\a -> (a, s)) $
        f (\(StateT m) -> liftM S (m s))
    {-# INLINE layerControl #-}


------------------------------------------------------------------------------
instance Monad m => MonadTrans (StateT s m) where
    type Outer (StateT s m) = StateT s
    transInvmap = const . transMap
    {-# INLINE transInvmap #-}


------------------------------------------------------------------------------
instance Monad m => MonadTransFunctor (StateT s m) where
    transMap f (StateT m) = StateT $ f . m
    {-# INLINE transMap #-}


------------------------------------------------------------------------------
instance Monad m => MonadTransControl (StateT s m) where
    transControl f = StateT $ \s -> liftM (\a -> (a, s)) $
        f (\(StateT m) -> liftM S (m s))
    {-# INLINE transControl #-}


------------------------------------------------------------------------------
instance (Monad m, Monoid w) => MonadLayer (L.WriterT w m) where
    type Inner (L.WriterT w m) = m
    layer = L.WriterT . liftM (\a -> (a, mempty))
    {-# INLINE layer #-}
    layerInvmap = const . layerMap
    {-# INLINE layerInvmap #-}


------------------------------------------------------------------------------
instance (Monad m, Monoid w) => MonadLayerFunctor (L.WriterT w m) where
    layerMap f (L.WriterT m) = L.WriterT (f m)
    {-# INLINE layerMap #-}


------------------------------------------------------------------------------
instance (Monad m, Monoid w) => MonadLayerControl (L.WriterT w m) where
    newtype LayerState (L.WriterT w m) a = WL {unWL :: (a, w)}
    restore = L.WriterT . return . unWL
    {-# INLINE restore #-}
    layerControl f = layer $ f (\(L.WriterT m) -> liftM WL m)
    {-# INLINE layerControl #-}


------------------------------------------------------------------------------
instance (Monad m, Monoid w) => MonadTrans (L.WriterT w m) where
    type Outer (L.WriterT w m) = L.WriterT w
    transInvmap = const . transMap
    {-# INLINE transInvmap #-}


------------------------------------------------------------------------------
instance (Monad m, Monoid w) => MonadTransFunctor (L.WriterT w m) where
    transMap f (L.WriterT m) = L.WriterT (f m)
    {-# INLINE transMap #-}


------------------------------------------------------------------------------
instance (Monad m, Monoid w) => MonadTransControl (L.WriterT w m) where
    transControl f = layer $ f (\(L.WriterT m) -> liftM WL m)
    {-# INLINE transControl #-}


------------------------------------------------------------------------------
instance (Monad m, Monoid w) => MonadLayer (WriterT w m) where
    type Inner (WriterT w m) = m
    layer = WriterT . liftM (\a -> (a, mempty))
    {-# INLINE layer #-}
    layerInvmap = const . layerMap
    {-# INLINE layerInvmap #-}


------------------------------------------------------------------------------
instance (Monad m, Monoid w) => MonadLayerFunctor (WriterT w m) where
    layerMap f (WriterT m) = WriterT (f m)
    {-# INLINE layerMap #-}


------------------------------------------------------------------------------
instance (Monad m, Monoid w) => MonadLayerControl (WriterT w m) where
    newtype LayerState (WriterT w m) a = W {unW :: (a, w)}
    restore = WriterT . return . unW
    {-# INLINE restore #-}
    layerControl f = layer $ f (\(WriterT m) -> liftM W m)
    {-# INLINE layerControl #-}


------------------------------------------------------------------------------
instance (Monad m, Monoid w) => MonadTrans (WriterT w m) where
    type Outer (WriterT w m) = WriterT w
    transInvmap = const . transMap
    {-# INLINE transInvmap #-}


------------------------------------------------------------------------------
instance (Monad m, Monoid w) => MonadTransFunctor (WriterT w m) where
    transMap f (WriterT m) = WriterT (f m)
    {-# INLINE transMap #-}


------------------------------------------------------------------------------
instance (Monad m, Monoid w) => MonadTransControl (WriterT w m) where
    transControl f = layer $ f (\(WriterT m) -> liftM W m)
    {-# INLINE transControl #-}


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
    --     [Identity] @lift . return = return@
    --
    --     [Composition] @lift m >>= lift . f = lift (m >>= f)@
    --
    -- These laws are equivalent to the monad transformer laws of the
    -- 'Control.Monad.Trans.Class.MonadTrans' class from the @transformers@
    -- package.
    lift :: i a -> m a

    -- | 'liftInvmap' represents an invariant (endo)functor in the category
    -- of monads. It takes a transformation @f@ of an inner monad @i@ and its
    -- inverse @g@ (such that @g . f = id@) and returns transformation of @m@
    -- analogous to @f@. (i.e., @liftInvmap@ lifts an automorphism of @i@
    -- to an endomorphism of @m@).
    --
    -- The following laws hold for valid instances of 'MonadLift':
    --
    --     [Identity] @liftInvmap id id = id@
    --
    --     [Composition]
    --         @liftInvmap f g . liftInvmap f' g' = liftInvmap (f . f') (g' . g)@
    --
    -- The difference between 'liftInvmap' and 'layerInvmap' is that
    -- @layerInvmap@ only lifts from the monad directly beneath the top of the
    -- stack, while @liftInvmap@ can lift from /any/ monad anywhere in the
    -- stack (including @m@ itself). (@layerInvmap@ is used to implement
    -- @liftInvmap@)
    liftInvmap
        :: (forall b. i b -> i b) -> (forall b. i b -> i b) -> m a -> m a


------------------------------------------------------------------------------
-- | The type class 'MonadLiftFunctor' represents is the subclass of monad
-- lifts that support the 'liftMap' operation, which is more powerful than the
-- 'liftInvmap' operation of the 'MonadLift' type class.
class MonadLift i m => MonadLiftFunctor i m where
    -- | 'liftMap' represents an (endo)functor in the category of monads. It
    -- takes a transformation @f@ of an inner monad @i@ returns a
    -- transformation of @m@ analogous to @f@. (i.e., @liftInvmap@ lifts an
    -- endomorphism of @i@ to an endomorphism of @m@).
    --
    -- The following laws hold for valid instances of 'MonadLiftFunctor':
    --
    --     [Identity] @liftMap id = id@
    --
    --     [Composition] @liftMap f . liftMap g = liftMap (f . g)@
    --
    -- The difference between 'liftMap' and 'layerMap' is that @layerMap@ only
    -- lifts from the monad directly beneath the top of the stack, while
    -- @liftMap@ can lift from /any/ monad anywhere in the stack (including
    -- @m@ itself). (@layerMap@ is used to implement @liftMap@)
    liftMap :: (forall b. i b -> i b) -> m a -> m a


------------------------------------------------------------------------------
-- | 'MonadLiftControl' represents the class of monad lifts that support
-- lifting control operations. See "Documentation.Layers.Overview" for a more
-- complete discussion.
class MonadLiftFunctor i m => MonadLiftControl i m where
    -- | 'liftControl' is a version of 'lift' that makes it possible to lift
    -- control operations from an inner monad @i@ to the \"outer\" monad @m@.
    -- It takes a continuation, to which it passes an operation we call @run@,
    -- which is a kind of \"inverse\" of @lift@.
    --
    -- Instances should satisfy the following laws:
    --
    -- [Identity] @liftControl . const . return = return@
    --
    -- [Composition]
    --     @liftControl (const m) >>= liftControl . const . f@ = @liftControl (const (m >>= f))@
    --
    -- [Preservation] @join (liftControl (\\run -> run t)) = t@
    liftControl :: ((forall b. m b -> i (m b)) -> i a) -> m a


------------------------------------------------------------------------------
instance Monad m => MonadLift m m where
    lift = id
    {-# INLINE lift #-}
    liftInvmap f _ = f
    {-# INLINE liftInvmap #-}


------------------------------------------------------------------------------
instance (MonadLayer m, MonadLift i (Inner m)) => MonadLift i m where
    lift = layer . lift
    {-# INLINE lift #-}
    liftInvmap f g = layerInvmap (liftInvmap f g) (liftInvmap g f)
    {-# INLINABLE liftInvmap #-}


------------------------------------------------------------------------------
instance Monad m => MonadLiftFunctor m m where
    liftMap f = f
    {-# INLINE liftMap #-}


------------------------------------------------------------------------------
instance (MonadLayerFunctor m, MonadLiftFunctor i (Inner m)) =>
    MonadLiftFunctor i m
  where
    liftMap f = layerMap (liftMap f)
    {-# INLINE liftMap #-}


------------------------------------------------------------------------------
instance Monad m => MonadLiftControl m m where
    liftControl f = f (liftM return)
    {-# INLINE liftControl #-}


------------------------------------------------------------------------------
instance (MonadLayerControl m, MonadLiftControl i (Inner m)) =>
    MonadLiftControl i m
  where
    liftControl f = layerControl $ \runLayer -> liftControl $ \run ->
        f $ liftM (\m -> layer (lift m) >>= restore) . run . runLayer
    {-# INLINE liftControl #-}


------------------------------------------------------------------------------
-- | An often used composition: @'controlLayer' f = 'layerControl' f >>= 'restore'@
controlLayer :: MonadLayerControl m
    => ((forall b. m b -> Inner m (LayerState m b))
        -> Inner m (LayerState m a))
    -> m a
controlLayer f = layerControl f >>= restore
{-# INLINE controlLayer #-}


------------------------------------------------------------------------------
-- | @layerOp@ is a particular application of 'layerControl' that allows layering
-- control operations of type: @(a -> 'Inner' m b) -> 'Inner' m b@ to
-- @'MonadLayerControl' m => (a -> m b) -> m b@.
--
-- For example:
--
-- @layerOp . withMVar :: ('MonadLayerControl' m, 'Inner' m ~ 'IO')
--     => MVar a -> (a -> m b) -> m b@
layerOp :: MonadLayerControl m
    => ((a -> Inner m (LayerState m b)) -> Inner m (LayerState m c))
    -> (a -> m b)
    -> m c
layerOp f = \g -> controlLayer (\run -> f $ run . g)
{-# INLINE layerOp #-}


------------------------------------------------------------------------------
-- | @layerOp_@ is a particular application of 'layerControl' that allows
-- layering control operations of type: @'Inner' m a -> 'Inner' m b@ to
-- @'MonadLayerControl' m => m a -> m b@.
--
-- For example:
--
-- @layerOp_ mask_ :: ('MonadLayerControl' m, 'Inner' m ~ 'IO') => m a -> m a@
layerOp_ :: MonadLayerControl m
    => (Inner m (LayerState m a) -> Inner m (LayerState m b))
    -> m a
    -> m b
layerOp_ f = \m -> controlLayer (\run -> f $ run m)
{-# INLINE layerOp_ #-}


------------------------------------------------------------------------------
-- | @layerDiscard@ is a particular application of 'layerControl' that allows
-- layering control operations of type: @'Inner' m () -> 'Inner' m a@ to
-- @'MonadLayerControl' m => m () -> m a@.
--
-- Note that, while the argument computation @m ()@ has access to the captured
-- state, all its side-effects in @m@ are discarded. It is run only for its
-- side-effects in the inner monad @'Inner' m@.
--
-- For example:
--
-- @layerDiscard forkIO :: ('MonadLayerControl' m, 'Inner' m ~ 'IO')
--     => m () -> m ThreadId@
layerDiscard :: MonadLayerControl m
    => (Inner m () -> Inner m a)
    -> m ()
    -> m a
layerDiscard f m = layerControl $ \run -> f $ liftM (const ()) $ run m
{-# INLINE layerDiscard #-}


------------------------------------------------------------------------------
-- | An often used composition: @'control' f = join . 'liftControl' f@
control :: MonadLiftControl i m
    => ((forall b. m b -> i (m b)) -> i (m a))
   -> m a
control = join . liftControl
{-# INLINE control #-}


------------------------------------------------------------------------------
-- | @liftOp@ is a particular application of 'liftControl' that allows lifting
-- control operations of type: @(a -> i b) -> i b@ to
-- @'MonadLiftControl' i m => (a -> m b) -> m b@.
--
-- For example:
--
-- @liftOp . withMVar :: 'MonadLiftControl' 'IO' m => MVar a -> (a -> m b) -> m b@
liftOp :: MonadLiftControl i m
    => ((a -> i (m b)) -> i (m c))
    -> (a -> m b)
    -> m c
liftOp f = \g -> control $ \run -> f $ run . g
{-# INLINE liftOp #-}


------------------------------------------------------------------------------
-- | @liftOp_@ is a particular application of 'liftControl' that allows
-- lifting control operations of type: @i a -> i b@ to
-- @'MonadLiftControl' i m => m a -> m b@.
--
-- For example:
--
-- @liftOp_ mask_ :: 'MonadLiftControl' 'IO' m => m a -> m a@
liftOp_ :: MonadLiftControl i m => (i (m a) -> i (m b)) -> m a -> m b
liftOp_ f = \m -> control $ \run -> f $ run m
{-# INLINE liftOp_ #-}


------------------------------------------------------------------------------
-- | @liftDiscard@ is a particular application of 'liftControl' that allows
-- lifting control operations of type: @i () -> i a@ to
-- @'MonadLiftControl' i m => m () -> m a@.
--
-- Note that, while the argument computation @m ()@ has access to the captured
-- state, all its side-effects in @m@ are discarded. It is run only for its
-- side-effects in the inner monad @i@.
--
-- For example:
--
-- @liftDiscard forkIO :: 'MonadLiftControl' 'IO' m => m () -> m ThreadId@
liftDiscard :: MonadLiftControl i m => (i () -> i a) -> m () -> m a
liftDiscard f = \m -> liftControl $ \run -> f $ liftM (const ()) $ run m
{-# INLINE liftDiscard #-}