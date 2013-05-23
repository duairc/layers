{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE UndecidableInstances #-}

{-|
-}

module Control.Monad.Interface.Recover
    ( MonadRecover (recover)
    )
where

-- base ----------------------------------------------------------------------
import          Control.Exception (SomeException, catch)
import          Control.Monad (mplus)
import          GHC.Conc.Sync (STM, catchSTM)
#if !MIN_VERSION_base(4, 6, 0)
import          Prelude hiding (catch)
#endif


-- transformers --------------------------------------------------------------
import          Control.Monad.Trans.Error (ErrorT (ErrorT), Error)
import          Control.Monad.Trans.Maybe (MaybeT)
import          Control.Monad.Trans.List (ListT)
import          Data.Functor.Product (Product (Pair))


-- layers --------------------------------------------------------------------
import          Control.Monad.Layer
                    ( MonadLayer (type Inner)
                    , MonadLayerControl
                    , controlLayer
                    )
import          Control.Monad.Interface.Abort (MonadAbort)


------------------------------------------------------------------------------
-- | The 'MonadRecover' type class represents the class of monads which can
-- fail, and, if possible, store a value of type @e@ in doing so.
--
-- Minimal complete definition: recover.
class MonadAbort e m => MonadRecover e m | m -> e where
    -- | Recover from a given exception.
    recover :: m a -> (e -> m a) -> m a


------------------------------------------------------------------------------
instance MonadRecover e (Either e) where
    recover m h = either h Right m
    {-# INLINE recover #-}


------------------------------------------------------------------------------
instance MonadRecover () ([]) where
    recover m h = mplus m (h ())
    {-# INLINE recover #-}


------------------------------------------------------------------------------
instance MonadRecover () Maybe where
    recover m h = mplus m (h ())
    {-# INLINE recover #-}


------------------------------------------------------------------------------
instance MonadRecover SomeException IO where
    recover = catch
    {-# INLINE recover #-}


------------------------------------------------------------------------------
instance MonadRecover SomeException STM where
    recover = catchSTM
    {-# INLINE recover #-}


------------------------------------------------------------------------------
instance (Error e, Monad m) => MonadRecover e (ErrorT e m) where
    recover (ErrorT m) h = ErrorT $ m >>= either
        (\e -> let ErrorT m' = h e in m')
        (return . Right)
    {-# INLINE recover #-}


------------------------------------------------------------------------------
instance Monad m => MonadRecover () (ListT m) where
    recover m h = mplus m (h ())
    {-# INLINE recover #-}


------------------------------------------------------------------------------
instance Monad m => MonadRecover () (MaybeT m) where
    recover m h = mplus m (h ())
    {-# INLINE recover #-}


------------------------------------------------------------------------------
instance (MonadRecover e f, MonadRecover e g) => MonadRecover e (Product f g)
  where
    recover (Pair f g) h = Pair
        (recover f (\e -> let Pair f' _ = h e in f'))
        (recover g (\e -> let Pair _ g' = h e in g'))


------------------------------------------------------------------------------
#if __GLASGOW_HASKELL__ >= 702
instance (MonadLayerControl m, MonadRecover e (Inner m)) =>
#else
instance
    ( MonadLayerControl m
    , MonadRecover e (Inner m)
    , MonadAbort e m
    ) =>
#endif
    MonadRecover e m
  where
    recover m h = controlLayer (\run -> recover (run m) (run . h))
    {-# INLINE recover #-}
