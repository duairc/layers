{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE UndecidableInstances #-}

{-|

This module exports:

    1. The type class 'MonadRecover' and its operation 'recover'.

    2. Instances of 'MonadRecover' for every 'MonadPlus' and 'Either'-like
    monad from the @base@ and @transformers@ packages.

    3. A universal pass-through instance of 'MonadRecover' for any existing
    'MonadRecover' wrapped by any 'MonadLayerControl'.

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
import          Control.Monad.Lift (MonadTransControl, control)
import          Control.Monad.Interface.Abort (MonadAbort)


------------------------------------------------------------------------------
-- | The 'MonadRecover' type class represents the subclass of monads which can
-- fail ('MonadAbort') and recover from that failure.
--
-- Minimal complete definition: recover.
class MonadAbort e m => MonadRecover e m | m -> e where
    -- | In addition to the 'MonadAbort' \"zero\" law, the following laws hold
    -- for valid instances of 'MonadRecover';
    --
    --     [Left Identity] @recover (abort e) (\e -> m) = m@
    --     [Right Identity] @recover m abort = m@
    --     [Associativity] @recover m (\_ -> recover n (\_ -> o)) = recover (recover m (\_ -> n)) (\_ -> o)@
    --     [Preservation] @recover (abort e) return = return e@
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
    {-# INLINE recover #-}


------------------------------------------------------------------------------
instance (MonadTransControl t, MonadRecover e m, MonadAbort e (t m)) =>
    MonadRecover e (t m)
  where
    recover m h = control (\run -> recover (run m) (run . h))
    {-# INLINE recover #-}
