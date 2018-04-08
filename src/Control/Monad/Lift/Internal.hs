{-# LANGUAGE CPP #-}
{-# LANGUAGE RankNTypes #-}

#ifdef LANGUAGE_SafeHaskell
#if __GLASGOW_HASKELL__ >= 704
{-# LANGUAGE Safe #-}
#else
{-# LANGUAGE Trustworthy #-}
#endif
#endif

module Control.Monad.Lift.Internal
    ( coercePeel, coercePeelI
    )
where

#if __GLASGOW_HASKELL__ < 704
-- base ----------------------------------------------------------------------
import           Unsafe.Coerce (unsafeCoerce)


#endif
-- layers --------------------------------------------------------------------
import           Control.Monad.Lift (LayerEffects, OuterEffects)


------------------------------------------------------------------------------
coercePeel :: forall t m. ()
    => (forall a. t m a -> m (LayerEffects t m a))
    -> (forall a. t m a -> m (LayerEffects t m a))
#if __GLASGOW_HASKELL__ >= 704
coercePeel f = f
#else
coercePeel = unsafeCoerce
#endif
{-# INLINE coercePeel #-}


------------------------------------------------------------------------------
coercePeelI :: forall i m. ()
    => (forall a. m a -> i (OuterEffects i m a))
    -> (forall a. m a -> i (OuterEffects i m a))
#if __GLASGOW_HASKELL__ >= 704
coercePeelI f = f
#else
coercePeelI = unsafeCoerce
#endif
{-# INLINE coercePeelI #-}
