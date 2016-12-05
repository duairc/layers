{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

#ifdef LANGUAGE_ConstraintKinds
{-# LANGUAGE ConstraintKinds #-}
#endif

#include <docmacros.h>
#include <overlap.h>

{-|

This module defines the 'MonadWriter' G(monadinterface,interface). It is
designed to be compatible with the with the
T(mtl,Control-Monad-Writer-Class,MonadWriter) interface from the H(mtl)
package. It consists of:

  * The 'MonadWriter' constraint.
  * The 'tell', 'listen' and 'pass' operations.

  * Instances of 'MonadWriter':

      * For arbitrary G(innermonad,inner monads) wrapped by one of the
      following G(monadlayer,monad layers):

          * Lazy 'L.WriterT'
          * Strict 'WriterT'
          * Lazy 'L.RWST'
          * Strict 'RWST'

      * G(universalpassthroughinstance,Pass-through instances) for:

          * Any G(innermonad,inner monad) with an existing 'MonadWriter'
          instance wrapped by any G(monadlayer,monad layer) implementing
          'Control.Monad.Lift.MonadTrans'.
          * The 'Product' of any two G(monadictype,monadic types) which both
          have existing 'MonadWriter' instances.
          * The <M(mmorph,Control-Monad-Trans-Compose)#t:ComposeT composition>
          of two G(monadlayer,monad layers) wrapped around an
          G(innermonad,inner monad), where either the
          G(innermonad,inner monad) or one or more of the composed
          G(monadlayer,monad layers) has an existing instance for
          'MonadWriter'.

  * The 'writer', 'listens' and and 'censor' utility operations.

-}

module Monad.Writer
    ( MonadWriter (writer, tell, listen, pass)
    , listens
    , censor
    )
where

-- base ----------------------------------------------------------------------
import           Control.Monad (liftM)
#if !MIN_VERSION_base(4, 8, 0)
import           Data.Monoid (Monoid)
#endif


#if MIN_VERSION_mmorph(1, 0, 1)
-- mmorph --------------------------------------------------------------------
import           Control.Monad.Trans.Compose (ComposeT (ComposeT))


#endif
-- transformers --------------------------------------------------------------
import qualified Control.Monad.Trans.RWS.Lazy as L (RWST (RWST))
import           Control.Monad.Trans.RWS.Strict (RWST (RWST))
import qualified Control.Monad.Trans.Writer.Lazy as L (WriterT (WriterT))
import           Control.Monad.Trans.Writer.Strict (WriterT (WriterT))
#if MIN_VERSION_transformers(0, 3, 0)
import           Data.Functor.Product (Product (Pair))
#endif


-- layers --------------------------------------------------------------------
import           Control.Monad.Lift.Top (MonadTop, liftT)


------------------------------------------------------------------------------
-- | It is often desirable for a G(computation,computation) to generate output
-- \"on the side\". Logging and tracing are the most common examples in which
-- data is generated during a G(computation,computation) that we want to
-- retain but is not the primary result of the G(computaiton,computation).
--
-- Explicitly managing the logging or tracing data can clutter up the code and
-- invite subtle bugs such as missed log entries. The 'MonadWriter'
-- G(monadinterface,interface) provides a cleaner way to manage the output
-- without cluttering the main G(computation,computation).
--
-- Minimal complete definition: 'listen', 'pass' and one of either 'writer' or
-- 'tell'.
class (Monad m, Monoid w) => MonadWriter w m | m -> w where
    -- | @'writer' (a,w)@ embeds a simple writer G(computation,action).
    writer :: (a, w) -> m a

    -- | @'tell' w@ is an G(computation,action) that produces the output @w@.
    tell :: w -> m ()

    -- | @'listen' m@ is an G(computation,action) that executes the
    -- G(computation,action) @m@ and adds its output to the value of the
    -- G(compuation,computation).
    listen :: m a -> m (a, w)

    -- | @'pass' m@ is an G(computation,action) that executes the
    -- G(computation,action) @m@, which returns a value and a function, and
    -- returns the value, applying the function to the output.
    pass :: m (a, w -> w) -> m a

    writer ~(a, w) = tell w >> return a
    {-# INLINABLE writer #-}

    tell w = writer ((), w)
    {-# INLINABLE tell #-}

#ifdef MinimalPragma
    {-# MINIMAL listen, pass, (writer | tell) #-}

#endif

------------------------------------------------------------------------------
instance (Monad m, Monoid w) => MonadWriter w (L.WriterT w m) where
    writer = L.WriterT . return
    tell w = L.WriterT $ return ((), w)
    listen (L.WriterT m) = L.WriterT $ liftM (\(a, w) -> ((a, w), w)) m
    pass (L.WriterT m) = L.WriterT $ liftM (\((a, f), w) -> (a, f w)) m


------------------------------------------------------------------------------
instance (Monad m, Monoid w) => MonadWriter w (WriterT w m) where
    writer = WriterT . return
    tell w = WriterT $ return ((), w)
    listen (WriterT m) = WriterT $ liftM (\(a, w) -> ((a, w), w)) m
    pass (WriterT m) = WriterT $ liftM (\((a, f), w) -> (a, f w)) m


------------------------------------------------------------------------------
instance (Monad m, Monoid w) => MonadWriter w (L.RWST r w s m) where
    writer (a, w) = L.RWST $ \_ s -> return (a, s, w)
    tell w = L.RWST $ \_ s -> return ((), s, w)
    listen (L.RWST m) = L.RWST $ \r s ->
        liftM (\(~(a, s', w)) -> ((a, w), s', w)) (m r s)
    pass (L.RWST m) = L.RWST $ \r s ->
        liftM (\(~((a, f), s', w)) -> (a, s', f w)) (m r s)


------------------------------------------------------------------------------
instance (Monad m, Monoid w) => MonadWriter w (RWST r w s m) where
    writer (a, w) = RWST $ \_ s -> return (a, s, w)
    tell w = RWST $ \_ s -> return ((), s, w)
    listen (RWST m) = RWST $ \r s ->
        liftM (\(a, s', w) -> ((a, w), s', w)) (m r s)
    pass (RWST m) = RWST $ \r s ->
        liftM (\((a, f), s', w) -> (a, s', f w)) (m r s)


#if MIN_VERSION_transformers(0, 3, 0)
------------------------------------------------------------------------------
instance (MonadWriter w f, MonadWriter w g) => MonadWriter w (Product f g)
  where
    writer f = Pair (writer f) (writer f)
    tell w = Pair (tell w) (tell w)
    listen (Pair f g) = Pair (listen f) (listen g)
    pass (Pair f g) = Pair (pass f) (pass g)


#endif
#if MIN_VERSION_mmorph(1, 0, 1)
------------------------------------------------------------------------------
instance MonadWriter w (f (g m)) => MonadWriter w (ComposeT f g m) where
    writer f = ComposeT (writer f)
    tell w = ComposeT (tell w)
    listen (ComposeT m) = ComposeT (listen m)
    pass (ComposeT m) = ComposeT (pass m)


#endif
------------------------------------------------------------------------------
instance __OVERLAPPABLE__ (MonadTop t m, Monad (t m), MonadWriter w m) =>
    MonadWriter w (t m)
  where
    writer = liftT . writer
    {-# INLINABLE writer #-}
    tell = liftT . tell
    {-# INLINABLE tell #-}
    listen m = m >>= liftT . listen . return
    {-# INLINABLE listen #-}
    pass m = m >>= liftT . pass . return
    {-# INLINABLE pass #-}


------------------------------------------------------------------------------
-- | @'listens' f m@ is an G(computation,action) that executes the
-- G(computation,action) @m@ and adds the result of applying @f@ to the output
-- to the value of the G(computation,computation).
--
-- @'listens' f m = 'liftM' (\\(~(a, w)) -> (a, f w)) ('listen' m)@
listens :: MonadWriter w m => (w -> b) -> m a -> m (a, b)
listens f = liftM (\(~(a, w)) -> (a, f w)) . listen
{-# INLINABLE listens #-}


------------------------------------------------------------------------------
-- | @'censor' f m@ is an G(computation,action) that executes the
-- G(computation,action) @m@ and applies the function @f@ to its output,
-- leaving the return value unchanged.
--
-- @'censor' f m = 'pass' ('liftM' (\\a -> (a,f)) m)@
censor :: MonadWriter w m => (w -> w) -> m a -> m a
censor f = pass . liftM (\a -> (a, f))
{-# INLINABLE censor #-}
