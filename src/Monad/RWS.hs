{-# LANGUAGE CPP #-}
#ifdef LANGUAGE_ConstraintKinds
{-# LANGUAGE ConstraintKinds #-}
#else
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
#endif

{-|

This module exports:

    1. The 'MonadRWS' interface.

    2. The "Monad.Reader" module.

    3. The "Monad.State" module.

    4. The "Monad.Writer" module.

-}

module Monad.RWS
    ( MonadRWS
    , module Monad.Reader
    , module Monad.State
    , module Monad.Writer
    )
where

#ifndef LANGUAGE_ConstraintKinds
-- base ----------------------------------------------------------------------
import           Data.Monoid (Monoid)
#endif


-- layers --------------------------------------------------------------------
import           Monad.Reader
                     ( MonadReader (reader, ask, local)
                     , asks
                     )
import           Monad.State
                     ( MonadState (state, get, put)
                     , modify
                     , gets
                     )
import           Monad.Writer
                     ( MonadWriter (writer, tell, listen, pass)
                     , listens
                     , censor
                     )


------------------------------------------------------------------------------
-- | The 'MonadRWS' interface is defined as a type synonym (using
-- the @ConstraintKinds@ extension) for the combination of 'MonadReader',
-- 'MonadState' and 'MonadWriter'.
#ifdef LANGUAGE_ConstraintKinds
type MonadRWS r w s m = (MonadReader r m, MonadWriter w m, MonadState s m)
#else
class
    ( Monoid w
    , MonadReader r m
    , MonadWriter w m
    , MonadState s m
    )
  =>
    MonadRWS r w s m | m -> r, m -> w, m -> s
instance
    ( Monoid w
    , MonadReader r m
    , MonadWriter w m
    , MonadState s m
    )
  =>
    MonadRWS r w s m
#endif
