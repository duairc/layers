{-# LANGUAGE CPP #-}
#if LANGUAGE_ConstraintKinds >= 704
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

    2. The "Control.Monad.Interface.Reader" module.

    3. The "Control.Monad.Interface.State" module.

    4. The "Control.Monad.Interface.Writer" module.

-}

module Control.Monad.Interface.RWS
    ( MonadRWS
    , module Control.Monad.Interface.Reader
    , module Control.Monad.Interface.State
    , module Control.Monad.Interface.Writer
    )
where

#ifndef LANGUAGE_ConstraintKinds
-- base ----------------------------------------------------------------------
import           Data.Monoid (Monoid)
#endif


-- layers --------------------------------------------------------------------
import           Control.Monad.Interface.Reader
                     ( MonadReader (reader, ask, local)
                     , asks
                     )
import           Control.Monad.Interface.State
                     ( MonadState (state, get, put)
                     , modify
                     , gets
                     )
import           Control.Monad.Interface.Writer
                     ( MonadWriter (writer, tell, listen, pass)
                     , listens
                     , censor
                     )


------------------------------------------------------------------------------
-- | The 'MonadRWS' interface is defined as a type synonym (using
-- the @ConstraintKinds@ extension) for the combination of 'MonadReader',
-- 'MonadState' and 'MonadWriter'.
#if LANGUAGE_ConstraintKinds
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
