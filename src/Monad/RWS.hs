{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

#ifdef LANGUAGE_SafeHaskell
{-# LANGUAGE Safe #-}
#endif

#include "docmacros.h"
#include "newtypec.h"
#include "overlap.h"

{-|

This module defines the 'MonadRWS' G(monadinterface,interface), which consists
of:

    * The 'MonadRWS' constraint.
    * The "Monad.Reader" module.
    * The "Monad.State" module.
    * The "Monad.Writer" module.

The 'MonadRWS' G(monadinterface,interface) is provided for compatibility with
the T(mtl,Control-Monad-RWS-Class,MonadRWS) G(monadinterface,interface) from
the H(mtl) library.

-}

module Monad.RWS
    ( MonadRWS
    , module Monad.Reader
    , module Monad.State
    , module Monad.Writer
    )
where

-- layers --------------------------------------------------------------------
import           Monad.Reader
                     ( MonadReader (reader, ask, local), asks
                     )
import           Monad.State
                     ( MonadState (state, get, put), modify, gets
                     )
import           Monad.Writer
                     ( MonadWriter (writer, tell, listen, pass)
                     , listens, censor
                     )


------------------------------------------------------------------------------
-- | 'MonadRWS' is simply a
-- UG(glasgow_exts.html#the-constraint-kind,constraint synonym) for the
-- combination of 'MonadReader', 'MonadState' and 'MonadWriter'.
newtypeC(MonadRWS r w s m, (MonadReader r m, MonadWriter w m, MonadState s m))
