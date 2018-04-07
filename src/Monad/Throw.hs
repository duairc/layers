{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-warnings-deprecations #-}

#include "docmacros.h"
#include "newtypec.h"

{-|

This module defines the 'MonadThrow' G(monadinterface,interface). The
'MonadThrow' G(monadinterface,interface) is a specialisation of the
'MonadAbort' G(monadinterface,interface). It, along with its sister
G(monadinterface,interface) 'Monad.Catch.MonadCatch', is designed to be
largely compatible with the "Control.Exception" module from H(base). It
consists of:

  * The 'MonadThrow' constraint (a specialisation of the 'MonadAbort'
  constraint).
  * The 'throw' operation (a specialisation of the 'abort' operation).

-}

module Monad.Throw
    ( MonadThrow, throw
    )
where

-- base ----------------------------------------------------------------------
import           Control.Exception
                     ( Exception, SomeException, toException
#if !MIN_VERSION_transformers(0, 6, 0)
                     , PatternMatchFail (PatternMatchFail)
#endif
                     )


-- layers --------------------------------------------------------------------
import           Monad.Abort (MonadAbort, abort)
#if !MIN_VERSION_transformers(0, 6, 0)
import           Control.Monad.Trans.Error (Error, noMsg, strMsg)
#endif


------------------------------------------------------------------------------
-- | 'MonadThrow' is an alias of 'MonadAbort' where the failure state type @e@
-- is fixed to 'SomeException'. It represents the class of monads which
-- support some sort of 'Control.Exception.throwIO'-like operation.
newtypeC(MonadThrow m, MonadAbort SomeException m)


------------------------------------------------------------------------------
-- | A version of 'Control.Exception.throwIO' for arbitrary instances of
-- 'MonadThrow'.
throw :: (Exception e, MonadThrow m) => e -> m a
throw = abort . toException
#if !MIN_VERSION_transformers(0, 6, 0)


------------------------------------------------------------------------------
-- | Cheeky orphan instance of 'Error' for 'SomeException'. This allows
-- 'SomeException' to be used with the 'Control.Monad.Trans.Error.ErrorT'
-- G(monadtransformer,monad transformer), and thus 'Monad.Throw.MonadThrow'
-- and 'Monad.Catch.MonadCatch' instances to be defined for
-- @'Control.Monad.Trans.Error.ErrorT' 'SomeException'@.
instance Error SomeException where
    noMsg = strMsg "mzero"
    strMsg = toException . PatternMatchFail
#endif
