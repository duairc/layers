{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
#ifdef LANGUAGE_ConstraintKinds
{-# LANGUAGE ConstraintKinds #-}
#endif

{-# OPTIONS_GHC -fno-warn-warnings-deprecations #-}

#include <macros.h>

{-|

This module defines the 'MonadError' G(monadinterface,interface). It is
entirely superfluous, consisting only of synonyms and re-exports, but is
provided for compatibility with the
T(mtl,Control-Monad-Error-Class,MonadError) G(monadinterface,interface) from
the H(mtl) package. It consists of:

  * The 'MonadError' constraint (a synonym for 'MonadRecover').
  * The 'throwError' operation (a synonym for 'abort').
  * The 'catchError' operation (a synonym for 'recover').
#if !MIN_VERSION_transformers(0, 5, 0)
  * The 'Error' class and its 'noMsg' and 'strMsg' operations (re-exported
  from H(transformers)).
#endif

-}

module Monad.Error
    (
#if !MIN_VERSION_transformers(0, 6, 0)
      Error (noMsg, strMsg)
    ,
#endif
      MonadError
    , catchError
    , throwError
    )
where


#if !MIN_VERSION_transformers(0, 6, 0)
-- transformers --------------------------------------------------------------
#if __GLASGOW_HASKELL__ >= 706
import           Control.Monad.Trans.Error (Error (noMsg, strMsg))
#else
import           Control.Monad.Trans.Error (Error (..))
#endif
#endif


-- layers --------------------------------------------------------------------
import           Monad.Abort (MonadAbort (abort))
import           Monad.Recover (MonadRecover (recover))


------------------------------------------------------------------------------
-- | The strategy of combining computations that can throw exceptions by
-- bypassing bound functions from the point an exception is thrown to the
-- point that it is handled.
--
-- Is parameterized over the type of error information and the monad type
-- constructor. It is common to use @'Either' 'String'@ as the monad type
-- constructor for an error monad in which error descriptions take the form of
-- strings. In that case and many other common cases the resulting monad is
-- already defined as an instance of the 'MonadError' class.
--
-- You can also define your own error type and\/or use a monad type
-- constructor other than @'Either' 'String'@ or
-- @'Either' 'Control.Exception.IOError'@. In these cases you will have to
-- explicitly define instances of the 'Error' and\/or 'MonadError' classes.
#ifdef LANGUAGE_ConstraintKinds
type MonadError = MonadRecover
#else
class MonadRecover e m => MonadError e m
instance MonadRecover e m => MonadError e m
#endif


------------------------------------------------------------------------------
-- | Is used within a monadic computation to begin exception processing.
throwError :: MonadError e m => e -> m a
throwError = abort


------------------------------------------------------------------------------
-- | A handler function to handle previous errors and return to normal
-- execution.
--
-- A common idiom is:
--
-- @do { action1; action2; action3 } \``catchError`\` handler@
--
-- where the @action@ functions can call 'throwError'.
--
-- Note that @handler@ and the do-block must have the same return type.
catchError :: MonadError e m => m a -> (e -> m a) -> m a
catchError = recover
