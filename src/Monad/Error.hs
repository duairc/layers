{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
#ifdef LANGUAGE_ConstraintKinds
{-# LANGUAGE ConstraintKinds #-}
#endif

{-|

This module defines the 'MonadError' interface, which consists of:

    * 'MonadError' :: @* -> (* -> *) -> Constraint@

    * 'throwError' :: @MonadError e m => e -> m a@

    * 'catchError' :: @MonadError e m => m a -> (e -> m a) -> m a@

    * 'Error' :: @* -> Constraint@

    * 'noMsg' :: @Error a => a@

    * 'strMsg' :: @Error a => String -> a@

The 'MonadError' interface is defined purely in terms of the 'MonadAbort' and
'MonadRecover' interfaces. It is provided for compatibility with the
@MonadError@ class from the @mtl@ library. 'Error', 'noMsg' and 'strMsg' are
re-exported from @transformers@.

-}

module Monad.Error
    ( Error (noMsg, strMsg)
    , MonadError
    , catchError
    , throwError
    )
where

-- transformers --------------------------------------------------------------
#if __GLASGOW_HASKELL__ >= 706
import           Control.Monad.Trans.Error (Error (noMsg, strMsg))
#else
import           Control.Monad.Trans.Error (Error (..))
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
-- constructor. It is common to use @'Data.Either' String@ as the monad type
-- constructor for an error monad in which error descriptions take the form of
-- strings. In that case and many other common cases the resulting monad is
-- already defined as an instance of the 'MonadError' class.
--
-- You can also define your own error type and\/or use a monad type
-- constructor other than @'Either' 'String'@ or @'Either' 'IOError'@. In
-- these cases you will have to explicitly define instances of the 'Error'
-- and\/or 'MonadError' classes.
--
-- Note: This is for compatibility with the @MonadError@ type class from the
-- @mtl@ package. It doesn't provide anything that isn't provided by the
-- 'Monad.Abort.MonadAbort' and 'Monad.Recover.MonadRecover' interfaces.
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
{-# INLINE throwError #-}


------------------------------------------------------------------------------
-- | A handler function to handle previous errors and return to normal
-- execution.
--
-- A common idiom is:
--
-- > do { action1; action2; action3 } `catchError` handler
--
-- where the @action@ functions can call 'throwError'.
--
-- Note that @handler@ and the do-block must have the same return type.
catchError :: MonadError e m => m a -> (e -> m a) -> m a
catchError = recover
{-# INLINE catchError #-}
