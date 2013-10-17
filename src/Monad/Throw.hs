{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
#ifdef LANGUAGE_ConstraintKinds
{-# LANGUAGE ConstraintKinds #-}
#endif

{-|

This module defines the 'MonadThrow' interface, which consists of:

    * 'MonadThrow' :: @(* -> *) -> Constraint@

    * 'throw' :: @(Exception e, MonadThrow m) => e -> m a@

The 'MonadThrow' interface is defined purely in terms of the 'MonadAbort'
interface. It is provided for compatibility with "Control.Exception" (see
"Monad.Catch").

-}

module Monad.Throw
    ( MonadThrow
    , throw
    )
where

-- base ----------------------------------------------------------------------
import           Control.Exception
                     ( Exception
                     , PatternMatchFail (PatternMatchFail)
                     , SomeException
                     , toException
                     )


-- layers --------------------------------------------------------------------
import           Monad.Abort (MonadAbort, abort)
import           Control.Monad.Trans.Error (Error, noMsg, strMsg)


------------------------------------------------------------------------------
-- | 'MonadThrow' is an alias of 'MonadAbort' where the failure state type @e@
-- is fixed to 'SomeException'. It represents the class of monads which
-- support some sort of 'Control.Exception.throwIO'-like operation.
#ifdef LANGUAGE_ConstraintKinds
type MonadThrow = MonadAbort SomeException
#else
class MonadAbort SomeException m => MonadThrow m
instance MonadAbort SomeException m => MonadThrow m
#endif


------------------------------------------------------------------------------
-- | A version of 'Control.Exception.throwIO' for arbitrary instances of
-- 'MonadThrow'.
throw :: (Exception e, MonadThrow m) => e -> m a
throw = abort . toException


------------------------------------------------------------------------------
-- | Cheeky orphan instance of 'Error' for 'SomeException'. This allows
-- @SomeException@ to be used with the 'ErrorT' monad transformer, and thus
-- 'MonadThrow' and 'MonadCatch' instances to be defined for
-- @ErrorT SomeException@.
instance Error SomeException where
    noMsg = strMsg "mzero"
    strMsg = toException . PatternMatchFail
