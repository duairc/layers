{-# LANGUAGE CPP #-}
#ifdef LANGUAGE_ConstraintKinds
{-# LANGUAGE ConstraintKinds #-}
#else
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
#endif
{-# LANGUAGE FlexibleContexts #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

{-|

This module exports:

    1. The 'MonadThrow' constraint synonym (a version of 'MonadAbort').

    2. The 'throw' operation (a version of 'abort').

-}

module Control.Monad.Interface.Throw
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
import           Control.Monad.Interface.Abort (MonadAbort, abort)
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
{-# INLINE throw #-}


------------------------------------------------------------------------------
-- | Cheeky orphan instance of 'Error' for 'SomeException'. This allows
-- @SomeException@ to be used with the 'ErrorT' monad transformer, and thus a
-- 'MonadCatch' instance to be defined for @ErrorT SomeException@.
instance Error SomeException where
    noMsg = strMsg "mzero"
    {-# INLINE noMsg #-}
    strMsg = toException . PatternMatchFail
    {-# INLINE strMsg #-}
