{-# LANGUAGE CPP #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

#ifdef LANGUAGE_ConstraintKinds
{-# LANGUAGE ConstraintKinds #-}
#endif

#include <docmacros.h>

{-|

This module defines the 'MonadCatch' G(monadinterface,interface). The
'MonadCatch' G(monadinterface,interface) is a specialisation of the
'MonadRecover' G(monadinterface,interface). It, along with its sister
G(monadinterface,interface) 'MonadThrow', is designed to be largely compatible
with the "Control.Exception" module from H(base). It consists of:

  * The 'MonadCatch' constraint (a specialisation of the 'MonadRecover'
  constraint).
  * The 'catch' operation (a specialisation of the 'recover' operation).
  * The following alternate versions of 'catch' as defined in
  "Control.Exception":

      * 'catches'
      * 'catchJust'
      * 'handle'
      * 'handleJust'
      * 'try'
      * 'tryJust'

  * The helper 'Handler' data type (needed for 'catches').

-}

module Monad.Catch
    ( MonadCatch
    , catch
    , catches
    , Handler (Handler)
    , catchJust
    , handle
    , handleJust
    , try
    , tryJust
    )
where

-- base ----------------------------------------------------------------------
import           Control.Exception
                     ( Exception (fromException)
                     , SomeException
                     )
import           Control.Monad (liftM)
#if !MIN_VERSION_base(4, 6, 0)
import           Prelude hiding (catch)
#endif


-- layers --------------------------------------------------------------------
import           Monad.Recover (MonadRecover (recover))
import           Monad.Throw (throw)


------------------------------------------------------------------------------
-- | 'MonadCatch' is an alias of 'MonadRecover' where the failure state type
-- @e@ is fixed to 'SomeException'. It represents the class of monads which
-- support some sort of 'Control.Exception.catch'-like operation to recover
-- from failures caused by a call to 'Monad.Throw.throw'.
#ifdef LANGUAGE_ConstraintKinds
type MonadCatch = MonadRecover SomeException
#else
class MonadRecover SomeException m => MonadCatch m
instance MonadRecover SomeException m => MonadCatch m
#endif


------------------------------------------------------------------------------
-- | This is the simplest of the exception-catching functions. It takes a
-- single argument, runs it, and if an exception is raised the \"handler\"
-- is executed, with the value of the exception passed as an argument.
-- Otherwise, the result is returned as normal. For example:
--
-- @'catch' ('readFile' f)
--      (\\e -> do let err = 'show' (e :: 'Control.Exception.IOException')
--                'System.IO.hPutStr' 'System.IO.stderr' ("Warning: Couldn't open " '++' f '++' ": " '++' err)
--                'return' "")@
--
-- Note that we have to give a type signature to e, or the program will
-- not typecheck as the type is ambiguous. While it is possible to catch
-- exceptions of any type, see the section \"Catching all exceptions\" in
-- "Control.Exception" for an explanation of the problems with doing so.
--
-- Note that due to Haskell's unspecified evaluation order, an expression
-- may throw one of several possible exceptions: consider the expression
-- @('error' \"urk\") '+' (1 \``div`\` 0)@. Does the expression throw
-- @'Control.Exception.ErrorCall' \"urk\"@, or
-- 'Control.Exception.DivideByZero'?
--
-- The answer is \"it might throw either\"; the choice is
-- non-deterministic. If you are catching any type of exception then you
-- might catch either. If you are calling catch with type
-- @m 'Int' -> ('Control.Exception.ArithException' -> m 'Int') -> m 'Int'@
-- then the handler may get run with 'Control.Exception.DivideByZero' as an
-- argument, or an @'Control.Exception.ErrorCall' \"urk\"@ exception may be
-- propogated further up. If you call it again, you might get a the opposite
-- behaviour. This is ok, because 'catch' is a monadic computation.
catch :: (Exception e, MonadCatch m) => m a -> (e -> m a) -> m a
catch m h = recover m (\e -> maybe (throw e) h (fromException e))
{-# INLINABLE catch #-}


------------------------------------------------------------------------------
-- | Sometimes you want to catch two different sorts of exception. You could
-- do something like
--
-- @f = expr \``catch`\` \\(ex :: 'Control.Exception.ArithException') -> handleArith ex
--         \``catch`\` \\(ex :: 'Control.Exception.IOException')    -> handleIO    ex@
--
-- However, there are a couple of problems with this approach. The first is
-- that having two exception handlers is inefficient. However, the more
-- serious issue is that the second exception handler will catch exceptions
-- in the first, e.g. in the example above, if @handleArith@ throws an
-- 'Control.Exception.IOException' then the second exception handler will
-- catch it.
--
-- Instead, we provide a function 'catches', which would be used thus:
--
-- @f = expr \``catches`\` ['Handler' (\\(ex :: 'Control.Exception.ArithException') -> handleArith ex),
--                    'Handler' (\\(ex :: 'Control.Exception.IOException')    -> handleIO    ex)]@
catches :: MonadCatch m => m a -> [Handler m a] -> m a
catches m handlers = m `catch` go handlers
  where
    go [] e = throw e
    go (Handler handler:xs) e = maybe (go xs e) handler (fromException e)
{-# INLINABLE catches #-}


------------------------------------------------------------------------------
-- | You need this when using 'catches'.
data Handler m a = forall e. Exception e => Handler (e -> m a)


------------------------------------------------------------------------------
-- | The function 'catchJust' is like 'catch', but it takes an extra argument
-- which is an /exception predicate/, a function which selects which type of
-- exceptions we're interested in.
--
-- @'catchJust' (\\e -> if 'System.IO.Error.isDoesNotExistErrorType' ('System.IO.Error.ioeGetErrorType' e) then 'Just' () else 'Nothing')
--          ('readFile' f)
--          (\\_ -> do 'System.IO.hPutStrLn' 'System.IO.stderr' ("No such file: " '++' 'show' f)
--                    'return' "")@
--
-- Any other exceptions which are not matched by the predicate are re-raised,
-- and may be caught by an enclosing 'catch', 'catchJust', etc.
catchJust
    :: (Exception e, MonadCatch m)
    => (e -> Maybe b)
    -> m a
    -> (b -> m a)
    -> m a
catchJust p a handler = catch a (\e -> maybe (throw e) handler (p e))
{-# INLINABLE catchJust #-}


------------------------------------------------------------------------------
-- | A version of 'catch' with the arguments swapped around; useful in
-- situations where the code for the handler is shorter.  For example:
--
-- @do 'handle' (\\'Control.Exception.NonTermination' -> 'System.Exit.exitWith' ('System.Exit.ExitFailure' 1)) '$'
--   ...@
handle :: (Exception e, MonadCatch m) => (e -> m a) -> m a -> m a
handle = flip catch
{-# INLINABLE handle #-}


------------------------------------------------------------------------------
-- | A version of 'catchJust' with the arguments swapped around (see
-- 'handle').
handleJust
    :: (Exception e, MonadCatch m)
    => (e -> Maybe b)
    -> (b -> m a)
    -> m a
    -> m a
handleJust = flip . catchJust
{-# INLINABLE handleJust #-}


------------------------------------------------------------------------------
-- | Similar to 'catch', but returns an 'Either' result which is @('Right' a)@
-- if no exception of type @e@ was raised, or @('Left' ex)@ if an exception of
-- type @e@ was raised and its value is @ex@. If any other type of exception
-- is raised than it will be propogated up to the next enclosing exception
-- handler.
--
-- @'try' a = 'catch' ('Right' \``liftM`\` a) ('return' '.' 'Left')@
try :: (Exception e, MonadCatch m) => m a -> m (Either e a)
try = handle (return . Left) . liftM Right
{-# INLINABLE try #-}


------------------------------------------------------------------------------
-- | A variant of 'try' that takes an exception predicate to select which
-- exceptions are caught (c.f. 'catchJust').  If the exception does not match
-- the predicate, it is re-thrown.
tryJust
    :: (Exception e, MonadCatch m)
    => (e -> Maybe b)
    -> m a
    -> m (Either b a)
tryJust p = handleJust p (return . Left) . liftM Right
{-# INLINABLE tryJust #-}
