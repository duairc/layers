{-# LANGUAGE CPP #-}
#ifdef LANGUAGE_ConstraintKinds
{-# LANGUAGE ConstraintKinds #-}
#endif
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

{-|

This module exports:

    1. The 'MonadCatch' constraint synonym (a version of 'MonadRecover').

    2. The 'catch' operation (a version of 'recover').

    3. The utility operations 'catches', 'catchJust', 'handle', 'handleJust',
    'try' and 'tryJust'.

-}

module Control.Monad.Interface.Catch
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
import           Control.Monad.Interface.Recover (MonadRecover (recover))
import           Control.Monad.Interface.Throw (throw)


------------------------------------------------------------------------------
-- | 'MonadCatch' is an alias of 'MonadRecover' where the failure state type
-- @e@ is fixed to 'SomeException'. It represents the class of monads which
-- support some sort of 'Control.Exception.catch'-like operation to recover
-- from failures caused by a call to 'Control.Monad.Interface.Throw.throw'.
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
-- > catch (readFile f) (\(e :: IOException) -> do
-- >     hPutStr stderr ("Warning: Couldn't open " ++ f ++ ": " ++ show e)
-- >     return "")
--
-- Note that we have to give a type signature to e, or the program will
-- not typecheck as the type is ambiguous. While it is possible to catch
-- exceptions of any type, see the section \"Catching all exceptions\" in
-- "Control.Exception" for an explanation of the problems with doing so.
--
-- Note that due to Haskell's unspecified evaluation order, an expression
-- may throw one of several possible exceptions: consider the expression
-- @(error \"urk\") + (1 `div` 0)@. Does the expression throw
-- @ErrorCall \"urk\"@, or @DivideByZero@?
--
-- The answer is \"it might throw either\"; the choice is
-- non-deterministic. If you are catching any type of exception then you
-- might catch either. If you are calling catch with type
-- @m Int -> (ArithException -> m Int) -> m Int@ then the handler may get
-- run with @DivideByZero@ as an argument, or an @ErrorCall \"urk\"@
-- exception may be propogated further up. If you call it again, you might
-- get a the opposite behaviour. This is ok, because 'catch' is a monadic
-- computation.
catch :: (Exception e, MonadCatch m) => m a -> (e -> m a) -> m a
catch m h = recover m (\e -> maybe (throw e) h (fromException e))
{-# INLINE catch #-}


------------------------------------------------------------------------------
-- | Sometimes you want to catch two different sorts of exception. You could
-- do something like
--
-- > f = expr `catch` \(ex :: ArithException) -> handleArith ex
-- >          `catch` \(ex :: IOException)    -> handleIO    ex
--
-- However, there are a couple of problems with this approach. The first is
-- that having two exception handlers is inefficient. However, the more
-- serious issue is that the second exception handler will catch exceptions
-- in the first, e.g. in the example above, if @handleArith@ throws an
-- @IOException@ then the second exception handler will catch it.
--
-- Instead, we provide a function 'catches', which would be used thus:
--
-- > f = expr `catches` [Handler (\ (ex :: ArithException) -> handleArith ex),
-- >                     Handler (\ (ex :: IOException)    -> handleIO    ex)]
catches :: MonadCatch m => m a -> [Handler m a] -> m a
catches m handlers = m `catch` go handlers
  where
    go [] e = throw e
    go (Handler handler:xs) e = maybe (go xs e) handler (fromException e)
{-# INLINE catches #-}


------------------------------------------------------------------------------
-- | You need this when using 'catches'.
data Handler m a = forall e. Exception e => Handler (e -> m a)


------------------------------------------------------------------------------
-- | The function 'catchJust' is like 'catch', but it takes an extra argument
-- which is an /exception predicate/, a function which selects which type of
-- exceptions we're interested in.
--
-- > catchJust (\e -> if isDoesNotExistErrorType (ioeGetErrorType e)
-- >               then Just ()
-- >               else Nothing)
-- >           (readFile f)
-- >           (\_ -> do
-- >               hPutStrLn stderr ("No such file: " ++ show f)
-- >               return "")
--
-- Any other exceptions which are not matched by the predicate are re-raised,
-- and may be caught by an enclosing 'catch', 'catchJust', etc.
catchJust
    :: (MonadCatch m, Exception e)
    => (e -> Maybe b)
    -> m a
    -> (b -> m a)
    -> m a
catchJust p a handler = catch a (\e -> maybe (throw e) handler (p e))
{-# INLINE catchJust #-}


------------------------------------------------------------------------------
-- | A version of 'catch' with the arguments swapped around; useful in
-- situations where the code for the handler is shorter.  For example:
--
-- >   do handle (\NonTermination -> exitWith (ExitFailure 1)) $
-- >      ...
handle :: (MonadCatch m, Exception e) => (e -> m a) -> m a -> m a
handle = flip catch
{-# INLINE handle #-}


------------------------------------------------------------------------------
-- | A version of 'catchJust' with the arguments swapped around (see
-- 'handle').
handleJust
    :: (MonadCatch m, Exception e)
    => (e -> Maybe b)
    -> (b -> m a)
    -> m a
    -> m a
handleJust = flip . catchJust
{-# INLINE handleJust #-}


------------------------------------------------------------------------------
-- | Similar to 'catch', but returns an 'Either' result which is @('Right' a)@
-- if no exception of type @e@ was raised, or @('Left' ex)@ if an exception of
-- type @e@ was raised and its value is @ex@. If any other type of exception
-- is raised than it will be propogated up to the next enclosing exception
-- handler.
--
-- > try a = catch (Right `liftM` a) (return . Left)
try :: (MonadCatch m, Exception e) => m a -> m (Either e a)
try = handle (return . Left) . liftM Right
{-# INLINE try #-}


------------------------------------------------------------------------------
-- | A variant of 'try' that takes an exception predicate to select which
-- exceptions are caught (c.f. 'catchJust').  If the exception does not match
-- the predicate, it is re-thrown.
tryJust
    :: (MonadCatch m, Exception e)
    => (e -> Maybe b)
    -> m a
    -> m (Either b a)
tryJust p = handleJust p (return . Left) . liftM Right
{-# INLINE tryJust #-}
