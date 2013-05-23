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

{-# OPTIONS_GHC -fno-warn-orphans #-}

{-|

This module exports:

    1. The 'MonadCatch' type class and its operations 'throw' and 'catch'.

    2. Instances of 'MonadCatch' for 'IO', 'Either', 'STM' and the
    'ErrorT' monad transformer from the @transformers@ package.

    3. An orphan instance of 'Error' for the 'SomeException' type: this is a
    necessary hack in order to make 'ErrorT' an instance of 'MonadCatch'.

    3. A universal pass-through instance of 'MonadCatch' for any existing
    @MonadCatch@ wrapped by a 'MonadLayerControl'.

    4. The utility operations 'catches', 'catchJust', 'handle', 'handleJust',
    'try' and 'tryJust'.

-}

module Control.Monad.Interface.Exception
    ( -- * The @MonadCatch@ class
      -- $split
      MonadThrow
    , MonadCatch
    , MonadException
    , throw
    , catch
    , catches
    , Handler (Handler)
    , catchJust
    , handle
    , handleJust
    , try
    , tryJust
    , Exception (toException, fromException)
    , SomeException (SomeException)
    )
where

-- base ----------------------------------------------------------------------
import           Control.Exception
                     ( Exception (toException, fromException)
                     , SomeException (SomeException)
                     , PatternMatchFail (PatternMatchFail)
                     )
import           Control.Monad (liftM)
#if !MIN_VERSION_base(4, 6, 0)
import           Prelude hiding (catch)
#endif


-- transformers --------------------------------------------------------------
import           Control.Monad.Trans.Error (Error (noMsg, strMsg))


-- layers --------------------------------------------------------------------
import           Control.Monad.Interface.Abort (MonadAbort (abort))
import           Control.Monad.Interface.Recover (MonadRecover (recover))


------------------------------------------------------------------------------
#if LANGUAGE_ConstraintKinds
type MonadThrow = MonadAbort SomeException
#else
class MonadAbort SomeException m => MonadThrow m
instance MonadAbort SomeException m => MonadThrow m
#endif


------------------------------------------------------------------------------
throw :: (Exception e, MonadThrow m) => e -> m a
throw = abort . toException
{-# INLINE throw #-}


------------------------------------------------------------------------------
#if LANGUAGE_ConstraintKinds
type MonadCatch = MonadRecover SomeException
#else
class MonadRecover SomeException m => MonadCatch m
instance MonadRecover SomeException m => MonadCatch m
#endif


------------------------------------------------------------------------------
catch :: (Exception e, MonadCatch m) => m a -> (e -> m a) -> m a
catch m h = recover m (\e -> maybe (abort e) h (fromException e))
{-# INLINE catch #-}


------------------------------------------------------------------------------
-- | A synonym for 'MonadCatch'.
#if LANGUAGE_ConstraintKinds
type MonadException = MonadCatch
#else
class MonadCatch m => MonadException m
instance MonadCatch m => MonadException m
#endif


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


------------------------------------------------------------------------------
-- | Cheeky orphan instance of 'Error' for 'SomeException'. This allows
-- @SomeException@ to be used with the 'ErrorT' monad transformer, and thus a
-- 'MonadCatch' instance to be defined for @ErrorT SomeException@.
instance Error SomeException where
    noMsg = strMsg "mzero"
    {-# INLINE noMsg #-}
    strMsg = SomeException . PatternMatchFail
    {-# INLINE strMsg #-}
