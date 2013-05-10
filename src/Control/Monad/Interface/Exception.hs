{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverlappingInstances #-}
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
      MonadThrow (throw)
    , MonadCatch (catch)
    , MonadException
    , catches
    , Handler (Handler)
    , catchJust
    , handle
    , handleJust
    , try
    , tryJust
    , SomeException
    )
where

-- base ----------------------------------------------------------------------
import           Control.Exception
                     ( Exception (toException, fromException)
                     , SomeException (SomeException)
                     , PatternMatchFail (PatternMatchFail)
                     , throwIO
                     )
import qualified Control.Exception as E (catch)
import           Control.Monad (liftM, mzero)
import           GHC.Conc.Sync (STM, catchSTM, throwSTM)


-- transformers --------------------------------------------------------------
import           Control.Monad.Trans.Error
                     ( Error (noMsg, strMsg)
                     , ErrorT (ErrorT)
                     )
import           Control.Monad.Trans.List (ListT)
import           Control.Monad.Trans.Maybe (MaybeT)
import           Data.Functor.Product (Product (Pair))


-- layers --------------------------------------------------------------------
import           Control.Monad.Layer
                     ( MonadLayer (type Inner, layer)
                     , MonadLayerControl
                     , controlLayer
                     )


------------------------------------------------------------------------------
-- $split
--
-- The 'MonadCatch' monad interface is split up into two parts:
-- 'MonadThrow' and 'MonadCatch', the latter of which is a subclass of the
-- former. The reason they are split up is that there are more monads that can
-- provide 'MonadThrow' than 'MonadCatch'. This is partially because
-- @MonadThrow@'s universal pass-through instance can pass through any
-- 'MonadLayer', while @MonadCatch@ requires @MonadLayerControl@. But even
-- ignoring that, there are more monads that can provide @MonadThrow@ than
-- @MonadCatch@ in the first place. The monads which implement @MonadCatch@
-- are basically 'IO' and the 'Either'-like monads: these are the monads where
-- failure is possible and the failure state contains some sort of 'Exception'
-- value which can be observed. However, @MonadThrow@ can reasonably be
-- implemented by any monad which can fail. In the case of monads which
-- implement @MonadCatch@, @MonadThrow@'s 'throw' operation will \"store\" the
-- 'Exception' value its given in the monad's failure \"state\". For other
-- monads which can fail, @throw@ simply fails, and can be recovered from by
-- other means: for example, using the 'Control.Monad.MonadPlus' interface.


------------------------------------------------------------------------------
-- | The 'MonadThrow' type class represents the class of monads which can
-- fail, and, if possible, can store an 'Exception' value in their failure
-- \"state\" somehow.
--
-- Minimal complete definition: instance head only.
class Monad m => MonadThrow m where
    -- | Fail with a given exception.
    throw :: Exception e => e -> m a
    throw = fail . show
    {-# INLINE throw #-}


------------------------------------------------------------------------------
instance MonadThrow ([]) where
    throw = const mzero
    {-# INLINE throw #-}


------------------------------------------------------------------------------
instance MonadThrow Maybe where
    throw = const mzero
    {-# INLINE throw #-}


------------------------------------------------------------------------------
instance e ~ SomeException => MonadThrow (Either e) where
    throw = Left . toException
    {-# INLINE throw #-}


------------------------------------------------------------------------------
instance MonadThrow IO where
    throw = throwIO
    {-# INLINE throw #-}


------------------------------------------------------------------------------
instance MonadThrow STM where
    throw = throwSTM
    {-# INLINE throw #-}


------------------------------------------------------------------------------
instance Monad m => MonadThrow (ListT m) where
    throw = const mzero
    {-# INLINE throw #-}


------------------------------------------------------------------------------
instance Monad m => MonadThrow (MaybeT m) where
    throw = const mzero
    {-# INLINE throw #-}


------------------------------------------------------------------------------
instance (e ~ SomeException, Monad m) => MonadThrow (ErrorT e m) where
    throw = ErrorT . return . Left . toException
    {-# INLINE throw #-}


------------------------------------------------------------------------------
instance (MonadThrow f, MonadThrow g) => MonadThrow (Product f g) where
    throw e = Pair (throw e) (throw e)


------------------------------------------------------------------------------
instance (MonadLayer m, MonadThrow (Inner m)) => MonadThrow m where
    throw = layer . throw
    {-# INLINE throw #-}


------------------------------------------------------------------------------
-- | The 'MonadCatch' type class represents the subclass of monads which can
-- fail with an exception ('MonadThrow') which can recover from that failure
-- by 'catch'ing specific 'Exception' values. This includes 'IO'-based monads
-- as well as 'Either'-like monads.
--
-- Minimal complete definition: 'throw', 'catch'.
class MonadThrow m => MonadCatch m where
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
    -- For catching exceptions in pure (non-IO) expressions, see the function
    -- 'Control.Exception.evaluate'.
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
    catch :: Exception e => m a -> (e -> m a) -> m a


------------------------------------------------------------------------------
instance e ~ SomeException => MonadCatch (Either e) where
    catch m h = either (\e -> maybe (Left e) h (fromException e)) Right m
    {-# INLINE catch #-}


------------------------------------------------------------------------------
instance MonadCatch IO where
    catch = E.catch
    {-# INLINE catch #-}


------------------------------------------------------------------------------
instance MonadCatch STM where
    catch = catchSTM
    {-# INLINE catch #-}


------------------------------------------------------------------------------
instance (e ~ SomeException, Monad m) => MonadCatch (ErrorT e m) where
    catch (ErrorT m) h = ErrorT $ m >>= either
        (\e -> maybe
            (return $ Left e)
            (\e' -> let ErrorT m' = h e' in m')
            (fromException e))
        (return . Right)
    {-# INLINE catch #-}


------------------------------------------------------------------------------
instance (MonadCatch f, MonadCatch g) => MonadCatch (Product f g) where
    catch (Pair f g) h = Pair
        (catch f (\e -> let Pair f' _ = h e in f'))
        (catch g (\e -> let Pair _ g' = h e in g'))


------------------------------------------------------------------------------
instance (MonadLayerControl m, MonadCatch (Inner m)) => MonadCatch m where
    catch m h = controlLayer (\run -> catch (run m) (run . h))
    {-# INLINE catch #-}


------------------------------------------------------------------------------
-- | A synonym for 'MonadCatch'.
type MonadException = MonadCatch


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