{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

{-|

This module exports:

    1. The 'MonadTry' type class and its operation 'mtry'.

    2. Instances of 'MonadTry' for all the base monads in the @base@ and
    @transformers@ packages.

    3. A universal pass-through instance of 'MonadMask' for any existing
    @MonadMask@ wrapped by a 'MonadLayerControl'.

    4. The utility operations 'bracket', 'bracket_', 'bracketOnError',
    'finally' and 'onException'.

-}

module Control.Monad.Interface.Try
    ( MonadTry (mtry)
    , bracket
    , bracket_
    , bracketOnError
    , finally
    , onException
    )
where

-- base ----------------------------------------------------------------------
import           Control.Exception (SomeException, throwIO, try)
import           Control.Monad (liftM)
import           Control.Monad.ST (ST)
import qualified Control.Monad.ST.Lazy as L (ST)
import           GHC.Conc.Sync (STM, catchSTM, throwSTM)


-- transformers --------------------------------------------------------------
import           Data.Functor.Identity (Identity)
import           Data.Functor.Product (Product (Pair))


-- layers --------------------------------------------------------------------
import           Control.Monad.Interface.Mask (MonadMask, mask)
import           Control.Monad.Lift
                     ( MonadTransControl
                     , extract
                     , lift
                     , peel
                     , restore
                     , suspend
                     )


------------------------------------------------------------------------------
-- | The 'MonadTry' type class provides a single operation 'mtry', which is a
-- way to observe short-circuiting in monads. The name refers to the fact that
-- @mtry@ is a generalised version of 'Control.Monad.Interface.Exception.try':
-- whereas @try@ guards against the specific case of a
-- 'Control.Monad.Interface.Exception.MonadException' short-circuiting due to
-- an exception being thrown, it can still short-circuit in other ways: e.g.,
-- if a @'Control.Monad.Trans.Maybe.MaybeT' 'IO'@ returns
-- 'Control.Monad.mzero' ('Nothing'). The action returned by 'mtry' is
-- guaranteed to never short-circuit.
--
-- Nearly every monad should have an instance of @MonadTry@, with the
-- exception of CPS-style monads whose (possible) short-circuiting is
-- impossible to observe. Instances for every base monad in the @base@ and
-- @transformers@ packages. @mtry@ has a default definition that only needs
-- to be overridden for monads which actually short-circuit, so it costs
-- very little to add an instance of @MonadTry@ to a monad.
--
-- Minimal complete definition: instance head only.
class MonadMask m => MonadTry m where
    -- | 'mtry' takes a monadic action in @m@ and returns a new monadic value
    -- in @m@ which is guaranteed not to short-circuit. If the action @m@ that
    -- was given to @mtry@ would have short-circuited, it returns @Left m@,
    -- otherwise it returns @Right a@, where @a@ is the value returned by the
    -- computation @m@.
    mtry :: m a -> m (Either (m a) a)
    mtry = liftM Right
    {-# INLINE mtry #-}


------------------------------------------------------------------------------
instance MonadTry Identity


------------------------------------------------------------------------------
instance (MonadTry f, MonadTry g) => MonadTry (Product f g) where
    mtry (Pair f g) = Pair
        (liftM (either (Left . (flip Pair g)) Right) (mtry f))
        (liftM (either (Left . (Pair f)) Right) (mtry g))


------------------------------------------------------------------------------
instance MonadTry Maybe where
    mtry = return . maybe (Left Nothing) Right
    {-# INLINE mtry #-}


------------------------------------------------------------------------------
instance MonadTry (Either e) where
    mtry = return . either (Left . Left) Right
    {-# INLINE mtry #-}


------------------------------------------------------------------------------
instance MonadTry [] where
    mtry [] = [Left []]
    mtry (x:_) = [Right x]
    {-# INLINE mtry #-}


------------------------------------------------------------------------------
instance MonadTry ((->) r)


------------------------------------------------------------------------------
instance MonadTry IO where
    mtry m = try' m >>= return . either (Left . throwIO) Right
      where
        try' :: IO a -> IO (Either SomeException a)
        try' = try
    {-# INLINE mtry #-}


------------------------------------------------------------------------------
instance MonadTry (ST s)


------------------------------------------------------------------------------
instance MonadTry (L.ST s)


------------------------------------------------------------------------------
instance MonadTry STM where
    mtry m = try' m >>= return . either (Left . throwSTM) Right
      where
        try' :: STM a -> STM (Either SomeException a)
        try' m' = catchSTM (liftM Right m') (return . Left)
    {-# INLINE mtry #-}


------------------------------------------------------------------------------
data P (t :: (* -> *) -> * -> *) = P


------------------------------------------------------------------------------
instance (MonadTransControl t, MonadMask (t m), MonadTry m) => MonadTry (t m)
  where
    mtry m = do
        state <- suspend
        ma <- lift . mtry $ peel state m
        case ma of
            Left m' -> return . Left $ lift m' >>= uncurry restore
            Right (result, state') -> case extract (P :: P t) result of
                Nothing -> return . Left $ restore result state'
                Just _ -> liftM Right $ restore result state'
    {-# INLINE mtry #-}


------------------------------------------------------------------------------
-- | When you want to acquire a resource, do some work with it, and then
-- release the resource, it is a good idea to use 'bracket', because @bracket@
-- will install the necessary handler to release the resource in the event
-- that the monad short circuits during the computation. If the monad
-- short-circuits, then @bracket@ will re-return the monad in its
-- short-circuited state (after performing the release).
--
-- A common example is opening a file:
--
-- > bracket
-- >   (openFile "filename" ReadMode)
-- >   (hClose)
-- >   (\fileHandle -> do { ... })
--
-- The arguments to @bracket@ are in this order so that we can partially apply
-- it, e.g.:
--
-- > withFile name mode = bracket (openFile name mode) hClose
--
bracket :: MonadTry m
    => m a         -- ^ computation to run first (\"acquire resource\")
    -> (a -> m b)  -- ^ computation to run last (\"release resource\")
    -> (a -> m c)  -- ^ computation to run in-between
    -> m c         -- ^ returns the value from the in-between computation
bracket acquire release run = mask $ \unmask -> do
    a <- acquire
    unmask (run a) `finally` release a
{-# INLINE bracket #-}


------------------------------------------------------------------------------
-- | A variant of 'bracket' where the return value from the first computation
-- is not required.
bracket_ :: MonadTry m => m a -> m b -> m c -> m c
bracket_ acquire release run = bracket acquire (const release) (const run)
{-# INLINE bracket_ #-}


------------------------------------------------------------------------------
-- | Like 'bracket', but only performs the final action if the monad
-- short-circuited during the in-between computation.
bracketOnError :: MonadTry m => m a -> (a -> m b) -> (a -> m c) -> m c
bracketOnError acquire release run = mask $ \unmask -> do
    a <- acquire
    unmask (run a) `onException` release a
{-# INLINE bracketOnError #-}


------------------------------------------------------------------------------
-- | A specialised variant of 'bracket' with just a computation to run
-- afterward.
finally :: MonadTry m => m a -> m b -> m a
finally m sequel = mask $ \unmask -> do
    r <- unmask m `onException` sequel
    _ <- sequel
    return r
{-# INLINE finally #-}


------------------------------------------------------------------------------
-- | Like 'finally', but only performs the final action if the monad
-- short-circuited during the computation.
onException :: MonadTry m => m a -> m b -> m a
onException m sequel = mask $ \unmask -> do
    mtry (unmask m) >>= either (sequel >>) return
{-# INLINE onException #-}
