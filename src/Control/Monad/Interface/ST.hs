{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

{-|

This module exports:

    1. The 'MonadST' type class and its operations 'newRef', 'readRef',
    'writeRef' and 'atomicModifyRef'.

    2. Instances of 'MonadST' for 'IO', 'STM', strict 'ST' and lazy
    'L.ST'.

    3. A universal pass-through instance of 'MonadST' for any existing
    @MonadST@ wrapped by a 'MonadLayer'.

    4. The utility operations 'atomicModifyRef'', 'atomicWriteRef',
    'modifyRef' and 'modifyRef''.

-}

module Control.Monad.Interface.ST
    ( -- * The @MonadST@ class
      MonadST (newRef, readRef, writeRef, atomicModifyRef)
    , atomicModifyRef'
    , atomicWriteRef
    , modifyRef
    , modifyRef'
    )
where

--  base ---------------------------------------------------------------------
import           Control.Monad.ST (ST)
import qualified Control.Monad.ST.Lazy as L (ST)
import           Data.IORef
                     ( IORef
                     , newIORef
                     , readIORef
                     , writeIORef
                     , atomicModifyIORef
                     )
import           Data.STRef (STRef, newSTRef, readSTRef, writeSTRef)
import qualified Data.STRef.Lazy as L (newSTRef, readSTRef, writeSTRef)
import           GHC.Conc.Sync (STM, TVar, newTVar, readTVar, writeTVar)


-- transformers --------------------------------------------------------------
import           Data.Functor.Product (Product (Pair))


-- layers --------------------------------------------------------------------
import           Control.Monad.Lift (MonadTrans, lift)


------------------------------------------------------------------------------
-- | The type class 'MonadST' represents the class of \"'ST'-like\" monads
-- (i.e., monads which have mutable variables and operations for mutating the
-- values contained therein). The @ref@ parameter is the type of the mutable
-- variable; e.g., for 'IO', @ref@ is 'IORef'.
--
-- Minimal complete definition: 'newRef', 'readRef', 'writeRef'.
class Monad m => MonadST ref m | m -> ref where
    -- | Create a new mutable variable holding the value supplied.
    newRef :: a -> m (ref a)

    -- | Return the current value stored in the mutable variable.
    readRef :: ref a -> m a

    -- | Write the supplied value into the mutable variable 
    writeRef :: ref a -> a -> m ()

    -- | Atomically modifies the contents of a mutable variable.
    --
    -- This function is useful for using mutable varibales in a safe way in a
    -- multithreaded program. If you only have one mutable variable, then
    -- using 'atomicModifyRef' to access and modify it will prevent race
    -- conditions.
    --
    -- Extending the atomicity to multiple mutable variables is problematic,
    -- so it is recommended that if you need to do anything more complicated
    -- then using 'Control.Concurrent.MVar.MVar' instead is a good idea.
    --
    -- 'atomicModifyRef' does not apply the function strictly. This is
    -- important to know even if all you are doing is replacing the value.
    -- For example, this will leak memory:
    --
    -- > ref <- newIORef 1
    -- > forever $ atomicModifyRef ref (\_ -> (2, ()))
    --
    -- Use 'atomicModifyRef'' or 'atomicWriteRef' to avoid this problem.
    atomicModifyRef :: ref a -> (a -> (a, b)) -> m b

    atomicModifyRef ref f = do
        a <- readRef ref
        let (a', b) = f a
        writeRef ref a'
        return b
    {-# INLINE atomicModifyRef #-}


------------------------------------------------------------------------------
instance MonadST IORef IO where
    newRef = newIORef
    {-# INLINE newRef #-}
    readRef = readIORef
    {-# INLINE readRef #-}
    writeRef = writeIORef
    {-# INLINE writeRef #-}
    atomicModifyRef = atomicModifyIORef
    {-# INLINE atomicModifyRef #-}


------------------------------------------------------------------------------
instance MonadST (STRef s) (L.ST s) where
    newRef = L.newSTRef
    {-# INLINE newRef #-}
    readRef = L.readSTRef
    {-# INLINE readRef #-}
    writeRef = L.writeSTRef
    {-# INLINE writeRef #-}


------------------------------------------------------------------------------
instance MonadST (STRef s) (ST s) where
    newRef = newSTRef
    {-# INLINE newRef #-}
    readRef = readSTRef
    {-# INLINE readRef #-}
    writeRef = writeSTRef
    {-# INLINE writeRef #-}


------------------------------------------------------------------------------
instance MonadST TVar STM where
    newRef = newTVar
    {-# INLINE newRef #-}
    readRef = readTVar
    {-# INLINE readRef #-}
    writeRef = writeTVar
    {-# INLINE writeRef #-}


------------------------------------------------------------------------------
instance (MonadST ref f, MonadST ref g) =>
    MonadST ref (Product f g)
  where
    newRef a = Pair (newRef a) (newRef a)
    {-# INLINE newRef #-}
    readRef ref = Pair (readRef ref) (readRef ref)
    {-# INLINE readRef #-}
    writeRef ref a = Pair (writeRef ref a) (writeRef ref a)
    {-# INLINE writeRef #-}
    atomicModifyRef ref f = Pair
        (atomicModifyRef ref f)
        (atomicModifyRef ref f)
    {-# INLINE atomicModifyRef #-}


------------------------------------------------------------------------------
instance (MonadTrans t, Monad (t m), MonadST ref m) => MonadST ref (t m) where
    newRef = lift . newRef
    {-# INLINE newRef #-}
    readRef = lift . readRef
    {-# INLINE readRef #-}
    writeRef ref = lift . writeRef ref
    {-# INLINE writeRef #-}
    atomicModifyRef ref = lift . atomicModifyRef ref
    {-# INLINE atomicModifyRef #-}


------------------------------------------------------------------------------
-- | Strict version of 'atomicModifyRef'. This forces both the value stored in
-- the mutable variable as well as the value returned.
atomicModifyRef' :: MonadST ref m => ref a -> (a -> (a, b)) -> m b
atomicModifyRef' ref f = do
    b <- atomicModifyRef ref (\x -> let (a, b) = f x in (a, a `seq` b))
    return $! b
{-# INLINE atomicModifyRef' #-}


------------------------------------------------------------------------------
-- | Mutate the contents of a mutable variable.
--
-- Be warned that 'modifyRef' does not apply the function strictly. This means
-- if the program calls @modifyRef@ many times, but seldomly uses the value,
-- thunks will pile up in memory resulting in a space leak. This is a common
-- mistake made when using a mutable varible as a counter. For example, the
-- following will likely produce a stack overflow:
--
-- > ref <- newRef 0
-- > replicateM_ 1000000 $ modifyRef ref (+1)
-- > readRef ref >>= print
--
-- To avoid this problem, use 'modifyRef'' instead.
modifyRef :: MonadST ref m => ref a -> (a -> a) -> m ()
modifyRef ref f = readRef ref >>= writeRef ref . f
{-# INLINE modifyRef #-}


------------------------------------------------------------------------------
-- | Strict version of 'modifyRef'.
modifyRef' :: MonadST ref m => ref a -> (a -> a) -> m ()
modifyRef' ref f = do
    x <- readRef ref
    let x' = f x
    x' `seq` writeRef ref x'
{-# INLINE modifyRef' #-}


------------------------------------------------------------------------------
-- | Variant of 'writeRef' with the \"barrier to reordering\" property that
-- 'atomicModifyRef' has. 
atomicWriteRef :: MonadST ref m => ref a -> a -> m ()
atomicWriteRef ref a = do
    x <- atomicModifyRef ref (\_ -> (a, ()))
    x `seq` return ()
{-# INLINE atomicWriteRef #-}
