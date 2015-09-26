{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
#if LANGUAGE_ConstraintKinds
{-# LANGUAGE ConstraintKinds #-}
#endif

#include <macros.h>

{-|

This module defines the 'MonadTry' G(monadinterface,interface). It, along with
its sister G(monadinterface,interface) 'Monad.Mask.MonadMask', is designed to
be largely compatible with the "Control.Exception" module from H(base). It
consists of:

  * The 'MonadTry' constraint.
  * The 'mtry' operation.

  * Instances of 'MonadTry':

      * For every G(basemonad,base monad) in the H(base) and H(transformers)
      packages:

          * 'Either'
          * @-@@>@
          * 'Identity'
          * 'IO'
          * @[@@]@
          * 'Maybe'
          * 'Proxy'
          * Lazy 'L.ST'
          * Strict 'ST'
          * 'STM'

      * G(universalpassthroughinstance,Pass-through instances) for:

          * Any G(innermonad,inner monad) with an existing 'MonadTry'
          instance wrapped by any G(monadlayer,monad layer) implementing
          'Control.Monad.Lift.MonadTransControl'.
          * The 'Product' of any two G(monadictype,monadic types) which both
          have existing 'MonadTry' instances.
          * The <M(mmorph,Control-Monad-Trans-Compose)#t:ComposeT composition>
          of two G(monadlayer,monad layers) wrapped around an
          G(innermonad,inner monad), where either the
          G(innermonad,inner monad) or one or more of the composed
          G(monadlayer,monad layers) has an existing instance for
          'MonadTry'.

  * The \"bracket\" family of functions (as defined in "Control.Exception"):

      * 'bracket'
      * 'bracket_'
      * 'bracketOnError'
      * 'finally'
      * 'onException'
      * 'orElse' (actually from the H(stm) package)

-}

module Monad.Try
    ( MonadTry (mtry)
    , bracket
    , bracket_
    , bracketOnError
    , finally
    , onException
    , orElse
    )
where

-- base ----------------------------------------------------------------------
#if MIN_VERSION_mmorph(1, 0, 1)
import           Control.Arrow (left)
#endif
import           Control.Exception (SomeException, throwIO, try)
import           Control.Monad (liftM)
import           Control.Monad.ST (ST)
import qualified Control.Monad.ST.Lazy as L (ST)
#if MIN_VERSION_base(4, 3, 0)
import           GHC.Conc.Sync (STM, catchSTM, throwSTM)
#else
import           GHC.Conc (STM, catchSTM, unsafeIOToSTM)
#endif
#if MIN_VERSION_base(4, 7, 0)
import           Data.Proxy (Proxy)
#endif


#if MIN_VERSION_mmorph(1, 0, 1)
-- mmorph --------------------------------------------------------------------
import           Control.Monad.Trans.Compose (ComposeT (ComposeT))
#endif


-- transformers --------------------------------------------------------------
import           Data.Functor.Identity (Identity)
#if MIN_VERSION_transformers(0, 3, 0)
import           Data.Functor.Product (Product (Pair))
#endif


-- layers --------------------------------------------------------------------
import           Monad.Mask (MonadMask, mask)
import           Control.Monad.Lift.Top
                     ( MonadTopControl
                     , extractT
                     , liftT
                     , suspendT
                     , resumeT
                     , captureT
                     )


------------------------------------------------------------------------------
-- | The 'MonadTry' type class provides a single operation 'mtry', which is a
-- generalised way to observe G(shortcircuit,short-circuiting) in monads.
-- The name refers to the fact that 'mtry' is a generalised version of
-- 'Monad.Exception.try': whereas 'try' guards against the specific case of a
-- monad G(shortcircuit,short-circuiting) from a call to 'Monad.Throw.throw',
-- there can be other ways that a monad can G(shortcircuit,short-circuit).
-- For example, the monad @'Control.Monad.Trans.Maybe.MaybeT' 'IO'@ can be
-- G(shortcircuit,short-circuited) by calling 'Control.Monad.mzero'
-- ('Nothing') or by raising an exception in the underlying 'IO' monad. The
-- G(computation,computation) returned by 'mtry' is guaranteed never to
-- G(shortcircuit,short-circuit), even if the G(monadtransformerstack,stack)
-- is built from many G(shortcircuit,short-circuiting) different
-- G(monadlayer,layers).
--
-- Nearly every monad should permit an instance of 'MonadTry', with the
-- exception of CPS-style monads whose (possible)
-- G(shortcircuit,short-circuiting) is impossible to observe. Instances are
-- provided for every G(basemonad, base monad) in the H(base) and
-- H(transformers) packages. 'mtry' has a default definition that only needs
-- to be overridden for monads which actually G(shortcircuit,short-circuit),
-- so it costs very little to add an instance of 'MonadTry' to a monad.
--
-- Minimal complete definition: instance head only.
class MonadMask m => MonadTry m where
    -- | 'mtry' takes a G(computation,computation) in @m@ and returns a new
    -- monadic value in @m@ which is guaranteed not to
    -- G(shortcircuit,short-circuit). If the original
    -- G(computation,computation) @m@
    -- given to 'mtry' would have G(shortcircuit,short-circuited), the
    -- resulting value returned by 'mtry' is @'Left' m@.
    -- Otherwise, 'mtry' returns @'Right' a@, where @a@ is the value returned
    -- by the computation @m@.
    --
    -- Instances should satisfy the following laws:
    --
    -- [Preserve-Unit]
    --     @'mtry' ('return' a) ≡ 'return' ('Right' a)@
    --
    -- [Implies-Non-Zero]
    --     @('mtry' m ≡ 'liftM' 'Right' m) ^ ((a ≢ b) ⇒ ('return' a ≢ 'return' b)) ⇒ (∃f. m '>>=' f ≢ m)@
    --
    -- [Implies-Zero]
    --     @('mtry' m ≡ 'return' ('Left' m)) ⇒ (∀f. m '>>=' f ≡ m)@
    mtry :: m a -> m (Either (m a) a)
    mtry = liftM Right

#ifdef MINIMALSupport
    {-# MINIMAL mtry #-}
#endif

------------------------------------------------------------------------------
instance MonadTry Identity


#if MIN_VERSION_transformers(0, 3, 0)
------------------------------------------------------------------------------
instance (MonadTry f, MonadTry g) => MonadTry (Product f g) where
    mtry (Pair f g) = Pair
        (liftM (either (Left . (flip Pair g)) Right) (mtry f))
        (liftM (either (Left . (Pair f)) Right) (mtry g))
#endif


#if MIN_VERSION_mmorph(1, 0, 1)
------------------------------------------------------------------------------
instance MonadTry (f (g m)) => MonadTry (ComposeT f g m) where
    mtry (ComposeT m) = ComposeT (liftM (left ComposeT) (mtry m))
#endif


------------------------------------------------------------------------------
instance MonadTry Maybe where
    mtry = return . maybe (Left Nothing) Right


------------------------------------------------------------------------------
instance MonadTry (Either e) where
    mtry = return . either (Left . Left) Right


------------------------------------------------------------------------------
instance MonadTry [] where
    mtry [] = [Left []]
    mtry (x:xs) = Right x : map Right xs


------------------------------------------------------------------------------
instance MonadTry ((->) r)


------------------------------------------------------------------------------
instance MonadTry IO where
    mtry m = try' m >>= return . either (Left . throwIO) Right
      where
        try' :: IO a -> IO (Either SomeException a)
        try' = try


------------------------------------------------------------------------------
instance MonadTry (ST s)


------------------------------------------------------------------------------
instance MonadTry (L.ST s)


------------------------------------------------------------------------------
instance MonadTry STM where
    mtry m = try' m >>= return . either (Left . throwSTM) Right
      where
#if !MIN_VERSION_base(4, 3, 0)
        throwSTM = unsafeIOToSTM . throwIO
#endif
        try' :: STM a -> STM (Either SomeException a)
        try' m' = catchSTM (liftM Right m') (return . Left)


#if MIN_VERSION_base(4, 7, 0)
------------------------------------------------------------------------------
instance MonadTry Proxy


#endif
------------------------------------------------------------------------------
data Pt (t :: (* -> *) -> * -> *) = Pt


------------------------------------------------------------------------------
data Pm (m :: * -> *) = Pm


------------------------------------------------------------------------------
instance _OVERLAPPABLE (MonadTopControl t m, MonadMask (t m), MonadTry m) =>
    MonadTry (t m)
  where
    mtry (m :: t m a) = do
        state <- captureT
        ma <- liftT . mtry $ suspendT m state
        case ma of
            Left m' -> return . Left $ liftT m' >>= resumeT
            Right (result, state') ->
                case extractT (Pt :: Pt t) (Pm :: Pm m) result of
                    Nothing ->  return . Left $ resumeT (result, state')
                    Just _ -> liftM Right $ resumeT (result, state')
    {-# INLINE mtry #-}


------------------------------------------------------------------------------
-- | When you want to acquire a resource, do some work with it, and then
-- release the resource, it is a good idea to use 'bracket', because 'bracket'
-- will install the necessary handler to release the resource in the event
-- that the monad G(shortcircuit,short-circuits) during the
-- G(computation,computation). If the monad G(shortcircuit,short-circuits),
-- then 'bracket' will re-return the monad in its
-- G(shortcircuit,short-circuited) state (after performing the release).
--
-- A common example is opening a file:
--
-- @'bracket'
--   ('System.IO.openFile' "filename" 'System.IO.ReadMode')
--   ('System.IO.hClose')
--   (\\fileHandle -> do { ... })@
--
-- The arguments to @bracket@ are in this order so that we can partially apply
-- it, e.g.:
--
-- @'System.IO.withFile' name mode = 'bracket' ('System.IO.openFile' name mode) 'System.IO.hClose'@
--
bracket :: MonadTry m
    => m a         -- ^ G(computation,computation) to run first (\"acquire resource\")
    -> (a -> m b)  -- ^ G(computation,computation) to run last (\"release resource\")
    -> (a -> m c)  -- ^ G(computation,computation) to run in-between
    -> m c         -- ^ returns the value from the in-between G(computation,computation)
bracket acquire release run = mask $ \restore -> do
    a <- acquire
    restore (run a) `finally` release a
{-# INLINABLE bracket #-}


------------------------------------------------------------------------------
-- | A variant of 'bracket' where the return value from the first
-- G(computation,computation) is not required.
bracket_ :: MonadTry m => m a -> m b -> m c -> m c
bracket_ acquire release run = bracket acquire (const release) (const run)
{-# INLINABLE bracket_ #-}


------------------------------------------------------------------------------
-- | Like 'bracket', but only performs the final G(computation,action) if the
-- monad G(shortcircuit,short-circuited) during the in-between
-- G(computation,computation).
bracketOnError :: MonadTry m => m a -> (a -> m b) -> (a -> m c) -> m c
bracketOnError acquire release run = mask $ \restore -> do
    a <- acquire
    restore (run a) `onException` release a
{-# INLINABLE bracketOnError #-}


------------------------------------------------------------------------------
-- | A specialised variant of 'bracket' with just a G(computation,computation)
-- to run afterward.
finally :: MonadTry m => m a -> m b -> m a
finally m sequel = mask $ \restore -> do
    r <- restore m `onException` sequel
    _ <- sequel
    return r
{-# INLINABLE finally #-}


------------------------------------------------------------------------------
-- | Like 'finally', but only performs the final G(computation,action) if
-- the monad G(shortcircuit,short-circuited) during the
-- G(computation,computation).
onException :: MonadTry m => m a -> m b -> m a
onException m sequel = mask $ \restore -> do
    mtry (restore m) >>= either (sequel >>) return
{-# INLINABLE onException #-}


------------------------------------------------------------------------------
-- | Tries the first G(computation,action), and if it G(shortcircuit,fails),
-- tries the second G(computation,action).
orElse :: MonadTry m => m a -> m a -> m a
orElse a b = mask $ \restore -> mtry (restore a) >>= either (const b) return
{-# INLINABLE orElse #-}
