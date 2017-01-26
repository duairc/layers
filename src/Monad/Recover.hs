{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -fno-warn-warnings-deprecations #-}

#ifdef LANGUAGE_ConstraintKinds
{-# LANGUAGE ConstraintKinds #-}
#endif

#include <docmacros.h>
#include <overlap.h>

{-|

This module defines the 'MonadRecover' G(monadinterface,interface). It, along
with its sister G(monadinterface,interface) 'MonadAbort', is inspired by the
<M(monad-abort-fd,Control-Monad-Abort-Class)#t:MonadRecover eponymous interfaces>
from the
<M(monad-abort-fd,Control-Monad-Abort-Class) Control.Monad.Abort.Class> module
of the H(monad-abort-fd) package. It consists of:

  * The 'MonadRecover' constraint.
  * The 'recover' operation.
  * Instances of 'MonadRecover':

      * For the following G(basemonad,base monads):

          * 'Either'
          * @[@@]@
          * 'Maybe'
          * 'IO'
          * 'STM'

      * For arbitrary G(innermonad,inner monads) wrapped by one of the
      following G(monadlayer,monad layers):

          * 'ErrorT'
          * 'ExceptT'
          * 'ListT'
          * 'MaybeT'

      * G(universalpassthroughinstance,Pass-through instances) for:

          * Any G(innermonad,inner monad) with an existing 'MonadRecover'
          instance wrapped by any G(monadlayer,monad layer) implementing
          'Control.Monad.Lift.MonadTransControl'.
          * The 'Product' of any two G(monadictype,monadic types) which both
          have existing 'MonadRecover' instances.
          * The <M(mmorph,Control-Monad-Trans-Compose)#t:ComposeT composition>
          of two G(monadlayer,monad layers) wrapped around an
          G(innermonad,inner monad), where either the
          G(innermonad,inner monad) or one or more of the composed
          G(monadlayer,monad layers) has an existing instance for
          'MonadRecover'.

  * The following alternate versions of 'recover', analogous to the 'catch'
  family of operations (as defined in "Monad.Catch" and "Control.Exception",
  modulo the marshalling of 'Control.Exception.Exception's to and from
  'Control.Exception.SomeException'):

      * 'recoverJust'
      * 'handle'
      * 'handleJust'
      * 'try'
      * 'tryJust'

  * The \"bracket\" family of operations (as defined in "Monad.Try" and
  "Control.Exception"). Unlike the \"bracket\" operations in "Monad.Try",
  which can recover from a G(shortcircuit,short-circuit) in any
  G(monadlayer,monad layer) anywhere in the G(monadtransformerstack,stack),
  these \"bracket\" operations can only recover from an 'abort' in the
  G(outerlayers,outermost) G(monadlayer,layer) that implements 'MonadRecover'.

      * 'bracket'
      * 'bracket_'
      * 'bracketOnError'
      * 'finally'
      * 'onException'
      * 'orElse' (actually from the H(stm) package)

The 'Monad.Catch.MonadCatch' and 'Monad.Error.MonadError'
G(monadinterface,interfaces) are both built on top of 'MonadRecover'.

-}

module Monad.Recover
    ( MonadRecover (recover)

    -- * \"catch\" operations
    , recoverJust
    , handle
    , handleJust
    , try
    , tryJust

    -- * \"bracket\" operations
    , bracket
    , bracket_
    , bracketOnError
    , finally
    , onException
    , orElse
    )
where

-- base ----------------------------------------------------------------------
import           Control.Exception (SomeException, catch)
import           Control.Monad (liftM, mplus)
#if MIN_VERSION_base(4, 3, 0)
import           GHC.Conc.Sync (STM, catchSTM)
#else
import           GHC.Conc (STM, catchSTM)
#endif
#if !MIN_VERSION_base(4, 6, 0)
import           Prelude hiding (catch)
#endif


-- layers --------------------------------------------------------------------
import           Control.Monad.Lift.Top (MonadTopControl, controlT)
import           Monad.Abort (MonadAbort, abort)


#if MIN_VERSION_mmorph(1, 0, 1)
-- mmorph --------------------------------------------------------------------
import           Control.Monad.Trans.Compose (ComposeT (ComposeT))


#endif
-- transformers --------------------------------------------------------------
#if !MIN_VERSION_transformers(0, 6, 0)
import           Control.Monad.Trans.Error (ErrorT (ErrorT), Error)
#endif
#if MIN_VERSION_transformers(0, 4, 0)
import           Control.Monad.Trans.Except (ExceptT (ExceptT))
#endif
import           Control.Monad.Trans.Maybe (MaybeT)
import           Control.Monad.Trans.List (ListT)
#if MIN_VERSION_transformers(0, 3, 0)
import           Data.Functor.Product (Product (Pair))
#endif


------------------------------------------------------------------------------
-- | The @'MonadRecover' e@ constraint matches monads whose
-- G(computation,computations) can 'recover' from a failure caused by a call
-- to 'Monad.Abort.abort'.
--
-- Every monad which permits an instance 'Control.Monad.MonadPlus' (of the
-- <HW(MonadPlus) left catch> variety) trivially permits an instance of
-- 'MonadRecover': for these instances, the @e@ parameter is fixed to @()@, as
-- there is no @e@ value which can be recovered from a
-- \"G(shortcircuit,zero)\".
--
-- The other class of monads that permit a 'MonadRecover' instance are the
-- 'Either'-like monads (including 'IO'): these monads actually store the @e@
-- parameter passed to the 'abort' operation on failure, hence it can later be
-- retrieved using the 'recover' operation.
--
-- Minimal complete definition: 'recover'.
class MonadAbort e m => MonadRecover e m | m -> e where
    -- | In addition to the 'MonadAbort' \"G(shortcircuit,zero)\" law, the
    -- following laws hold for valid instances of 'MonadRecover':
    --
    --     [Left Identity] @'recover' ('Monad.Abort.abort' e) (\\_ -> m) ≡ m@
    --     [Right Identity] @'recover' m 'Monad.Abort.abort' ≡ m@
    --     [Associativity] @'recover' m (\\_ -> 'recover' n (\\_ -> o)) ≡ 'recover' ('recover' m (\\_ -> n)) (\\_ -> o)@
    --     [Left Catch] @'recover' ('return' a) _ ≡ 'return' a@
    --     [Recoverability] @'recover' ('Monad.Abort.abort' e) 'return' ≡ 'return' e@
    recover :: m a -> (e -> m a) -> m a

#ifdef MinimalPragma
    {-# MINIMAL recover #-}

#endif

------------------------------------------------------------------------------
instance MonadRecover e (Either e) where
    recover m h = either h Right m


------------------------------------------------------------------------------
instance MonadRecover () ([]) where
    recover m h = mplus m (h ())


------------------------------------------------------------------------------
instance MonadRecover () Maybe where
    recover m h = mplus m (h ())


------------------------------------------------------------------------------
instance MonadRecover SomeException IO where
    recover = catch


------------------------------------------------------------------------------
instance MonadRecover SomeException STM where
    recover = catchSTM


#if !MIN_VERSION_transformers(0, 6, 0)
------------------------------------------------------------------------------
instance (Error e, Monad m) => MonadRecover e (ErrorT e m) where
    recover (ErrorT m) h = ErrorT $ m >>= either
        (\e -> let ErrorT m' = h e in m')
        (return . Right)
    {-# INLINABLE recover #-}


#endif
#if MIN_VERSION_transformers(0, 4, 0)
------------------------------------------------------------------------------
instance Monad m => MonadRecover e (ExceptT e m) where
    recover (ExceptT m) h = ExceptT $ m >>= either
        (\e -> let ExceptT m' = h e in m')
        (return . Right)
    {-# INLINABLE recover #-}


#endif
------------------------------------------------------------------------------
instance Monad m => MonadRecover () (ListT m) where
    recover m h = mplus m (h ())


------------------------------------------------------------------------------
instance Monad m => MonadRecover () (MaybeT m) where
    recover m h = mplus m (h ())


#if MIN_VERSION_transformers(0, 3, 0)
------------------------------------------------------------------------------
instance (MonadRecover e f, MonadRecover e g) => MonadRecover e (Product f g)
  where
    recover (Pair f g) h = Pair
        (recover f (\e -> let Pair f' _ = h e in f'))
        (recover g (\e -> let Pair _ g' = h e in g'))


#endif
#if MIN_VERSION_mmorph(1, 0, 1)
------------------------------------------------------------------------------
instance MonadRecover e (f (g m)) => MonadRecover e (ComposeT f g m) where
    recover (ComposeT m) h = ComposeT
        (recover m (\e -> let ComposeT m' = h e in m'))


#endif
------------------------------------------------------------------------------
instance __OVERLAPPABLE__
    ( MonadTopControl t m
    , MonadRecover e m
    , MonadAbort e (t m)
    )
  =>
    MonadRecover e (t m)
  where
    recover m h = controlT (\peel -> recover (peel m) (peel . h))
    {-# INLINABLE recover #-}


------------------------------------------------------------------------------
-- | The function 'recoverJust' is like 'recover', but it takes an extra
-- argument which is an /exception predicate/, a function which selects which
-- type of exceptions we're interested in. It is analogous to
-- 'Monad.Catch.catchJust' in "Monad.Catch" and "Control.Exception".
--
-- Any other exceptions which are not matched by the predicate are re-raised,
-- and may be caught by an enclosing 'recover', 'recoverJust', etc.
recoverJust :: MonadRecover e m => (e -> Maybe b) -> m a -> (b -> m a) -> m a
recoverJust p m handler = recover m (\e -> maybe (abort e) handler (p e))
{-# INLINABLE recoverJust #-}


------------------------------------------------------------------------------
-- | A version of 'recover' with the arguments swapped around; useful in
-- situations where the code for the handler is shorter.
handle :: MonadRecover e m => (e -> m a) -> m a -> m a
handle = flip recover
{-# INLINABLE handle #-}


------------------------------------------------------------------------------
-- | A version of 'recoverJust' with the arguments swapped around (see
-- 'handle').
handleJust :: MonadRecover e m => (e -> Maybe b) -> (b -> m a) -> m a -> m a
handleJust = flip . recoverJust
{-# INLINABLE handleJust #-}


------------------------------------------------------------------------------
-- | Similar to 'recover', but returns an 'Either' result which is
-- @('Left' e)@ if the given computation 'abort'ed, or @('Right' a)@ if
-- completed normally.
--
-- @'try' a = 'handle' ('return' '.' 'Left') '.' 'liftM' 'Right'@
try :: MonadRecover e m => m a -> m (Either e a)
try = handle (return . Left) . liftM Right
{-# INLINABLE try #-}


------------------------------------------------------------------------------
-- | A variant of 'try' that takes an exception predicate to select which
-- exceptions are caught (c.f. 'recoverJust'). If the exception does not
-- match the predicate, it is re-thrown.
tryJust :: MonadRecover e m => (e -> Maybe b) -> m a -> m (Either b a)
tryJust p = handleJust p (return . Left) . liftM Right
{-# INLINABLE tryJust #-}


------------------------------------------------------------------------------
-- | This 'bracket' is analogous to the 'Monad.Try.bracket' in "Monad.Try" and
-- "Control.Monad.Exception".
--
-- Note: if you are trying to ensure that your release\/finalizer\/cleanup
-- action is always run, you probably want to use the 'Monad.Try.bracket' from
-- "Monad.Try" instead, which can recover from a G(shortcircuit,short-circuit)
-- in any G(monadlayer,monad layer) anywhere in the
-- G(monadtransformerstack,stack). This 'bracket' can only recover from an
-- 'abort' in the G(outerlayers,outermost) G(monadlayer,layer) that implements
-- 'MonadRecover'.
bracket :: MonadRecover e m
    => m a         -- ^ G(computation,computation) to run first (\"acquire resource\")
    -> (a -> m b)  -- ^ G(computation,computation) to run last (\"release resource\")
    -> (a -> m c)  -- ^ G(computation,computation) to run in-between
    -> m c         -- ^ returns the value from the in-between G(computation,computation)
bracket acquire release run = do
    a <- acquire
    run a `finally` release a
{-# INLINABLE bracket #-}


------------------------------------------------------------------------------
-- | A variant of 'bracket' where the return value from the first
-- G(computation,computation) is not required.
bracket_ :: MonadRecover e m => m a -> m b -> m c -> m c
bracket_ acquire release run = bracket acquire (const release) (const run)
{-# INLINABLE bracket_ #-}


------------------------------------------------------------------------------
-- | Like 'bracket', but only performs the final G(computation,action) if the
-- in-between G(computation,computation) G(shortcircuit,short-circuited).
bracketOnError :: MonadRecover e m => m a -> (a -> m b) -> (a -> m c) -> m c
bracketOnError acquire release run = do
    a <- acquire
    run a `onException` release a
{-# INLINABLE bracketOnError #-}


------------------------------------------------------------------------------
-- | A specialised variant of 'bracket' with just a G(computation,computation)
-- to run afterward.
finally :: MonadRecover e m => m a -> m b -> m a
finally m sequel = do
    r <- m `onException` sequel
    _ <- sequel
    return r
{-# INLINABLE finally #-}


------------------------------------------------------------------------------
-- | Like 'finally', but only performs the final G(computation,action) if
-- the G(computation,computation) G(shortcircuit,short-circuited).
onException :: MonadRecover e m => m a -> m b -> m a
onException m sequel = recover m (\e -> sequel >> abort e)
{-# INLINABLE onException #-}


------------------------------------------------------------------------------
-- | Tries the first G(computation,action), and if it G(shortcircuit,fails),
-- tries the second G(computation,action).
orElse :: MonadRecover e m => m a -> m a -> m a
orElse a b = recover a (const b)
{-# INLINABLE orElse #-}
