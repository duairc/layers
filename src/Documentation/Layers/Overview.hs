{-|

This document discusses extensively the motivation behind the @layers@ package
and explains the design decisions taken in its construction.

-}

module Documentation.Layers.Overview
{-# WARNING
    [ "This module exports no types, functions, classes or instances. "
    , "It exists solely for the Haddock documentation it produces. "
    , "You should not ever need to import it."
    ]
  #-}
    (
      -- * Monads and monad transformers
      -- $monads

      -- * Limitations of the existing machinery
      -- ** @lift@ is not powerful enough
      -- $lift

      -- ** Non-modularity, code duplication
      -- $non-modularity

      -- ** Redundant type classes
      -- $monadio

      -- * Other (partial) solutions
      -- ** @hoist@
      -- $hoist

      -- ** @mmtl@
      -- $mmtl

      -- ** @MonadCatchIO@
      -- $monadcatchio

      -- ** @monad-control@
      -- $monad-control

      -- * @layers@' solution

      -- ** Monad layers
      -- $layers

      -- ** Transformers as polymorphic layers
      -- $transformers

      -- ** Invariant functors
      -- $invariant

      -- ** Functorial layers
      -- $functorial

      -- ** Peelable layers
      -- $peelable

      -- ** Zero-aware layer state
      -- $zero

      -- ** Fully modular monad interfaces
      -- $modular

      -- ** Compatibility with @transformers@
      -- $compatibility

      -- ** Polymorphic, universal @lift@s
      -- $polymorphic

      -- * References
      -- $references
    )
where


{-$monads

Monads are used everywhere in Haskell. Monads in general represent
computations, and specific monads represent computations capable of specific
features. For example, there is a monad that encapsulates stateful
computations, a monad that encapsulates computations which may fail (and
recover from failure) and a monad that encapsulates computations which perform
I/O. When writing applications in Haskell, we often wish to use a combination
of these features all at once. The most common way to do this is to use monad
transformers. Monad transformers are monad \"constructors\" that take an
existing monad and return a new monad that has all the features of the
original monad in addition to any features that were added by the monad
transformer. Haskell applications often use a custom monad which consists of a
stack of (standard) monad transformers layered on top of some base monad. The
standard monad transformers are provided by the @transformers@ package, which
at the time of writing has 676 reverse dependencies on Hackage.

For the \"monad transformer stack\" design pattern to be usable, quite a lot
of type class machinery is required. As it stands, some of this machinery
lives in the @transformers@ package ('Control.Monad.Trans.Class.MonadTrans',
'Control.Monad.IO.Class.MonadIO'), while the rest is provided by the @mtl@
(monad transformer library) package (@MonadReader@, @MonadState@, etc.: the
documentation of this package refers to these sorts of type classes as \"monad
interfaces\"). However, the type class machinery provided by these packages is
limited in many important respects and it doesn't allow us to do many of the
things we might expect to be able to do.

-}

{-$lift

One of the most glaring weaknesses of the existing type class machinery is
that the 'Control.Monad.Trans.lift' operation provided by
'Control.Monad.Trans.MonadTrans' is not very powerful. It's only capable of
lifting very simple types of operations up from the inner monad. Consider the
'Control.Monad.Interface.Reader.local' operation from the
'Control.Monad.Interface.Reader.MonadReader' interface:

> local :: MonadReader r m => (r -> r) -> m a -> m a

It should be possible to automatically lift @local@ through every conceivable
monad transformer, even 'Control.Monad.Trans.Cont.ContT'. However, with @lift@
alone, it is impossible. The @MonadTrans@ class clearly needs at least one
more operation in addition to @lift@ to allow this. And while it should be
possible to automatically lift @local@ through /every/ possible monad
transformer, there are some operations which can be lifted through /most/
monad transformers, but not all. These sorts of operations are often called
\"control\" operations. One example is
'Control.Monad.Interface.Exception.catch' from the
'Control.Monad.Interface.Exception.MonadException' interface:

> catch :: (Exception e, MonadException m) => m a -> (e -> m a) -> m a

@catch@ can be lifted through most monad transformers, but not @ContT@. Given
that there is a class of monad transformers through which control operations
can be lifted, one would expect there to be a type class (a subclass of
@MonadTrans@) for such monad transformers, but none is provided by the
@transformers@ library. There are other packages which define such classes,
some of which are discussed below, but none of them are standard.

-}

{-$non-modularity

Imagine you are designing a monad transformer @Foo@. A common design pattern
would be to define a type constructor @FooT :: (* -> *) -> * -> *@ in the
module @Control.Monad.Trans.Foo@, and a function @runFooT :: FooT m a -> m a@.
Let's say that your monad transformer adds a single operation, @getFoo@, to
the transformed monad:

> getFoo :: Monad m => FooT m Foo

@getFoo@, defined in this way, will only work when @FooT@ is at the top of the
transformer stack, but we would like it would work on any monad built from a
transformer stack containing @FooT@ anywhere in the stack. The common design
pattern achieves this by defining in the @Control.Monad.Foo.Class@ module a
@MonadFoo@ class like the following:

> class Monad m => MonadFoo m where
>     getFoo :: m Foo
>
> instance Monad m => MonadFoo (FooT m) where
>     getFoo = FooT $ return Foo

So now we have a class that represents every monad that supports the @getFoo@
operation, and we have an instance for @FooT@. But where are the instances for
other monads which support @getFoo@ (i.e., monads built on top of @FooT@)? You
might expect there to be an instance like:

> instance (MonadTrans t, Monad (t m), MonadFoo m) => MonadFoo (t m) where
>     getFoo = lift getFoo

But this is not what is generally done. There are two reasons for this: one is
that @lift@ is not powerful enough to lift all types of operations, so for
many interfaces it isn't possible to define a universal pass-through instance
that can lift all the required operations through an arbitrary monad
transformer (although our @MonadFoo@ interface happens to only contain
@getFoo@ which is simple enough for @lift@). The second reason is that such
instances require the @OverlappingInstances@ extension, which is considered
far too controversial to be made a requirement for such a core part of the
Haskell ecosystem (the monad transfomer library).

So instead what monad transformer authors generally do is create pass-through
instances for each of the monad transformers in the @transformers@ library:

> instance MonadFoo m => MonadFoo (ReaderT r m) where
>     getFoo = lift getFoo

> instance MonadFoo m => MonadFoo (StateT s m) where
>     getFoo = lift getFoo

...and so on. This is a lot of code duplication. If you have @n@ monad
transformers and @m@ monad interfaces, you need to define @O(n * m)@ instances
to ensure that each monad interface can pass through each transformer. This
means @n@ identical instances for each monad interface (or at least they would
be identical if there was a sufficiently powerful complement to @lift@). But
the code duplication isn't the worst part: the worst part is how this destroys
/modularity/. It means that every third-party monad interface can only be
lifted through the standard monad transformers. If you want to lift a third
party monad interface through a third party monad transformer, your only hope
is to define an orphan instance, which means your application will break if
any of the libraries you depend on ever define this instance themselves.

-}

{-$monadio

The @transformers@ package includes the 'Control.Monad.IO.Class.MonadIO'
interface. It provides a single operation, 'Control.Monad.IO.Class.liftIO'.
This is unsatisfactory in a lot of ways. Firstly, @liftIO@ suffers from the
same lack of power as 'Control.Monad.Trans.Class.lift': only very simple types
of 'IO' operations can be lifted with @liftIO@. Secondly: while obviously it's
particularly useful to be able to lift operations from 'IO' through an
arbitrary stack of monad transformers, and it makes sense to have a type class
for the class of monads capable of performing I/O (including @IO@ itself),
there's no theoretical reason why @IO@ should be a special case like this. It
should be equally valid to have a @MonadST@ class that represents all monad
stacks that have 'Control.Monad.ST.ST' as their base monad, but obviously it
would be ridiculous to have to define a new type class for every possible base 
monad. Instead all that's needed is a class like the following:

> class MonadBase b m | m -> b where
>     liftBase :: b a -> m a

Indeed the @transformers-base@ package does define such a class. However, that
means there are two incompatible ways of expressing the same constraint:
@MonadIO m@ and @MonadBase IO m@. It would be better to do away with @MonadIO@
completely, or at least redefine it as a synonym for @MonadBase IO@.

However, even this is unsatisfactory, at least on its own. If
'Control.Monad.Trans.Class.lift' is for lifting operations from the monad just
beneath the top of the stack, and @liftBase@ is for lifting operations from
the monad at the very bottom of the stack, what operation do we have for
lifting operations from every monad in between? What we really want is a fully
polymorphic @lift@ that can lift operations from any monad anywhere in the
stack. Such an operation would unify @lift@, @liftIO@, @liftBase@ and friends
into a single operation that could do all of these things and more.

-}

{-$hoist

The @pipes@ package contains a module @Control.MFunctor@ which exports the
following class:

> class MFunctor t where
>     hoist :: Monad m => (forall b. m b -> n b) -> t m a -> t n a

The documentation describes @MFunctor@ as \"a functor in the category of
monads\". Its @hoist@ operation lifts a homomorphism between monads to a 
homomorphism between inner monads wrapped by the same transformer. The
combination of @hoist@ and @lift@ is powerful enough to define a universal
pass-through instance for the 'Control.Monad.Interface.Reader.MonadReader'
interface discussed above:

> instance (MonadTrans t, MFunctor t, Monad (t m), MonadReader r m) =>
>     MonadReader r (t m)
>   where
>     ask = lift ask
>     local f = hoist (local f)

The crucial question then is: is @hoist@ the missing operation from
'Control.Monad.Trans.Class.MonadTrans'? Can every type that is currently an
instance of @MonadTrans@ be made an instance of @MFunctor@ too? The answer,
unfortunately, is no. Once again, 'Control.Monad.Trans.Cont.ContT' proves too
stubborn, and won't permit an instance of @MFunctor@. However, the idea behind
@MFunctor@\/@hoist@ is \"close\", and it certainly seems to be on the right
track.

-}

{-$mmtl

There is an alternative to @mtl@ on Hackage called @mmtl@: the /modular/ monad
transformer library. It's an old package, and it no longer builds with recent
versions of GHC, but it's worth studying. Its version of the @MonadTrans@
class contains an extra operation @tmap@:

> class MonadTrans t where
>     lift :: Monad m => m a -> t m a
>     tmap :: (Monad m, Monad n)
>         => (forall b. m b -> n b)
>         -> (forall b. n b -> m b)
>         -> t m a
>         -> t n a

This @tmap@ operation is very similar to the @hoist@ operation in the @pipes@
package. It lifts an /isomorphism/ between monads to a homomorphism between
inner monads wrapped by the same transformer. (In addition to the morphism
between monads that @hoist@ takes, @tmap@ also takes that morphism's inverse.
In other words, @tmap@ is less powerful than @hoist@, because it can only lift
morphisms which have an inverse, while @hoist@ can lift any moprhism.) This
type of operation is sometimes called an /invariant/ functor: if @hoist@ is
\"a functor in the category of monads\", then @tmap@ is \"an invariant functor
in the category of monads\". Invariant functors from the category of @Hask@
can be represented by the following type class:

> class Invariant f where
>     invmap :: (a -> b) -> (b -> a) -> f a -> f b

This class is defined in the @invariant@ package. What's interesting is that
the documentation of @Invariant@ states that it's possible to define an
instance of @Invariant@ (i.e., an invariant functor from the category of
@Hask@) for every @* -> *@ type parametric in the argument. If we translate
this to the category of monads, that means that it's possible to define an
invariant functor (i.e., @tmap@) for every @(* -> *) -> * -> *@ type
parametric in the argument: this is exactly what monad transformers are. So
unlike @hoist@, it's possible to define @tmap@ for every possible monad
transfomer, even 'Control.Monad.Trans.Cont.ContT'. The crucial question for us
now is: is @tmap@ powerful enough to lift
'Control.Monad.Interface.Reader.local'? Can we define a universal pass-through
instance of 'Control.Monad.Interface.Reader.MonadReader'? The answer is yes!

> instance (MonadTrans t, Monad (t m), MonadReader r m) =>
>     MonadReader r (t m)
>   where
>     ask = lift ask
>     local f m = lift ask >>= \r -> tmap (local f) (local (const r)) m

As with @hoist@ above, we pass @local f@ to @tmap@ to lift the morphism into
the transformed monad. The tricky part though is that @tmap@ also requires us
to give it the inverse of @local f@. How can we get the inverse of @local f@?
It could be @local g@ if we knew a function @g@ which was the inverse of @f@
(such that @g . f = id@), but we don't. However, given that we know that
@local f@ just applies a transfomation @f@ to the environment @r@ of a reader
monad, we can cheat by using @const r@ (where @r@ is value in the environment
prior to running @local@, which we obtain by @ask@ing the inner monad using
@lift ask@) as the function we pass to @local@. This leaves us with
@tmap (local f) (local (const r))@, which does the job. (Although @const r@ is
not the inverse of @f@, we know that @r@ is the value applied to @f@ by
@local@, which means that the invariant @local (const r) . local f = id@
holds true.)

So what is the problem with @mmtl@ then? Well, as stated above, it has
bitrotted to the point where it no longer builds with recent versions of GHC.
But it also defines its own versions of all the monad transformers from
@transformers@. @transformers@ has 676 reverse dependencies on Hackage, and
the types that are defined in that package are used /everywhere/ in the
Haskell ecosystem. No matter how much of an improvement a package is on
@transformers@, it will never take off unless it remains compatible with it.
The proof of this is that @mmtl@ has only three reverse dependencies on
Hackage.

Also, remember we mentioned above that while @local@ should be able to be
automatically lifted through every monad transformer, there should also be a
subclass of monad transformers which can lift control operations like
'Control.Monad.Interface.Exception.catch'? @mmtl@ does not do anything to
provide this class (although we are free to build it on top of @mmtl@
ourselves).

-}

{-$monadcatchio

One of the first packages that attempted to deal with the \"catch\" problem to
gain widespread adoption on Hackage was @MonadCatchIO-{mtl,transformers}@. It
defines a type class @MonadCatchIO@ specifically for the purpose of lifting
the @catch@ operation through monad transformer stacks:

> class MonadIO m => MonadCatchIO m where
>     catch :: Exception e => m a -> (e -> m a) -> m a
>     block :: m a -> m a
>     unblock :: m a -> m a

(It also includes the (deprecated) operations @block@ and @unblock@ for
dealing with asynchronous exceptions, but we'll ignore asynchronous exceptions
for the moment and focus on @catch@.) While @MonadCatchIO@ succeeds at being
the class of monads into which @catch@ can be lifted, that is all it is. If we
want to lift from @IO@ (or indeed any monad) any other control operation, we
need to define a separate type class just for lifting that operation. That is
unacceptable. What we're really looking for is the class of monad transformers
/through/ which control operations can be lifted, but @MonadCatchIO@ doesn't
get us any closer to that.

Another problem with @MonadCatchIO@ is with the @bracket@ operation it
provides, defined in terms of @catch@. Consider the following example:

> foo = runMaybeT $ bracket
>     (lift (putStrLn "init"))
>     (\() -> lift (putStrLn "close"))
>     (\() -> lift (putStrLn "running") >> mzero)

Running @foo@ in GHCi, you might expect it to produce the following output:

>>> foo
init
running
close
Nothing

However, instead the following is produced:

>>> foo
init
running
Nothing

The finaliser was never run. What this shows is that the naive implementation
of the @bracket@ family of operations is deficient when lifted through a monad
transformer stack containing one or more short-circuiting monad transformers.
(The short-circuiting monad transformers provided by the @transformers@
package are @ErrorT@, @ListT@ and @MaybeT@). This hereafter is referred to as
\"the bracket problem\".

-}

{-$monad-control

There is another family of packages in this solution space; @monad-peel@ and
@monad-control@. (@monad-control@ started as a faster replacement for
@monad-peel@, but its design has diverged from the original @monad-peel@
design over time.) They both provide a subclass of
'Control.Monad.Trans.Class.MonadTrans' called @MonadTransControl@
(@monad-peel@ calls it @MonadTransPeel@), and this /is/ the type class that
we've been looking for! It represents the subclass of monad transformers
through which control operations can be automatically lifted. The latest
version of @monad-control@ implements @MonadTransControl@ as follows:

> class MonadTrans t => MonadTransControl t where
>     data StT t :: * -> *
>     restoreT :: Monad m => m (StT t a) -> t m a
>     liftWith :: Monad m
>         => ((forall n b. Monad n => t n b -> n (StT t b)) -> m a)
>         -> t m a

Where to begin? The first thing to note is the associated data type @StT@.
For a given monad transformer @t@, @StT t@ represents the \"state\" added that
@t@ adds to the monad it wraps. For example, @StT MaybeT a =~ Maybe a@ and
@StT (ErrorT e) a =~ Either e a@. A good rule of thumb for figuring out what
@StT t@ is for a given transformer @t@ is to look at the definition of @t@:

> data {t} m a = {t} { run{t} :: m (Foo a) }

If @t@ is defined as above, then @StT t a =~ Foo a@. In general, @StT t@ will
be the type of everything \"inside\" the @m@ in the definition of the
transformer. The trick is to extract the component of \"monadic state\" added
by @t@ which is independent of the underlying monad @m@.

The next thing to note is @restoreT@, which is pretty simple. It just takes
the \"monadic state of @t@\" (@StT t a@) wrapped in @m@, and \"restores\" it
to a value of @t m a@. For most transformers, @restoreT@ is trivial: e.g., for
@MaybeT@, @restoreT =~ MaybeT@, modulo the unwrapping of the @StT@ value.

It's the @liftWith@ operation where the real \"magic\" of the @monad-control@
package happens. It's a CPS-style operation which takes a function which takes
a function, to which @liftWith@ passes an operation (usually called @run@) of
type @forall n b. Monad n => t n b -> n (StT t b)@. @run@ is basically the
inverse of @lift@: if @lift@ wraps a transformer @t@ around a monadic action,
@run@ peels a transformer off a monadic action, storing its monadic state in
an @StT t@ value inside @m@. The type variable @n@ in the signature of @run@
is usually instantiated to @m@, but it's type is polymorphic over all monads
to statically ensure that there are no remaining side effects in @m@ in the
resulting action. Using something like @MonadTransControl@, it's possible to
define a universal pass-through instance for the
'Control.Monad.Interface.Exception.MonadException' interface as follows:

> instance (MonadTransControl t, Monad (t m), MonadException m) =>
>     MonadException (t m)
>   where
>     throw = lift . throw
>     catch m h = do
>         st <- liftWith (\run -> catch (run m) (run . h))
>         restoreT (return st)

The @monad-control@ package also includes a class @MonadBaseControl@, which is
a subclass of the @MonadBase@ class from @transformers-base@ (described
above):

> class MonadBase i m => MonadBaseControl i m | m -> i where
>     data StM m :: * -> *
>     restoreM :: StM m a -> m a
>     liftBaseWith :: ((forall b. m b -> i (StM m b)) -> i a) -> m a

(@MonadBaseControl@ is to @MonadBase@ as @MonadTransControl@ is to
@MonadTrans@. While @lift@ lifts operations from the monad just beneath the
top of the transformer stack, @liftBase@ lifts operations from the monad at
the very bottom of the stack. @lift@ and @liftBase@ can only lift very basic
types of operations, @liftWith@ and @liftBaseWith@ can lift much more
complicated types of operations. @MonadBaseControl@ uses the same technique to
achieve this with @liftBaseWith@ as @MonadTransControl@ uses with @liftWith@.)

@MonadBaseControl@ is used extensively in the @lifted-base@ package, whose
author is also the author of @monad-control@. It re-exports many of the 'IO'
operations in the @base@ package, but lifted to work with any monad built from
a transformer stack with @IO@ as its base monad. This includes a version of
@bracket@, defined as follows:

> bracket :: MonadBaseControl IO m => m a -> (a -> m b) -> (a -> m c) -> m c
> bracket before after thing = do
>     st <- liftBaseWith $ \run -> E.bracket
>         (run before)
>         (\st -> run $ restoreM st >>= after)
>         (\st -> run $ restoreM st >>= thing)
>     restoreM st

Let's see if this version of @bracket@ can handle the bracket problem
correctly:

> foo = runMaybeT $ bracket
>     (lift (putStrLn "init"))
>     (\() -> lift (putStrLn "close"))
>     (\() -> lift (putStrLn "running") >> mzero)

>>> foo
init
running
close
Nothing

Success! But this success comes at a cost. Consider a more complicated
example:

> bar :: IO (Maybe (), Int)
> bar = flip runStateT 0 $ runMaybeT $ bracket
>     (do
>         liftBase $ putStrLn "init"
>         modify (+1))
>     (\() -> do
>         modify (+16)
>         liftBase $ putStrLn "close"
>         modify (+32)
>         mzero
>         modify (+64))
>     (\() -> do
>         modify (+2)
>         liftBase $ putStrLn "body"
>         modify (+4)
>         mzero
>         modify (+8))

(Adding a different power of two with each @modify@ operation is equivalent to
setting a different bit of the state. By looking at the final value of the
state we can see exactly which @modify@ operations were run.) We would expect
the following to output:

>>> bar
init
body
close
(Nothing,55)

Instead, however, we get:

>>> bar
init
body
close
(Nothing,7)

While @monad-control@'s @bracket@ operation manages to run the finaliser even
in the presence of a short-circuiting monad transformer, the reason it is able
to do this so is because it liberally discards the monadic state of the outer
transformers. The finaliser is run with the monadic state that existed after
the /initialiser/ finished, not the monadic state after the \"body\" (which is
what one would expect). Additionally, the monadic state after the finaliser is
discarded, so the final monadic state after @bracket@ completes is the monadic
state that existed after the \"body\" exited, not after the finaliser exited.
This makes sense if you look at the definition of @bracket@ above: @bracket@
discards any potentially \"zero\" (i.e., short-circuiting) monadic state that
could prevent the finaliser from being run. While this solution \"works\", the
above example shows that it can lead to incorrect results as shown above. We
do not consider that @monad-control@ solves the bracket problem
satisfactorily.

-}

{-$layers

The most fundamental type class in @layers@, and its replacement for
'Control.Monad.Trans.Class.MonadTrans', is 'Control.Monad.Layer.MonadLayer'.
Unlike @MonadTrans@, which is instantiated with types of kind
@(* -> *) -> * -> *@ (i.e., monad constructors), @MonadLayer@ is instantiated
on normal monads (of kind @* -> *@). Simply put, a monad @m@ can be an
instance of @MonadLayer@ if it is built (\"layered\") on top of some inner
monad @'Control.Monad.Layer.Inner' m@. (@Inner m@ is an associated type that
must be provided by instances @MonadLayer@.) It looks like the following:

> class (Monad m, Monad (Inner m)) => MonadLayer m where
>     type Inner m :: * -> *
>     layer :: Inner m a -> m a

('Control.Monad.Layer.layer' is the @MonadLayer@ equivalent to
'Control.Monad.Trans.Class.lift'.)

Note that unlike monad transformers, monad layers are not necessarily
parametric in their inner monad. This means that the class of layers, in
addition to including all valid monad transformers, also includes monads which
are implemented on top of some fixed inner monad. For example, it's common to
define an @Application@ monad in terms of some transformer stack based on
'IO', and then use 'Control.Monad.IO.Class.liftIO' to lift @IO@ computations
into the @Application@ monad. @Application@ could look like the following:

> newtype Application a = Application (StateT AppState IO a)
>   deriving (Functor, Applicative, Monad, MonadState)

While it clearly isn't a monad transformer (it doesn't even have the right
kind), it's clearly in some sense a \"layer\" around @IO@, and we would like
to be able to lift computations from @IO@ to the @Application@ monad.
@MonadLayer@ allows us to express this (without resorting to hacks like
'Control.Monad.IO.Class.MonadIO'):

> instance MonadLayer Application where
>     type Inner Application = IO
>     layer = Application . layer

(This definition makes use of the @MonadLayer@ instance for
'Control.Monad.Trans.State.StateT' provided by the @layers@ package.)

-}

{-$transformers

Every monad transformer can be made an instance of
'Control.Monad.Layer.MonadLayer', but not all monad layers are monad
transformers. For this reason it might be useful to have a subclass of
@MonadLayer@ that represents all the monad layers which are monad
transformers. (A monad layer is a monad transformer if it is (parametrically)
polymorphic in its inner monad.) @layers@ provides such a type class:

> class (MonadLayer m, m ~ Outer m (Inner m)) => MonadTrans m where
>     type Outer m :: (* -> *) -> * -> *

This 'Control.Monad.Layer.MonadTrans' class is a bit different to the one from
@transformers@. Like @MonadLayer@ (of which it is a subclass), it is
instantiated on monads (@* -> *@) rather than monad constructors
(@(* -> *) -> * -> *@). It has an associated type
@'Control.Monad.Layer.Outer' m@ which points to the monad constructor from
which @m@ is composed. The superclass equality constraint on the @MonadTrans@
class @m ~ Outer m (Inner m)@ ensures that @m@ really is a composition of the
monad constructor @Outer m@ and the inner monad @Inner m@, making this class
pretty much equivalent to the @MonadTrans@ class from @transformers@.

If GHC supported @QuantifiedConstraints@ and @ImplicationConstraints@ (which
it doesn't due to GHC bugs \#2893 and \#5927 respectively (though it
seems possible that these could be fixed some day)), it would be tempting to
reformulate @MonadTrans@ to be instantiated directly on types @t@ of kind
@(* -> *) -> * -> *@, just like the @MonadTrans@ class from @transformers@.
To ensure that such a @MonadTrans@ class would still be a \"subclass\" of
@MonadLayer@, it would need to have the constraint
@(forall m. Monad m => (MonadLayer (t m), Inner (t m) ~ m))@ in its superclass
context. However, this would exclude some instances which are currently
permitted. For example, if it was done right (and if there existed a
@CommutativeMonad@ type class), the monad instance of
'Control.Monad.Trans.List.ListT' would look something like
@instance CommutativeMonad m => Monad (ListT m)@. This in turn would mean that
@ListT@'s @MonadLayer@ instance would have the constraint
@/CommutativeMonad/ m@ in its context, meaning that the constraint
@forall m. /Monad/ m => (MonadLayer (ListT m), Inner (ListT m) ~ m)@ would not
be satisfied, preventing @ListT@ from being made an instance of @MonadTrans@.
This in turn could be solved by adding an associated type of kind
@(* -> *) -> Constraint@ (which would default to @Monad@) to the @MonadTrans@
class and modifying the superclass constraint accordingly, but it would be
kind of messy. This is how it would look:

> class (forall m. MonadConstraint t m => (MonadLayer (t m), Inner (t m) ~ m))) =>
>     MonadTrans t
>   where
>     type MonadConstraint t :: (* -> *) -> Constraint
>     type MonadConstraint t = Monad

-}

{-$invariant

The 'Control.Monad.Layer.MonadLayer' and 'Control.Monad.Layer.MonadTrans'
classes we showed in the last two sections were not quite complete: we omitted
one method from each of them for the sake of the discussion. Here are the
complete definitions of both of these classes:

> class (Monad m, Monad (Inner m)) => MonadLayer m where
>     type Inner m :: * -> *
>     layer :: Inner m a -> m a
>     layerInvmap
>         :: (forall b. Inner m b -> Inner m b)
>         -> (forall b. Inner m b -> Inner m b)
>         -> m a
>         -> m a

> class (MonadLayer m, m ~ Outer m (Inner m)) => MonadTrans m where
>     type Outer m :: (* -> *) -> * -> *
>     transInvmap :: (MonadTrans n, Outer n ~ Outer m)
>         => (forall b. Inner m b -> Inner n b)
>         -> (forall b. Inner n b -> Inner m b)
>         -> m a
>         -> n a

We've added the 'Control.Monad.Layer.layerInvmap' and
'Control.Monad.Layer.transInvmap' operations to @MonadLayer@ and @MonadTrans@
respectively. @transInvmap@ corresponds exactly to the @tmap@ function
described in the @mmtl@ section above. (Its name refers to the fact that monad
transformers (should) be invariant functors in the category of monads.
(Re-read the @mmtl@ section above if you've forgotten what this means.))
@layerInvmap@ is like a restricted version of @transInvmap@ where the inner
monad is fixed: this is because monad layers are not necessarily parametric in
their inner monad. It might then seem that @layerInvmap@ is useless, but it
can still be useful to apply a transformation to the underlying monad which
does not change its type. For example,
@'Control.Monad.Interface.Reader.local' f@ is such a transformation. This
means that @layerInvmap@ (unlike @layer@ on its own) is powerful enough to
define a universal pass-through instance for any
'Control.Monad.Interface.Reader.MonadReader' through any @MonadLayer@.

-}

{-$functorial

While 'Control.Monad.Layer.layerInvmap' and 'Control.Monad.Layer.transInvmap'
are useful, they can only lift from the inner monad morphisms which have an
inverse. Sometimes we might want to lift morphisms which do not have an
inverse. However, not all monad layers/monad transformers are capable of
lifting such morphisms through them, so we need new subclasses of
'Control.Monad.Layer.MonadLayer' and 'Control.Monad.Layer.MonadTrans' to
represent monad layers and monad transformers through which such morphisms
/can/ be lifted. We call these classes 'Control.Monad.Layer.MonadLayerFunctor'
and 'Control.Monad.Layer.MonadTransFunctor'. (Their names refer to the fact
they represent functors in the category of monads.) The provide the operations
'Control.Monad.Layer.layerMap' and 'Control.Monad.Layer.transMap'
respectively, the definitions of which are given below:

> class MonadLayer m => MonadLayerFunctor m where
>     layerMap :: (forall b. Inner m b -> Inner m b) -> m a -> m a

> class (MonadLayerFunctor m, MonadTrans m) => MonadTransFunctor m where
>     transMap :: (MonadTrans n, Outer n ~ Outer m)
>         => (forall b. Inner m b -> Inner n b)
>         -> m a
>         -> n a

-}

{-$peelable

Is it possible to lift control operations like
'Control.Monad.Interface.Exception.catch' through a monad layer? Yes, if that
monad layer provides an instance of 'Control.Monad.Layer.MonadLayerControl':

> class MonadLayerFunctor m => MonadLayerControl m where
>     data LayerState m :: * -> *
>     restore :: LayerState m a -> m a
>     layerControl
>         :: ((forall b. m b -> Inner m (LayerState m b)) -> Inner m a)
>         -> m a

The design of @MonadLayerControl@ is very similar to that of
@MonadTransControl@ from the @monad-control@ package (discussed above). @StT@
is renamed to 'Control.Monad.Layer.LayerState', @restoreT@ to
'Control.Monad.Layer.restore' and @liftWith@ to
'Control.Monad.Layer.layerControl'. One of the main differences with
@MonadLayerControl@ is that @LayerState@ is parameterised by
@m :: * -> *@ rather than @t :: (* -> *) -> * -> *@ (as @StT@ is). This is
simply because monad layers don't necessarily have a @t@. What @LayerState m@
is supposed to represent then is the portion of the monadic state of @m@ that
is independent of @'Control.Monad.Layer.Inner' m@.

The other big difference between @MonadLayerControl@ and @monad-control@'s
@MonadTransControl@ is with the @run@ operation that @layerControl@ passes to
its continuation. @MonadLayerControl@'s @run@ operation is of type
@forall b. m b -> Inner m (LayerState m b)@: unlike @MonadTransControl@'s,
ours is not polymorphic over all inner monads, so we cannot statically ensure
with the type system that there are no remaining side effects in @m@ in the
action returned by @run@. The reason we make this change is because layers are
not necessarily parametric in their inner monad, so we cannot express for
monad layers the polymorphism used by @monad-control@'s @MonadTransControl@.

However, we do include our own version of
'Control.Monad.Layer.MonadTransControl'. It has the operation
'Control.Monad.Layer.transControl', which does exactly the same thing as
@layerControl@, but the @run@ operation it passes to its continuation is more
polymorphic. We can use the 'Control.Monad.Layer.Outer' associated type of the
@MonadTrans@ class (from which our @MonadTransControl@ is (transitively)
descended) to express the polymorphism required to statically ensure that the
result of the @run@ operation provided by @transControl@ has no remaining side
effects in @'Outer' m@. We don't intend for @MonadTransControl@ to ever really
be used in practice though. Its main use is as a guide for those who write
monad transformers. If you are implementing a monad transformer @t@, you'll
almost certainly want to make @t@ an instance of both @MonadLayerControl@ and
@MonadTransControl@. If it turns out that you can write an instance of
@MonadLayerControl@ for @t@, but not an instance of @MonadTransControl@, then
you'll know that your instance for @MonadLayerControl@ was invalid because it
relied on residual side effects in @t@ in the result of its @run@ computation.
Without @MonadTransControl@ it is possible that such instances would be
distributed in libraries and propogated widely before anybody realised their
invalidity, because the type system cannot do so automatically with
@MonadLayerControl@ alone. For monad layers which are not monad transformers,
we still have to just trust that their instances of @MonadLayerControl@ do the
right thing, but in practice most monad layers are monad transformers, so this
is not really that bad. Here is the @MonadTransControl@ type class:

> class (MonadLayerControl m, MonadTrans m) => MonadTransControl m where
>     transControl
>         :: (forall n. (MonadTrans n, Outer n ~ Outer m)
>             => (forall b. n b -> Inner n (LayerState n b))
>             -> Inner n a)
>         -> m a

In case it isn't obvious why @MonadLayerControl@ is a subclass of
@MonadLayerFunctor@ rather than @MonadLayer@, it's because it's possible to
define @layerMap@ in terms of @layerControl@ and @restore@. Given this fact,
@MonadLayerControl@ not being a subclass of @MonadLayerFunctor@ makes about as
much sense as 'Monad' not being a subclass of 'Functor' (given that 'fmap' can
be defined in terms of '>>=' and 'return'). Here is @layerMap@ defined in
terms of @layerControl@ and @restore@:

> layerMap f m = layerControl (\run -> f (run m)) >>= restore

-}

{-$zero

If @layers@ just copies @monad-control@'s design for
'Control.Monad.Layer.MonadLayerControl' pretty much verbatim, and
@monad-control@'s solution to the bracket problem produces incorrect results,
then how can @layers@ hope to solve the bracket problem any better? The truth
is that once again, we omitted a method when we showed you @MonadLayerControl@
class for the sake of the discussion. The full @MonadLayerControl@ class is as
follows:

> class MonadLayerFunctor m => MonadLayerControl m where
>     data LayerState m :: * -> *
>     zero :: LayerState m a -> Bool
>     restore :: LayerState m a -> m a
>     layerControl
>         :: ((forall b. m b -> Inner m (LayerState m b)) -> Inner m a)
>         -> m a
>
>     zero _ = False

The only differnce is that we've added the operation
'Control.Monad.Layer.zero'. It's very simple: it just takes the @LayerState@
of @m@ and says whether or not it's \"zero\". For example, in the
@MonadLayerControl@ instance for 'Control.Monad.Trans.Maybe.MaybeT', @zero@
is pretty much just 'Data.Maybe.isNothing'. For
'Control.Monad.Trans.List.ListT', @zero@ is 'Data.List.null'. A good rule of
thumb for implementing @zero@ for a monad layer is to ask, for a given
@LayerState m a@, does this @LayerState m a@ contain a value of type @a@
somewhere that can be extracted from it? If the answer is no, then @zero@
should return @True@. For example, @MaybeT@'s @zero@ returns @True@ when its
@LayerState@ is 'Data.Maybe.Nothing', and @ListT@'s @zero@ returns @True@ when
@LayerState@ is @[]@. This allows us to solve the bracket problem without
discarding the monadic state of the outer layers, because we can directly
detect when a short-circuiting monad transformer has short-circuited
(\"zero'd\"). @layers@ defines its version of
'Control.Monad.Interface.Try.bracket' in terms of the
'Control.Monad.Interface.Try.MonadTry' interface, which is shown here:

> class MonadMask m => MonadTry m where
>     mtry :: m a -> m (Either (m a) a)
>     mtry = liftM Right

It provides a single operation 'Control.Monad.Interface.Try.mtry', which takes
a monadic action in @m@ and returns a new monadic value in @m@ which is
guaranteed not to short-circuit. If the action @m@ that was given to @mtry@
would have short-circuited, it returns @Left m@, otherwise it returns
@Right a@, where @a@ is the value returned by the computation @m@.

(The 'Control.Monad.Interface.Mask.MonadMask' you see in the superclass
constraint is for dealing with asynchronous exceptions. @MonadMask@ is
actually not used anywhere in the
'Control.Monad.Interface.Exception.MonadException' interface, only by
@MonadTry@, for defining @bracket@ and friends. This seems to suggest that
@mask@\/@bracket@ and @throw@\/@catch@ are actually orthogonal to one
another.)

Instances of @MonadTry@ are provided for all the base monads defined in the
@base@ and @transformers@ libraries, and the @zero@ operation of
@MonadLayerControl@ is used to define a universal pass-through instance for
any @MonadTry@ wrapped by a monad layer:

> instance (MonadLayerControl m, MonadTry (Inner m)) => MonadTry m where
>     mtry m = do
>         ma <- layerControl (\run -> mtry (run m))
>         case ma of
>             Left m' -> return . Left $ layer m' >>= restore
>             Right a -> if zero a
>                 then return . Left $ restore a
>                 else liftM Right $ restore a

Let's try out the @bracket@ function defined in "Control.Monad.Interface.Try".
Let's see if it produces the correct result where @monad-control@ could not.

> bar :: IO (Maybe (), Int)
> bar = flip runStateT 0 $ runMaybeT $ bracket
>     (do
>         lift $ putStrLn "init"
>         modify (+1))
>     (\() -> do
>         modify (+16)
>         lift $ putStrLn "close"
>         modify (+32)
>         mzero
>         modify (+64))
>     (\() -> do
>         modify (+2)
>         lift $ putStrLn "body"
>         modify (+4)
>         mzero
>         modify (+8))

>>> bar
init
body
close
(Nothing,55)

This is the correct result! Our @bracket@ correctly handles the
short-circuiting monad transformer without discarding the monadic state of the
other transformers. The bracket problem is solved.

-}

{-$modular

We said above that one of the problems with the type class machinery provided
by (and the design patterns suggested by) @transformers@ and @mtl@ is the lack
of modularity possible with them. If you have @n@ monad interfaces and @m@
monad layers, then you need @O(n * m)@ instances. You need to write an
instance for each combination of monad transformer and monad interface. In
particular, this makes it impossible to use a third-party monad interface with
a third-party monad transformer, unless the author of the monad transformer
knew of that monad interface and was okay with making their monad transformer
depend on the package which provides that monad interface (or vice-versa). We
would like for there to only need to be @O(n + m)@ instances, and for it to be
possible to use third-party monad interfaces with third-party monad
transformers that know nothing about each other. The achieve this, we need
what this documentation has so far referred to as \"universal pass-through
instances\". We've shown a few examples of these already, but here is another
one just to make it completely clear what we're talking about (this time for
'Control.Monad.Interface.Cont.MonadCont'):

> instance (MonadLayerControl m, MonadCont (Inner m)) => MonadCont m where
>     callCC f = layerControl (\run -> callCC $ \c -> run . f $ \a ->
>         layer (run (return a) >>= c)) >>= restore

As we said above, there are two reasons why this is not done currently: one is
that the 'Control.Monad.Trans.Class.lift' operation provided by @transformers@
is not powerful enough to define universal pass-through instances for control
operations. @layers@ solves this by with the
'Control.Monad.Layer.MonadLayerControl' interface, whose
'Control.Monad.Layer.layerControl' operation can lift control operations (such
as 'Control.Monad.Interface.Cont.callCC' above). The other reason why this is
not done is because these instances require the @OverlappingInstances@
extension. @layers@ doesn't solve this problem, but we just say \"fuck it\"
and use @OverlappingInstances@ anyway.

It seems that the main reason that @OverlappingInstances@ is considered
evil is that if your code uses relies on an overlapping instance, it's
possible for the meaning of your code to change /if/ you import a module which
defines a more specific instance which matches the types in your code /and/
the more specific instance behaves differently to the more general instance.
While this is definitely bad, we don't think that it will be a problem in
practice with @layers@, as long as people use common sense when writing
instances. In order to spell out exactly what we mean by \"common sense\", let
us first note that there are three types of instances that are needed when
writing monad transformers and/or monad interfaces the @layers@ way.

    1. Monad transformers need to be made instances of
    'Control.Monad.Layer.MonadLayer' and 'Control.Monad.Layer.MonadTrans', and
    if applicable, 'Control.Monad.Layer.MonadLayerFunctor',
    'Control.Monad.Layer.MonadTransFunctor',
    'Control.Monad.Layer.MonadLayerControl' and
    'Control.Monad.Layer.MonadTransControl'. Using
    'Control.Monad.Trans.State.StateT' as an example, these instances have the
    form @instance Monad m => MonadLayer (StateT s m) where type Inner (StateT
    s m) = m@.

    2. The functionality provided by a monad transformer is often abstracted
    into a monad interface. Again using the example of @StateT@ (and
    'Control.Monad.Interface.State.MonadState'), these instances have the
    form @instance Monad m => MonadState s (StateT s m)@.

    3. To achieve the level of modularity we wish for, monad interfaces need
    to have universal pass-through instances. This means that an monad which
    is an instance of a given monad interface is still an instance of that
    monad interface when wrapped by a monad layer. These instances have the
    form: @instance (MonadLayer m, MonadState s (Inner m)) => MonadState s m@.
    (These are also the only instances that require the @OverlappingInstances@
    extension.)

Now that we can easily refer to these different types of instances by number,
the \"common sense\" rules that consumers of the @layers@ library need to
follow to ensure sensible behaviour are as follows:

    1. Every instance of type 3, e.g., the universal pass-through instance for
    a given monad interface, must be defined in the same module as that monad
    interface. (The only thing worse than overlapping instances are
    overlapping /orphan/ instances.)

    2. A monad transformer @t@'s instances of type 2, i.e., of the monad
    interfaces which encapsulate its functionality, can be defined in either
    the same module as @t@'s type 1 instances or in the same module(s) as the
    monad interface(s), but if the latter, then that/those module(s) /must/
    also import the module in which @t@'s type 1 instances are defined.
    (Normally this is not an issue because @t@'s instances of type 1 are
    defined in the same module as @t@ (or in the same module as
    @MonadLayer@ in the special case of the monad transformers from the
    @transformers@ package), but if @t@'s type 1 instances are orphans then
    this is relevant.)

    3. You should generally not need to define instances for monad layers that
    do not fit into one of the three types above. One exception is if you are
    writing a monad transformer @t@, and you want instances of the monad
    interface @m@ to be able to pass through @t@, but you know that the
    universal pass-through instance of @m@ can only pass-through monad layers
    which can provide @MonadLayerControl@, but your @t@ can't, yet you know of
    a way to define a pass-through instance of @m@ for @t@. This type of
    instance is not a problem. The same rules which apply to type 2 instances
    (these rules are described rule 2 above) apply to these kinds of
    instances,

    4. The only other case where you might want to define an instance that
    doesn't fall into one of the three types described above is where you are
    writing a monad transformer @t@, and you want instances of the monad
    interface @m@ to be able to pass through @t@, and the universal
    pass-through instance of @m@ /is/ able to pass through @t@, but you have a
    much more efficient implementation of the pass-through of @m@ through @t@
    than the universal one. The same rules which apply to type 2 instances
    (described in rule 2 above) apply to these kinds of instances, with the
    additional stipulation that you /must/ make absolutely certain that such
    instances behaves exactly the same as the universal ones. This is to
    ensure that the behaviour of a program which uses the universal instance
    does not change if it switches to the optimised one. However, we strongly
    recommend against providing such instances unless you are sure that the
    inefficiency of the universal pass-through instance is causing a noticable
    degradation in performance.

Having said all of that, there are real problems caused by
@OverlappingInstances@ that makes @layers@ harder to use than it should be.
These are mainly the error messages produced by @GHC@ when it cannot find a
solution for constraint which has overlapping instances. For example,
attempting to compile the following program produces the following error
message:

> import Control.Monad.Interface.State
>
> stupidGet :: Monad m => m Int
> stupidGet = get

>>> runhaskell Main.hs
Main.hs:4:9:
    Overlapping instances for MonadState Int m
      arising from a use of `get'
    Matching instances:
      instance [overlap ok] (Control.Monad.Layer.MonadLayer m,
                             MonadState s (Control.Monad.Layer.Inner m)) =>
                            MonadState s m
        -- Defined in `Control.Monad.Interface.State'
      instance [overlap ok] (Monad m, Data.Monoid.Monoid w) =>
                    MonadState s (Control.Monad.Trans.RWS.Strict.RWST r w s m)
        -- Defined in `Control.Monad.Interface.State'
      instance [overlap ok] (Monad m, Data.Monoid.Monoid w) =>
                      MonadState s (Control.Monad.Trans.RWS.Lazy.RWST r w s m)
        -- Defined in `Control.Monad.Interface.State'
      instance [overlap ok] Monad m =>
                    MonadState s (Control.Monad.Trans.State.Strict.StateT s m)
        -- Defined in `Control.Monad.Interface.State'
      instance [overlap ok] Monad m =>
                      MonadState s (Control.Monad.Trans.State.Lazy.StateT s m)
        -- Defined in `Control.Monad.Interface.State'
    (The choice depends on the instantiation of `m'
     To pick the first instance above, use -XIncoherentInstances
     when compiling the other instance declarations)
    In the expression: get
    In an equation for `stupidGet': stupidGet = get

Annoyingly, GHC unhelpfully suggests that we enable the @IncoherentInstances@
extension. Ideally the error message that GHC would produce would simply be:

>>> runhaskell Main.hs
Main.hs:4:9:
    Could not deduce (MonadState Int m) arising from a use of `get'
    from the context (Monad m)
      bound by the type signature for myGet :: Monad m => m Int
      at Main.hs:3:10-25
    Possible fix: add an instance declaration for (MonadState Int m)
    In the expression: get
    In an equation for `stupidGet': stupidGet = get

We are unsure whether this could be considered a bug in GHC or whether it's
expected behaviour, but either way we are documenting it here so that users
who run into this problem can know what to do when they see it.

(Sometimes similar messages pop up but are caused by the monomorphism
restriction. If you think your types really do satisfy the constraints in
question, try either disabling the monomorphism restriction or adding type
signatures where appropriate.)

-}

{-$compatibility

One of the fatal flaws of earlier attempts to do what we have done with the
@layers@ library, such as the modular monad transformer library @mmtl@, is
that they defined their own versions of the monad transformers from
@transformers@. As we have said already in this document, the types defined in
the @transformers@ package are used /everywhere/ in the Haskell ecosystem, to
the extent that it's simply not possible that a library which seeks to improve
the type class machinery for working with compositions of those types will be
adopted unless it reuses those types themselves. @layers@ gets this right by
reusing the monad transformer and base monad types from the @transformers@
package and not defining any of its own.

However, almost as widespread in the Haskell ecosystem as the monad
transformers defined in the @transformers@ package are the monad interfaces
defined in the @mtl@ package. We unfortunately can't reuse these monad
interfaces in the @layers@ package, because we need monad interfaces to have
universal pass-through instances, and the @mtl@ monad interfaces do not
provide universal pass-through instances. Rule 1 above states that we are only
allowed to define a universal pass-through instance for a given monad
interface in the same module module as that monad interface, so we can't
define them ourselves, and we have no control over modules in @mtl@ package 
in which those interfaces are defined.

The solution the @layers@ package takes is to define its own versions of all
of the monad interfaces from the @mtl@ package (as well as some new ones too).
However, this isn't nearly as bad as defining our own versions of all of the
monad transformers from the @transformers@ package. The reason for that is
that a constraint @MonadState' s m@ (where @MonadState'@ is the @MonadState@
defined in the @mtl@ package) should be more or less equivalent to the
constraint @'Control.Monad.Interface.State.MonadState' s m@ in the sense that
the set of monads @m@ that satisfy the former should be more or less the same
as the set of monads @m@ which satisfy the latter. In particular, a monad @m@
built from a stack of monad transformers from the @transformers@ library which
satisifes some constraint composed of monad interfaces from the @mtl@ package,
is guaranteed to satisfy corresponding constraint composed of equivalent monad
interfaces from the @layers@ package.

-}

{-$polymorphic

Notice that we renamed what is called 'Control.Monad.Trans.Class.lift' in
@transformers@ to 'Control.Monad.Layer.layer'. This is because one of the
goals of @layers@ is to unify all operations which are called @lift@ (e.g.,
'Control.Monad.IO.Class.liftIO', @liftBase@ and @lift@ itself) into a single,
universal 'Control.Monad.Layer.lift'. The @lift@ operation from the
'Control.Monad.Trans.Class.MonadTrans' interface is a little different to
the other lifts however, and we felt we should change the name to reflect that.
A more accurate name would be @wrap@, or as we call it, @layer@. Take @liftIO@
as an example. What we mean when we use @liftIO@ is \"Listen, monad, I know
you know how to do 'IO'. I don't care if @IO@ is one monad beneath the top of
your stack or ten, I don't even care if you are @IO@ itself, I just know you
understand @IO@ and that you can figure out the rest yourself.\" @liftBase@ is
generally used similarly. @lift@ on the other hand means \"wrap this monadic
action in exactly one monad transformer\". A useful operation no doubt, but
it isn't really the same thing.

Anyway, if @liftBase@ means \"lift this operation from the base of the monad
stack through an arbitrary number of monad transformers\", and @liftIO@ is a
special case of @liftBase@ where the base monad is always @IO@, is it possible
to define a @lift@ that can lift operations from any monad anywhere in the
stack? If so, then as well as being more powerful than @liftBase@, it would
also be able to do what @layer@ does, because another way of understanding
@layer@ is as an operation which means \"lift this operation from the monad
just beneath the top of the stack\", and a @lift@ which can lift operations
from /any/ monad anywhere in the stack can surely lift operations from the
monad just beneath the top of the stack as well. @layers@ provides such a
@lift@ function, using type class 'Control.Monad.Layer.MonadLift':

> class (Monad i, Monad m) => MonadLift i m where
>     lift :: i a -> m a

> instance Monad m => MonadLift m m where
>     lift = id

> instance (MonadLayer m, MonadLift i (Inner m)) => MonadLift i m where
>     lift = layer . lift

(In other words, a monadic operation in the monad @i@ can be lifted into
the monad @m@ if @i = m@, or if @m@ is a layer and operations from @i@ can be
lifted into the inner monad of @m@.) Let's demonstrate why @layers@' @lift@ is
nice. Imagine you have the following code (using @transformers@' @lift@):

> main = flip evalStateT 0 $ do
>     modify (+1)
>     x <- get
>     lift $ print x

>>> runhaskell Main.hs
1

Perfect! But now let's say that for some reason you wanted to add a load of
redundant 'Control.Monad.Trans.Identity.IdentityT' transformers to your monad
stack.

> main = flip evalStateT 0 $ runIdentityT $ runIdentityT $ runIdentityT $ do
>     modify (+1)
>     x <- get
>     lift $ print x

>>> runhaskell Main.hs
Main.hs:6:58:
    Couldn't match type `IO'
                  with `Control.Monad.Trans.Identity.IdentityT
                          (Control.Monad.Trans.Identity.IdentityT
                             (Control.Monad.Trans.State.Lazy.StateT b0 m0))'
    Expected type: Control.Monad.Trans.Identity.IdentityT
                     (Control.Monad.Trans.Identity.IdentityT
                        (Control.Monad.Trans.State.Lazy.StateT b0 m0))
                     ()
      Actual type: IO ()
    In the second argument of `($)', namely
      `runIdentityT
       $ do { modify (+ 1);
              x <- get;
              lift $ print x }'
    In the second argument of `($)', namely
      `runIdentityT
       $ runIdentityT
         $ do { modify (+ 1);
                x <- get;
                lift $ print x }'
    In the second argument of `($)', namely
      `runIdentityT
       $ runIdentityT
         $ runIdentityT
           $ do { modify (+ 1);
                  x <- get;
                  lift $ print x }'

What an ugly error message! It is the result of the @lift@ from @transformers@
not being polymorphic enough. This means that in practice, adding an
@IdentityT@ transformer in the middle of an existing transformer stack breaks
code written for that stack. This seems wrong because adding (or removing)
@IdentityT@ from a transformer stack should be a no-op. To get the above code
working using @transformers@' @lift@, we would have to change @lift@ to
@lift . lift . lift . lift@, which is still brittle and prone to breakage if
we in any way modify the transformer stack again. @layers@' polymorphic lift
does not have this problem. If we change the @lift@ in the above code to be
the @lift@ from @layers@ rather than from @transformers@, it just works:

>>> runhaskell Main.hs
1

In addition to @lift@, which is not very powerful by itself, @layers@ also
defines 'Control.Monad.Layer.liftInvmap', 'Control.Monad.Layer.liftMap' and
'Control.Monad.Layer.liftControl', which are analogous to
'Control.Monad.Layer.layerInvmap', 'Control.Monad.Layer.layerMap' and
'Control.Monad.Layer.layerControl' respectively. All of these are trivial
except for @liftControl@, which doesn't work in quite the same way as its
@layerControl@ counterpart. It is defined as follows:

> class MonadLiftFunctor i m => MonadLiftControl i m where
>     liftControl :: ((forall b. m b -> i (m b)) -> i a) -> m a

> instance Monad m => MonadLiftControl m m where
>     liftControl f = f (liftM return)

> instance (MonadLayerControl m, MonadLiftControl i (Inner m)) =>
>     MonadLiftControl i m
>   where
>     liftControl f = layerControl $ \runLayer -> liftControl $ \run ->
>         f $ liftM (\m -> layer (lift m) >>= restore) . run . runLayer

Unlike 'Control.Monad.Layer.MonadLayerControl',
'Control.Monad.Layer.MonadLiftControl' doesn't use an associated data type
for the layer state. Part of the reason for this is that associated data types
and overlapping instances do not play well together (see GHC bug #4259).
However, we still copied @monad-control@ - just an earlier version!
@monad-control@ 0.2 did not use associated data types either, and if you look
at version 0.3 the @transformers-base@ which depended on it, you will find a
type class @MonadBaseControl@ which is pretty much exactly equivalent to
@MonadLiftControl@, except for the fact that @MonadLiftControl@ is not just
restricted to the base monad of a transformer stack.

-}

{-$references

[Control.Monad.Trans.Class]
    From the @transformers@ package, this module contains the canonical
    implementation of monad transformers in Haskell. Its
    'Control.Monad.Trans.Class.MonadTrans' type class unfortunately only
    provides the 'Control.Monad.Trans.Class.lift' operation, which can only
    lift very simple types of operations through a monad transformer. In
    particular it can't lift what are called \"control\" operations. See:
    <http://hackage.haskell.org/packages/archive/transformers/0.3.0.0/doc/html/Control-Monad-Trans-Class.html>.

[Control.Monad.IO.Class]
    From the @transformers@ package, this module defines the
    'Control.Monad.IO.Class.MonadIO' interface with its
    'Control.Monad.IO.Class.liftIO' operation. @layers@ provides a much more
    powerful universal 'Control.Monad.Layer.lift' operation which replaces
    @liftIO@ and other similar operations.
    See: <http://hackage.haskell.org/packages/archive/transformers/0.3.0.0/doc/html/Control-Monad-IO-Class.html>.

[Control.Monad.Base]
    From the @transformers-base@ package, this module defines the @MonadBase@
    interface with its @liftBase@ operation, which is a slight generalisation
    of the @MonadIO@ interface provided by @transformers@. The
    'Control.Monad.Layer.lift' operation provided by @layers@ is even more
    general than @liftBase@, but @liftBase@ is still an improvement on
    @liftIO@.
    See: <http://hackage.haskell.org/packages/archive/transformers-base/0.4.1/doc/html/Control-Monad-Base.html>

[Control.MFunctor]
    The 'Control.Monad.Layer.layerMap', 'Control.Monad.Layer.transMap'
    and 'Control.Monad.Layer.liftMap' operations (from the
    'Control.Monad.Layer.MonadLayerFunctor',
    'Control.Monad.Layer.MonadTransFunctor' and
    'Control.Monad.Layer.MonadLiftFunctor' classes respectively) are inspired
    by the @hoist@ operation from the @MFunctor@ class in the @pipes@ package.
    See: <http://hackage.haskell.org/packages/archive/pipes/3.1.0/doc/html/Control-MFunctor.html>.

[Control.Monad.Trans]
    The 'Control.Monad.Layer.layerInvmap', 'Control.Monad.Layer.transInvmap'
    and 'Control.Monad.Layer.liftInvmap' operations (from the
    'Control.Monad.Layer.MonadLayer', 'Control.Monad.Layer.MonadTrans' and
    'Control.Monad.Layer.MonadLift' classes respectively) are inspired by the
    @tmap@ operation from the @MonadTrans@ class in the @mmtl@ package.
    See: <http://hackage.haskell.org/packages/archive/mmtl/0.1/doc/html/Control-Monad-Trans.html>.

[Data.Functor.Invariant]
    From the @invariant@ package, this module implements invariant functors
    from the category @Hask@. This is not used anywhere in the @layers@
    package, but the \"@invmap@\" name of the member operation of the
    @Invariant@ type class is reused by @layers@ for describing functors from
    the category of monads.
    See: <http://hackage.haskell.org/packages/archive/invariant/0.1.0/doc/html/Data-Functor-Invariant.html>

[MonadCatchIO, finally and the error monad]
    Michael Snoyman posts an example to the Haskell Cafe mailing list of
    @MonadCatchIO@'s @bracket@ operation producing incorrect results in the
    presence of short-circuiting monad transformers.
    See: <http://www.haskell.org/pipermail/haskell-cafe/2010-October/084890.html>

[Control.Monad.Trans.Control]
    From the @monad-control@ package, this module provides the
    @MonadTransControl@ class, which represents the class of monad
    transformers through which control operations can be lifted. The design of
    the 'Control.Monad.Layer.MonadLayerControl' and
    'Control.Monad.Layer.MonadTransControl' classes from @layers@ are based on
    the design of the @MonadTransControl@ class from the most recent version
    of @monad-control@.
    See: <http://hackage.haskell.org/packages/archive/monad-control/0.3.2/doc/html/Control-Monad-Trans-Control.html>

[Issue with monad-control]
    Leon Smith posts an example to the Snap Framework mailing list of
    @lifted-base@'s @bracket@ operation (which is based on @monad-control@)
    producing incorrect results.
    See: <http://permalink.gmane.org/gmane.comp.lang.haskell.snapframework/1574>

[#2893 (Implement \"Quantified contexts\" proposal)]
    GHC lacks support for @QuantifiedConstraints@. With
    @QuantifiedConstraints@, it would be possible to use @forall@ in
    constraint expressions. This bug, along with #5927, is what is preventing
    us from refactoring 'Control.Monad.Layer.MonadTrans' to be instantiated
    directly on types of kind @(* -> *) -> * -> *@.
    See: <http://hackage.haskell.org/trac/ghc/ticket/2893>.

[#5927 (A type-level \"implies\" constraint on Constraints)]
    GHC lacks support for @ImplicationConstraints@. With
    @ImplicationConstraints@, @=>@ would be a type level operator with the
    kind @Constraint -> Constraint -> Constraint@. This bug, along with #2893,
    is what is preventing us from refactoring 'Control.Monad.Layer.MonadTrans'
    to be instantiated directly on types of kind @(* -> *) -> * -> *@.
    See: <http://hackage.haskell.org/trac/ghc/ticket/5927>.

[Control.Monad.Exception]
    This module from the @monad-abort-fd@ package contains the @MonadMask@
    interface on which @layers@' 'Control.Monad.Interface.Mask.MonadMask'
    inteface is based.
    See: <http://hackage.haskell.org/packages/archive/monad-abort-fd/0.3/doc/html/Control-Monad-Exception.html>

[Are there any good use cases for OverlappingInstances?]
    A Stack Overflow thread about the dangers of the @OverlappingInstances@
    extension, on which this package relies heavily.
    See: <http://stackoverflow.com/questions/12628700/are-there-any-good-use-cases-for-overlappinginstances>.

[Haskell: Overlapping instances]
    Another Stack Overflow thread about the @OverlappingInstances@ extension.
    What's interesting about this one is that the OP's motivating example is
    pretty much identical to the 'Control.Monad.Layer.MonadLift' type class
    that @layers@ defines.
    See: <http://stackoverflow.com/questions/1064232/haskell-overlapping-instances>

[Control.Monad.Base.Control]
    From version 0.3 of the @transformers-base@ package. The design of the
    'Control.Monad.Layer.MonadLiftControl' class is copied from the design of
    @MonadBaseControl@ class in this module.
    See: <http://hackage.haskell.org/packages/archive/transformers-base/0.3/doc/html/Control-Monad-Base-Control.html>.

[#4259 (Relax restrictions on type family instance overlap)]
    Part of the reason why we base the design of
    'Control.Monad.Layer.MonadLiftControl' on the old version of
    @MonadBaseControl@ from @transformers-base@ 0.3 rather than the current
    version in @monad-control@ is because associated type families (which are
    used in the new version of @MonadBaseControl@) don't play well with
    @OverlappingInstances@ (which we want to use with @MonadLiftControl@).
    There is a bug report on GHC's bug tracker that is (sort of) about this
    problem.
    See: <http://hackage.haskell.org/trac/ghc/ticket/4259>.
-}