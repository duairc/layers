{-# LANGUAGE CPP #-}
{-# LANGUAGE NoImplicitPrelude #-}

#include <macros.h>

{-|

The H(layers) package is not complicated, but it can be very difficult to
understand if you aren't familiar with the jargon used in the documentation
and in the naming. This module attempts to define precisely what is meant by
the different pieces of jargon which I use throughout H(layers). Some of this
jargon is appropriated from other places in the Haskell ecosystem, while some
of it (to the best of my knowledge) is original. Even in the cases where it is
appropriated, the intended meaning in the context of H(layers) may be subtly
different, and even I get confused sometimes about what I'm talking sometimes.
So hopefully, by defining precisely the terms I use mean, I'll be able think
more clearly and therefore communicate more clearly, and ultimately make
H(layers) easier to understand.

Each section of the documentation of this module explains a different piece of
jargon, and, if applicable, shows how the concept it refers to is implemented
in H(layers).

-}

module Documentation.Layers.Glossary
{-# WARNING "This module exports no types, functions, classes or instances. It exists solely for the Haddock documentation it produces. You should not ever need to import it." #-}
    ( --
      -- | #basemonad#

      -- * Base monad
      -- $basemonad

      -- | #computation#

      -- * Computation
      -- $computation

      -- | #computationalfeature#

      -- * Computational feature
      -- $computationalfeature

      -- | #controloperation#

      -- * Control operation
      -- $controloperation

      -- | #innermonad#

      -- * Inner monad
      -- $innermonad

      -- | #layerffects#

      -- * Layer effects
      -- $layereffectss

      -- | #layerresult#

      -- * Layer result
      -- $layerresult

      -- | #layerstate#

      -- * Layer state
      -- $layerstate

      -- | #morphism#

      -- * Morphism
      -- $morphism

      -- | #monadconstructor#

      -- * Monad constructor
      -- $monadconstructor

      -- | #monadinterface#

      -- * Monad interface
      -- $monadinterface

      -- | #monadlayer#

      -- * Monad layer
      -- $monadlayer

      -- | #monadtransformer#

      -- * Monad transformer
      -- $monadtransformer

      -- | #monadtransformerstack#

      -- * Monad transformer stack
      -- $monadtransformerstack

      -- | #monadictype#

      -- * Monadic type
      -- $monadictype

      -- | #opentypeclass#
      -- | #closedtypeclass#

      -- * Open and closed type classes
      -- $opentypeclass

      -- | #outerlayers#

      -- * Outer layers
      -- $outerlayers

      -- | #passthroughinstance#

      -- * Pass-through instance
      -- $passthroughinstance

      -- | #shortcircuit#

      -- * Short circuit
      -- $shortcircuit

      -- | #sideeffect#

      -- * Side-effect
      -- $sideeffect

      -- | #universalpassthroughinstance#

      -- * Universal pass-through instance
      -- $universalpassthroughinstance
    )
where

{-$basemonad

A monad is a /base monad/ if it is not built from any monad transformers.
Common base monads are 'System.IO.IO', 'Data.Functor.Identity.Identity' and
'Data.Maybe.Maybe'.

It also makes sense to talk about the base monad of a monad which is built
from a stack of monad transformers.

monad can be build on top of another monad and still be a base monad, just as
long as it's monomorphic 

The /base monad/ of a particular monad is the<#monadtransformerstack transformer stack> is the monad
at the bottom of the stack.

This concept is implemented using functional dependencies in the module "Control.Monad.Lift.Base".

-}

{-$computation



-}

{-$computationalfeature



-}

{-$controloperation



-}

{-$innermonad



-}

{-$layereffectss




-}

{-$layerresult

The part of the result type of the inner function of @t@ which is not
part of the (updated) 'LayerState'.

-}

{-$layerstate

The parameters needed by the inner function of @t@ to return a computation in
the monad @m@. We call these parameters \"state\", because a component of the
return value of the @m@-computation returned by @t@'s inner function is often
meant to update one or more of these parameters, like a
'Control.Monad.Trans.State.Strict.State' monad.

-}

{-$morphism



-}

{-$monadconstructor



-}

{-$monadinterface



-}

{-$monadlayer



-}

{-$monadtransformer



-}

{-$monadtransformerstack



-}

{-$monadictype



-}

{-$opentypeclass



-}

{-$outerlayers



-}

{-$passthroughinstance



-}

{-$shortcircuit



-}

{-$sideeffect



-}

{-$universalpassthroughinstance



-}
