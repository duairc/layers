layers: Modular type class machinery for monad transformer stacks
=================================================================

The layers package provides the type class machinery needed to make monads built out of stacks of
onad transformers easy to use. It introduces the concept of monad layers, which are a generalisation
f monad transformers. The type class machinery provided by and the design patterns suggested by layers
llow for much more modularity than is possible with the existing type class machinery and design
atterns. With layers it is possible to use arbitrary monad interfaces (monad interfaces are what
e call the sort of type classes that you see in the mtl and similar packages) with arbtirary monad
ransformers (by monad transformers here, we are specifically to monad constructors, such as the
nes defined in transformers), without ever having to explicitly define how to lift specific interfaces
hrough specific transformers. 
