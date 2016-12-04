#define B(n) <https://ghc.haskell.org/trac/ghc/ticket/n #n>
#define BW(x) <https://ghc.haskell.org/trac/ghc/wiki/x>
#define BWT(x, y) <https://ghc.haskell.org/trac/ghc/wiki/x y>
#define H(p) @<https://hackage.haskell.org/package/p p>@
#define HW(p) <https://www.haskell.org/haskellwiki/p p>
#define HWT(p, x) <https://www.haskell.org/haskellwiki/p x>
#define M(p, m) https://hackage.haskell.org/package/p/docs/m.html
#define T(p, m, ty) @<https://hackage.haskell.org/package/p/docs/m.html#t:ty ty>@
#define V(p, m, va) @<https://hackage.haskell.org/package/p/docs/m.html#v:va va>@
#define TT(p, m, ty, x) @<https://hackage.haskell.org/package/p/docs/m.html#t:ty x>@
#define VT(p, m, va, x) @<https://hackage.haskell.org/package/p/docs/m.html#v:va x>@
#define G(section, text) <Documentation-Layers-Glossary.html#section text>

#if LANGUAGE_OverlappingInstances
#define _OVERLAPPABLE
#define _OVERLAPPING
#else
#define _OVERLAPPABLE {-# OVERLAPPABLE #-}
#define _OVERLAPPING {-# OVERLAPPING #-}
#endif