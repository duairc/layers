#define newtypeC(constraint, context)\
    class context => constraint; instance context => constraint
#if __GLASGOW_HASKELL__ >= 702
#define newtypeCE(c, d, e) class (d, e) => c; instance (d, e) => c
#define CE(constraint, equalities) (constraint)
#else
#define newtypeCE(c, d, e) class (d) => c; instance (d, e) => c
#define CE(constraint, equalities) (constraint, equalities)
#endif
