#ifdef OverlapPragma
#define __OVERLAPPABLE__ {-# OVERLAPPABLE #-}
#define __OVERLAPPING__ {-# OVERLAPPING #-}
#define __OVERLAPS__ {-# OVERLAPS #-}
#define __INCOHERENT__ {-# INCOHERENT #-}
#else
#define __OVERLAPPABLE__
#define __OVERLAPPING__
#define __OVERLAPS__
#define __INCOHERENT__
#endif
