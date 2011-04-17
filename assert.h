#ifdef DEBUG
#define ASSERTM(cond) assert (cond) (return ())
#else
#define ASSERTM(cond) (return ())
#endif