#ifndef GC_STATIC_OBJECTS_H
#define GC_STATIC_OBJECTS_H

#include "shared/GCTypes.h"

/* The bounds of the section that holds the objects that codegen emits outside
 * the heap and that have a lock word: class objects, the instances of
 * constant modules and the objects of string literals. Once such a lock word
 * is inflated, it is the only reference to its monitor, so the collector
 * scans the section as a root range. Multithreaded builds only, as others have
 * no lock words. */
word_t *StaticObjects_Start(void);
word_t *StaticObjects_End(void);

#endif // GC_STATIC_OBJECTS_H
