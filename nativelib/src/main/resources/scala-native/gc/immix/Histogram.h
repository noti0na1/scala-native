#ifndef IMMIX_HISTOGRAM_H
#define IMMIX_HISTOGRAM_H

#include <stdint.h>
#include <stdio.h>
#include "immix_commix/headers/ObjectHeader.h"

// Live objects by class, counted while marking. Enabled by GC_HISTOGRAM_FILE:
// after the marking of each collection, the classes with the most live bytes
// are appended to that file as tab-separated lines
//
//   <collection> <bytes> <objects> <instance size> <class>
//
// The instance size is the class's unaligned size with its header, or 0 for
// arrays. The last line of each collection has the class `*total*`.
// GC_HISTOGRAM_TOP sets how many classes are written (default 40).
typedef struct {
    uint64_t objects;
    uint64_t bytes;
    Rtti *rtti;
} HistogramEntry;

typedef struct {
    FILE *outFile;
    size_t top;
    uint64_t collections;
    // Indexed by class id
    HistogramEntry *entries;
    // The entries in use, sorted when they are written
    HistogramEntry **sorted;
    int32_t capacity;
} Histogram;

Histogram *Histogram_Create(const char *fileName, size_t top);
void Histogram_Grow(Histogram *histogram, int32_t id);
// Writes the classes of the objects recorded since the last call
void Histogram_EndMarking(Histogram *histogram);

static inline void Histogram_Record(Histogram *histogram, Object *object) {
    Rtti *rtti = object->rtti;
    int32_t id = rtti->rt.id;
    if (id >= histogram->capacity) {
        Histogram_Grow(histogram, id);
    }
    HistogramEntry *entry = &histogram->entries[id];
    entry->objects += 1;
    entry->bytes += Object_Size(object);
    entry->rtti = rtti;
}

#endif // IMMIX_HISTOGRAM_H
