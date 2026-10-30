#if defined(SCALANATIVE_GC_IMMIX)

#ifdef _WIN32
// fopen is deprecated in WinCRT, disable warnings
#define _CRT_SECURE_NO_WARNINGS
#endif

#include "Histogram.h"
#include "shared/Log.h"
#include <inttypes.h>
#include <stdlib.h>
#include <string.h>

Histogram *Histogram_Create(const char *fileName, size_t top) {
    FILE *outFile = fopen(fileName, "w");
    if (outFile == NULL) {
        GC_LOG_ERROR("Cannot open the histogram file %s", fileName);
        return NULL;
    }
    Histogram *histogram = calloc(1, sizeof(Histogram));
    if (histogram == NULL) {
        GC_LOG_ERROR("Out of memory for the heap histogram");
        abort();
    }
    histogram->outFile = outFile;
    histogram->top = top;
    return histogram;
}

void Histogram_Grow(Histogram *histogram, int32_t id) {
    int32_t capacity = histogram->capacity == 0 ? 4096 : histogram->capacity;
    while (capacity <= id) {
        capacity *= 2;
    }
    HistogramEntry *entries =
        realloc(histogram->entries, (size_t)capacity * sizeof(HistogramEntry));
    HistogramEntry **sorted =
        realloc(histogram->sorted, (size_t)capacity * sizeof(HistogramEntry *));
    if (entries == NULL || sorted == NULL) {
        GC_LOG_ERROR("Out of memory for the heap histogram");
        abort();
    }
    memset(entries + histogram->capacity, 0,
           (size_t)(capacity - histogram->capacity) * sizeof(HistogramEntry));
    histogram->entries = entries;
    histogram->sorted = sorted;
    histogram->capacity = capacity;
}

// Class names are java.lang.String literals: their first field is the char
// array, and they span all of it (see Object_nameWString).
static void Histogram_writeName(FILE *outFile, Rtti *rtti) {
    CharArray *chars = rtti->rt.name->value;
    int32_t length = chars->header.length;
    for (int32_t i = 0; i < length; i++) {
        uint32_t c = chars->values[i];
        if (c >= 0xD800 && c < 0xDC00 && i + 1 < length &&
            chars->values[i + 1] >= 0xDC00 && chars->values[i + 1] < 0xE000) {
            c = 0x10000 + ((c - 0xD800) << 10) + (chars->values[++i] - 0xDC00);
        }
        if (c < 0x80) {
            fputc((int)c, outFile);
        } else if (c < 0x800) {
            fputc(0xC0 | (c >> 6), outFile);
            fputc(0x80 | (c & 0x3F), outFile);
        } else if (c < 0x10000) {
            fputc(0xE0 | (c >> 12), outFile);
            fputc(0x80 | ((c >> 6) & 0x3F), outFile);
            fputc(0x80 | (c & 0x3F), outFile);
        } else {
            fputc(0xF0 | (c >> 18), outFile);
            fputc(0x80 | ((c >> 12) & 0x3F), outFile);
            fputc(0x80 | ((c >> 6) & 0x3F), outFile);
            fputc(0x80 | (c & 0x3F), outFile);
        }
    }
}

static int Histogram_compareBytes(const void *a, const void *b) {
    const HistogramEntry *left = *(HistogramEntry *const *)a;
    const HistogramEntry *right = *(HistogramEntry *const *)b;
    if (left->bytes != right->bytes) {
        return left->bytes < right->bytes ? 1 : -1;
    }
    return left->rtti->rt.id - right->rtti->rt.id;
}

void Histogram_EndMarking(Histogram *histogram) {
    uint64_t collection = ++histogram->collections;
    uint64_t totalObjects = 0, totalBytes = 0;
    size_t used = 0;
    for (int32_t id = 0; id < histogram->capacity; id++) {
        HistogramEntry *entry = &histogram->entries[id];
        if (entry->objects > 0) {
            totalObjects += entry->objects;
            totalBytes += entry->bytes;
            histogram->sorted[used++] = entry;
        }
    }
    if (used > 0) {
        qsort(histogram->sorted, used, sizeof(HistogramEntry *),
              Histogram_compareBytes);
    }

    FILE *outFile = histogram->outFile;
    size_t top = used < histogram->top ? used : histogram->top;
    for (size_t i = 0; i < top; i++) {
        HistogramEntry *entry = histogram->sorted[i];
        int32_t id = entry->rtti->rt.id;
        bool isArray = __array_ids_min <= id && id <= __array_ids_max;
        fprintf(outFile, "%" PRIu64 "\t%" PRIu64 "\t%" PRIu64 "\t%d\t",
                collection, entry->bytes, entry->objects,
                isArray ? 0 : entry->rtti->size);
        Histogram_writeName(outFile, entry->rtti);
        fputc('\n', outFile);
    }
    fprintf(outFile, "%" PRIu64 "\t%" PRIu64 "\t%" PRIu64 "\t0\t*total*\n",
            collection, totalBytes, totalObjects);
    // Flushed at each collection: the file is never closed, as a thread can
    // still collect while the program exits
    fflush(outFile);

    for (size_t i = 0; i < used; i++) {
        histogram->sorted[i]->objects = 0;
        histogram->sorted[i]->bytes = 0;
    }
}

#endif
