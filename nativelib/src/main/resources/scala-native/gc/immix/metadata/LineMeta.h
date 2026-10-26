#ifndef IMMIX_LINEMETA_H
#define IMMIX_LINEMETA_H

#include <stdint.h>
#include <stdbool.h>

typedef struct {
    int8_t next;
    uint8_t size;
} FreeLineMeta;

typedef enum {
    line_empty = 0x0,
    line_marked = 0x1,
} LineFlag;

typedef uint8_t LineMeta;

static inline bool Line_IsMarked(LineMeta *lineMeta) {
    return *lineMeta == line_marked;
}
static inline void Line_Mark(LineMeta *lineMeta) { *lineMeta = line_marked; }
static inline void Line_Unmark(LineMeta *lineMeta) { *lineMeta = line_empty; }

// Marks the lines from first to last, both included. Objects of one or two
// lines take two stores, where a loop would compile to a memset call.
static inline void Line_MarkRange(LineMeta *first, LineMeta *last) {
    Line_Mark(first);
    Line_Mark(last);
    for (LineMeta *lineMeta = first + 1; lineMeta < last; lineMeta++) {
        Line_Mark(lineMeta);
    }
}

#endif // IMMIX_LINEMETA_H
