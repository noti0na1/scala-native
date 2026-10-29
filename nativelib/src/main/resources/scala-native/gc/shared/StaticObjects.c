#if defined(SCALANATIVE_MULTITHREADING_ENABLED) &&                             \
    (defined(SCALANATIVE_GC_IMMIX) || defined(SCALANATIVE_GC_COMMIX) ||        \
     defined(SCALANATIVE_GC_BOEHM))

#include <stddef.h>
#include "shared/StaticObjects.h"

/* Codegen names the same section (AbstractCodeGen). The linker gives its
 * bounds: on ELF for a section whose name is a C identifier, and on Mach-O for
 * any section. On COFF, the sections whose names differ only after a `$` are
 * merged in the order of those suffixes, so the objects, in `.snobj$b`, lie
 * between the two markers. */
#if defined(_WIN32)
__attribute__((section(".snobj$a"))) static word_t staticObjectsStart = 0;
__attribute__((section(".snobj$c"))) static word_t staticObjectsEnd = 0;

word_t *StaticObjects_Start(void) { return &staticObjectsStart + 1; }
word_t *StaticObjects_End(void) { return &staticObjectsEnd; }
#elif defined(__APPLE__)
extern word_t staticObjectsStart __asm("section$start$__DATA$__sn_objects");
extern word_t staticObjectsEnd __asm("section$end$__DATA$__sn_objects");

word_t *StaticObjects_Start(void) { return &staticObjectsStart; }
word_t *StaticObjects_End(void) { return &staticObjectsEnd; }
#else
extern word_t __start_sn_objects[] __attribute__((visibility("hidden")));
extern word_t __stop_sn_objects[] __attribute__((visibility("hidden")));

word_t *StaticObjects_Start(void) { return __start_sn_objects; }
word_t *StaticObjects_End(void) { return __stop_sn_objects; }
#endif

#endif
