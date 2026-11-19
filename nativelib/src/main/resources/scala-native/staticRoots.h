#ifndef SCALANATIVE_STATIC_ROOTS_H
#define SCALANATIVE_STATIC_ROOTS_H

#include "staticSections.h"

#ifdef __cplusplus
extern "C" {
#endif

/* Registers the global objects section, and the inflated lock words of
 * static objects, as GC roots. Called after GC initialization, before Scala
 * initializers.
 */
SN_SECTION_LOCAL void scalanative_register_static_roots(void);

/* Called when a monitor is inflated on `object`. A static object in the class
 * or string sections then references the heap only through its lock word,
 * which becomes a GC root. Other objects are ignored. Objects have no lock
 * word without multithreading.
 */
SN_SECTION_LOCAL void scalanative_register_object_roots(void *object);

/* Programs that load extensions (scala.scalanative.runtime.Extension, whose
 * @define sets __SCALANATIVE_LIVE_EXECUTION) */
#if (defined(SCALANATIVE_COMPILE_ALWAYS) ||                                    \
     defined(__SCALANATIVE_LIVE_EXECUTION)) &&                                 \
    !defined(_WIN32)
#define SN_LIVE_EXECUTION 1

/* Live execution: registers the sections of an extension loaded into this
 * program (ext_loader.c), as this image's own are registered. Its global
 * objects become GC roots, and objects in its class and string sections are
 * static objects for scalanative_register_object_roots. Called by the loader
 * under its lock, before any code of the extension runs; extensions are never
 * unloaded.
 */
SN_SECTION_LOCAL void
scalanative_register_extension_sections(const SNStaticSection *globals,
                                        const SNStaticSection *classinfo,
                                        const SNStaticSection *strings);
#endif

#ifdef __cplusplus
}
#endif

#endif
