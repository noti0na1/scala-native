// Live execution: the loader of extensions. An extension is code linked
// separately against a host's manifest and loaded into the running host with
// dlopen. The loader checks that the extension was linked for this host,
// refuses a class another extension defines, and registers the extension's
// static storage: its module instances become GC roots, and its class objects
// and string literals are treated as the host's are.
//
// RTTI depends only on a type and its ancestors, so an extension's classes
// are complete when it is linked: their lineages and itables point to the
// host's RTTI where they extend or implement host types. Nothing in them is
// assigned at load time.
//
// Compiled only into programs that use scala.scalanative.runtime.Extension,
// whose @define sets __SCALANATIVE_LIVE_EXECUTION.
#if defined(__linux__) && !defined(_GNU_SOURCE)
#define _GNU_SOURCE // RTLD_DEFAULT
#endif
#include "staticRoots.h"
#ifdef SN_LIVE_EXECUTION

#include <dlfcn.h>
#include <pthread.h>
#include <stdarg.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

// Layouts written by the toolchain (codegen/Generate.scala,
// genExtensionDescriptor); keep them in sync.
#define EXT_DESCRIPTOR_MAGIC 0x58454e53 // "SNEX"
#define EXT_DESCRIPTOR_VERSION 3

typedef struct ExtClass {
    void *rtti;
    const char *name; // NIR name
} ExtClass;

typedef struct ExtModule {
    const char *name;
    void *accessor; // `G4load`, or NULL for a constant module
    void *instance; // `G8instance` of a constant module, or NULL
} ExtModule;

typedef struct ExtDescriptor {
    int32_t magic;
    int32_t version;
    const char *hostHash; // hash of the manifest the extension was linked to
    int32_t classCount;
    ExtClass *classes; // its classes and modules
    int32_t moduleCount;
    ExtModule *modules;
    void (*init)(void); // runs static initializers, or NULL
    // Its global objects, class objects and string literals, in that order
    // (codegen/LiveSections.scala)
    const SNStaticSection *sections;
} ExtDescriptor;

typedef struct Extension {
    void *handle;
    ExtDescriptor *descriptor;
    char *path;
    // Of its static initializers, which the Scala side runs and records under
    // a lock of its own (runtime/Extension.scala)
    int initState;
    struct Extension *next;
} Extension;

// The manifest hash, which a host link defines and exports. Looked up at run
// time, so that a program that is not a host gets an error instead of a link
// failure.
static const char *hostHash(void) {
    return (const char *)dlsym(RTLD_DEFAULT, "__scalanative_ext_host_hash");
}

static pthread_mutex_t loaderLock = PTHREAD_MUTEX_INITIALIZER;
static Extension *extensions = NULL;
static _Thread_local char errorMessage[1024];

static void setError(const char *format, ...) {
    va_list args;
    va_start(args, format);
    vsnprintf(errorMessage, sizeof(errorMessage), format, args);
    va_end(args);
}

const char *scalanative_ext_error(void) { return errorMessage; }

static const char *ownerOfClass(const char *name) {
    for (Extension *ext = extensions; ext != NULL; ext = ext->next) {
        ExtDescriptor *d = ext->descriptor;
        for (int32_t i = 0; i < d->classCount; i++)
            if (strcmp(d->classes[i].name, name) == 0)
                return ext->path;
    }
    return NULL;
}

// Under the lock. Checks everything before registering anything, so that a
// refused extension leaves no trace.
static int registerExtension(ExtDescriptor *d, const char *path) {
    for (int32_t i = 0; i < d->classCount; i++) {
        const char *owner = ownerOfClass(d->classes[i].name);
        if (owner != NULL) {
            setError("%s: class %s is already defined by %s", path,
                     d->classes[i].name, owner);
            return 0;
        }
    }
    // Its module instances live as long as the process, as the host's do
    scalanative_register_extension_sections(&d->sections[0], &d->sections[1],
                                            &d->sections[2]);
    return 1;
}

void *scalanative_ext_load_with(const char *path, int global);

void *scalanative_ext_load(const char *path) {
    return scalanative_ext_load_with(path, 0);
}

// global: the extension's symbols become visible to extensions loaded after
// it, which bind to them (chained extensions)
void *scalanative_ext_load_with(const char *path, int global) {
    errorMessage[0] = '\0';
    const char *host = hostHash();
    if (host == NULL) {
        setError("this program was not linked as a live-execution host");
        return NULL;
    }
    void *handle = dlopen(path, RTLD_NOW | (global ? RTLD_GLOBAL : RTLD_LOCAL));
    if (handle == NULL) {
        setError("%s", dlerror());
        return NULL;
    }
    ExtDescriptor *d = dlsym(handle, "__scalanative_ext_descriptor");
    if (d == NULL) {
        setError("%s is not a Scala Native extension", path);
        dlclose(handle);
        return NULL;
    }
    if (d->magic != EXT_DESCRIPTOR_MAGIC) {
        setError("%s is not a Scala Native extension (bad descriptor magic)",
                 path);
        dlclose(handle);
        return NULL;
    }
    if (d->version != EXT_DESCRIPTOR_VERSION) {
        setError("%s: extension format %d, this host reads %d", path,
                 d->version, EXT_DESCRIPTOR_VERSION);
        dlclose(handle);
        return NULL;
    }
    if (strcmp(d->hostHash, host) != 0) {
        setError("%s was linked for host %.12s, this host is %.12s", path,
                 d->hostHash, host);
        dlclose(handle);
        return NULL;
    }

    pthread_mutex_lock(&loaderLock);
    for (Extension *ext = extensions; ext != NULL; ext = ext->next) {
        if (ext->handle == handle) {
            // Loaded already: the same image
            pthread_mutex_unlock(&loaderLock);
            return ext;
        }
    }
    if (!registerExtension(d, path)) {
        pthread_mutex_unlock(&loaderLock);
        // Nothing of it was registered, and no object of its classes exists
        dlclose(handle);
        return NULL;
    }
    Extension *ext = malloc(sizeof(Extension));
    ext->handle = handle;
    ext->descriptor = d;
    ext->path = strdup(path);
    ext->initState = 0;
    ext->next = extensions;
    extensions = ext;
    pthread_mutex_unlock(&loaderLock);
    // No unloading: heap objects may point to its RTTI and constants
    return ext;
}

// What the Scala side needs: module accessors and the initializer

static ExtModule *findModule(Extension *ext, const char *name) {
    ExtDescriptor *d = ext->descriptor;
    for (int32_t i = 0; i < d->moduleCount; i++)
        if (strcmp(d->modules[i].name, name) == 0)
            return &d->modules[i];
    return NULL;
}

void *scalanative_ext_module_accessor(void *ext, const char *name) {
    ExtModule *m = findModule((Extension *)ext, name);
    return m != NULL ? m->accessor : NULL;
}

void *scalanative_ext_module_instance(void *ext, const char *name) {
    ExtModule *m = findModule((Extension *)ext, name);
    return m != NULL ? m->instance : NULL;
}

// The static initializers, or NULL
void *scalanative_ext_init_function(void *ext) {
    return (void *)((Extension *)ext)->descriptor->init;
}

int scalanative_ext_init_state(void *ext) {
    return ((Extension *)ext)->initState;
}

void scalanative_ext_set_init_state(void *ext, int state) {
    ((Extension *)ext)->initState = state;
}

int scalanative_ext_class_count(void *ext) {
    return ((Extension *)ext)->descriptor->classCount;
}

// Interrupts: extension code linked with an interrupt handler polls this flag
// at method entries and loop back-edges, and calls the handler while it is
// set (codegen/LiveInterrupts.scala)
volatile int32_t scalanative_ext_interrupt_flag = 0;

void scalanative_ext_set_interrupt(int32_t value) {
    __atomic_store_n(&scalanative_ext_interrupt_flag, value, __ATOMIC_SEQ_CST);
}

#endif
