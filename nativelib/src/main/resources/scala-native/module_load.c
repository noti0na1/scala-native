#ifdef SCALANATIVE_MULTITHREADING_ENABLED
#include "stdatomic.h"
#include <stdint.h>
#include "gc/shared/ScalaNativeGC.h"
#include "gc/shared/ThreadUtil.h"
#include "nativeThreadTLS.h"

#ifdef WIN32
#include <windows.h>
#elif _POSIX_C_SOURCE >= 199309L
#include <time.h> // for nanosleep
#else
#include <unistd.h> // for usleep
#endif

#include <assert.h>

// cross-platform sleep function
static void sleep_ms(int milliseconds) {
#ifdef WIN32
    Sleep(milliseconds);
#elif _POSIX_C_SOURCE >= 199309L
    struct timespec ts;
    ts.tv_sec = milliseconds / 1000;
    ts.tv_nsec = (milliseconds % 1000) * 1000000;
    nanosleep(&ts, NULL);
#else
    if (milliseconds >= 1000)
        sleep(milliseconds / 1000);
    usleep((milliseconds % 1000) * 1000);
#endif
}

static void sleep_us(int microseconds) {
#ifdef WIN32
    Sleep(microseconds >= 1000 ? microseconds / 1000 : 0);
#elif _POSIX_C_SOURCE >= 199309L
    struct timespec ts;
    ts.tv_sec = microseconds / 1000000;
    ts.tv_nsec = (microseconds % 1000000) * 1000;
    nanosleep(&ts, NULL);
#else
    usleep(microseconds);
#endif
}

typedef _Atomic(void **) ModuleRef;
typedef ModuleRef *ModuleSlot;
typedef void (*ModuleCtor)(ModuleRef);
typedef struct InitializationContext {
    ModuleRef instance;
    struct InitializationContext *previous;
} InitializationContext;
static SN_ThreadLocal InitializationContext *currentInitializationContext =
    NULL;

// Module instances are aligned pointers. Tag the otherwise stack-local
// context so readers never need to dereference it to distinguish it from an
// initialized module.
#define INITIALIZATION_CONTEXT_TAG ((uintptr_t)1)

static void **initializationContextRef(InitializationContext *ctx) {
    return (void **)((uintptr_t)ctx | INITIALIZATION_CONTEXT_TAG);
}

static bool isInitializationContext(ModuleRef module) {
    return ((uintptr_t)module & INITIALIZATION_CONTEXT_TAG) != 0;
}

extern ModuleRef scalanative_initializeModule(ModuleCtor ctor,
                                              ModuleRef instance,
                                              ModuleSlot slot, void *classInfo);
extern ModuleRef scalanative_awaitForInitialization(ModuleSlot slot,
                                                    void *classInfo);

// The tag can be inspected without dereferencing the stack-local context.
bool scalanative_isModuleInitializationContext(ModuleRef module) {
    return isInitializationContext(module);
}

ModuleRef
scalanative_moduleInitializationInstanceForCurrentThread(ModuleRef module) {
    InitializationContext *ctx = currentInitializationContext;
    while (ctx != NULL) {
        if (initializationContextRef(ctx) == module)
            return ctx->instance;
        ctx = ctx->previous;
    }
    return NULL;
}

void scalanative_completeModuleInitializationForCurrentThread(void) {
    assert(currentInitializationContext != NULL);
    currentInitializationContext = currentInitializationContext->previous;
}

// The thread that waits for a module on the class's monitor, on this carrier.
// Entering and waiting on a monitor run library code, which may find another
// module in initialization; the thread then waits for it without a monitor,
// so that the waits cannot nest without end. The running thread is told apart
// from the other threads that a carrier runs by its Thread object.
static SN_ThreadLocal void *currentModuleWaiter = NULL;

static void *runningThread(void) {
    void *thread = scalanative_currentThread();
    return thread != NULL ? thread : (void *)&currentThreadInfo;
}

bool scalanative_isWaitingForModule(void) {
    return currentModuleWaiter == runningThread();
}

void *scalanative_beginWaitForModule(void) {
    void *previous = currentModuleWaiter;
    currentModuleWaiter = runningThread();
    return previous;
}

void scalanative_endWaitForModule(void *previous) {
    currentModuleWaiter = previous;
}

// A virtual thread may be unmounted while it waits on the monitor, and go on
// on another carrier: it takes its mark out of the carrier's storage when it
// is unmounted, and back in when it is mounted.
void *scalanative_swapModuleWaiter(void *waiter) {
    void *previous = currentModuleWaiter;
    currentModuleWaiter = waiter;
    return previous;
}

// Pauses a wait for a module without a monitor: yields at first, then sleeps
// up to a millisecond at a time, unmanaged so that collections go on.
void scalanative_moduleInitializationPause(int32_t attempt) {
    const int32_t yields = 64;
    if (attempt < yields) {
        thread_yield();
        return;
    }
    int32_t shift = attempt - yields;
    if (shift > 10)
        shift = 10;
    scalanative_GC_set_mutator_thread_state(GC_MutatorThreadState_Unmanaged);
    sleep_us(1 << shift);
    scalanative_GC_set_mutator_thread_state(GC_MutatorThreadState_Managed);
}

NOINLINE static ModuleRef
__scalanative_waitForModuleInitialization(ModuleSlot slot, void *classInfo) {
    // Match the shared pointer against this thread's live context chain without
    // dereferencing stack storage owned by another thread.
    ModuleRef module = atomic_load_explicit(slot, memory_order_acquire);
    ModuleRef instance =
        scalanative_moduleInitializationInstanceForCurrentThread(module);
    if (instance != NULL)
        return instance;
    return scalanative_awaitForInitialization(slot, classInfo);
}

NOINLINE static ModuleRef __scalanative_startAndWaitForModuleInitialization(
    ModuleSlot slot, void *classInfo, size_t size, ModuleCtor ctor) {
    InitializationContext ctx = {};
    // Populate the owner-only context before publishing its tagged address.
    // Other threads use the tag only and wait on the class monitor; the owner
    // recognizes its live contexts by pointer equality through thread-local
    // storage, so it never dereferences another thread's stack.
    ModuleRef instance = scalanative_GC_alloc(classInfo, size);
    ctx.instance = instance;
    ctx.previous = currentInitializationContext;
    void **expected = NULL;
    if (atomic_compare_exchange_strong(slot, &expected,
                                       initializationContextRef(&ctx))) {
        currentInitializationContext = &ctx;
        return scalanative_initializeModule(ctor, instance, slot, classInfo);
    } else {
        return __scalanative_waitForModuleInitialization(slot, classInfo);
    }
}

/* Load module, if required.  The fast path is inlined into the caller. While
 * the slow path not-inlined to avoid pressure on the instruction cache.
 */
INLINE ModuleRef __scalanative_loadModule(ModuleSlot slot, void *classInfo,
                                          size_t size, ModuleCtor ctor) {
    ModuleRef module = atomic_load_explicit(slot, memory_order_acquire);

    if (module == NULL)
        return __scalanative_startAndWaitForModuleInitialization(
            slot, classInfo, size, ctor);

    if (!isInitializationContext(module) && *module == classInfo)
        return module;
    else
        return __scalanative_waitForModuleInitialization(slot, classInfo);
}

#endif
