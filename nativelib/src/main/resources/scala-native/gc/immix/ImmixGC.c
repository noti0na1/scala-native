#if defined(SCALANATIVE_GC_IMMIX)

#include <stdio.h>
#include <stdlib.h>
#include <memory.h>
#include "shared/GCTypes.h"
#include "Heap.h"
#include "immix_commix/datastructures/Stack.h"
#include "State.h"
#include "immix_commix/utils/MathUtils.h"
#include "WeakReferences.h"
#include "immix_commix/Settings.h"
#include "shared/Parsing.h"
#include "shared/Log.h"
#include "shared/jmx.h"
#include "shared/StaticObjects.h"
#ifdef SCALANATIVE_MULTITHREADING_ENABLED
#include "immix_commix/Synchronizer.h"
#endif
#include "MutatorThread.h"
#include <stdatomic.h>
#include "nativeThreadTLS.h"
#include <assert.h>

void scalanative_afterexit() { Stats_OnExit(heap.stats); }

NOINLINE void scalanative_GC_init() {
    volatile word_t dummy = 0;
    dummy = (word_t)&dummy;
    GC_Log_Init();
    Settings_Init();
    Heap_Init(&heap, Settings_MinHeapSize(), Settings_MaxHeapSize());
    Stack_Init(&stack, INITIAL_STACK_SIZE);
    Stack_Init(&weakRefStack, INITIAL_STACK_SIZE);
#ifdef SCALANATIVE_MULTITHREADING_ENABLED
    Synchronizer_init();
#endif
    MutatorThreads_init();
    if (!MutatorThread_init((word_t **)dummy)) // approximate stack bottom
        abort();
    customRoots = GC_Roots_Init();
    if (customRoots == NULL)
        abort();
#ifdef SCALANATIVE_MULTITHREADING_ENABLED
    // An inflated lock word of an object outside the heap, such as a class's,
    // is the only reference to its monitor
    AddressRange staticObjects = {StaticObjects_Start(), StaticObjects_End()};
    if (!GC_Roots_Add(customRoots, staticObjects))
        abort();
#endif
    atexit(scalanative_afterexit);
}

INLINE void *scalanative_GC_alloc_small(Rtti *info, size_t size) {
    size = MathUtils_RoundToNextMultiple(size, ALLOCATION_ALIGNMENT);

    Object *alloc = (Object *)Allocator_AllocInline(
        &currentMutatorThread->allocator, &heap, size);
    alloc->rtti = info;
    return (void *)alloc;
}

INLINE void scalanative_GC_collect() { Heap_Collect(&heap, &stack); }

#endif
