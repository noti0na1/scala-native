// AddressSanitizer's options that the runtime depends on
#if defined(__has_feature)
#if __has_feature(address_sanitizer)
#define SN_ASAN
#endif
#endif
#if defined(__SANITIZE_ADDRESS__) && !defined(SN_ASAN)
#define SN_ASAN
#endif

#ifdef SN_ASAN
#include <stdio.h>
#include <unistd.h>
#include "string_constants.h"

// The runtime uses the addresses of locals as positions on the machine stack,
// so AddressSanitizer must not move them to its fake stack.
const char *__asan_default_options(void) {
    return "detect_stack_use_after_return=0";
}

extern int __asan_option_detect_stack_use_after_return;

// Runs after the sanitizer has read ASAN_OPTIONS
__attribute__((constructor)) static void checkAsanOptions(void) {
    if (__asan_option_detect_stack_use_after_return) {
        fputs(SN_ERROR_MSG("AddressSanitizer's detect_stack_use_after_return "
                           "is not supported: the collectors scan the machine "
                           "stack, and not the sanitizer's fake one\n"),
              stderr);
        _exit(1);
    }
}
#endif
