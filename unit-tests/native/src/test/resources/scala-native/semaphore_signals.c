// For scala.scalanative.runtime.gc.GCThreadSignalsTest

// For sigaction and usleep with glibc
#define _GNU_SOURCE
#if defined(__APPLE__) || defined(__linux__)
#include <fcntl.h>
#include <pthread.h>
#include <semaphore.h>
#include <signal.h>
#include <stdbool.h>
#include <stdio.h>
#include <string.h>
#include <unistd.h>

// The collectors' semaphore functions (gc/shared/ThreadUtil.c)
extern bool semaphore_open(sem_t **ref, char *name, unsigned int initValue);
extern bool semaphore_wait(sem_t *ref);
extern bool semaphore_unlock(sem_t *ref);

static volatile sig_atomic_t scalanative_test_semaphore_signals = 0;
static sem_t *scalanative_test_semaphore;
static volatile int scalanative_test_semaphore_acquired = -1;

static void scalanative_test_semaphore_handler(int sig) {
    (void)sig;
    scalanative_test_semaphore_signals++;
}

static void *scalanative_test_semaphore_waiter(void *arg) {
    (void)arg;
    scalanative_test_semaphore_acquired =
        semaphore_wait(scalanative_test_semaphore) ? 1 : 0;
    return NULL;
}

// Signals a thread waiting in semaphore_wait, then posts. Returns 1 if the
// wait acquired the semaphore, 0 if it failed, -1 if the test could not run.
int scalanative_test_semaphore_wait_with_signals(void) {
    char name[64];
    snprintf(name, sizeof(name), "/sn_test_sem_%d", (int)getpid());
    sem_unlink(name);
    if (!semaphore_open(&scalanative_test_semaphore, name, 0))
        return -1;
    struct sigaction action, previous;
    memset(&action, 0, sizeof(action));
    action.sa_handler = scalanative_test_semaphore_handler;
    sigemptyset(&action.sa_mask);
    action.sa_flags = SA_RESTART;
    int result = -1;
    if (sigaction(SIGWINCH, &action, &previous) == 0) {
        scalanative_test_semaphore_signals = 0;
        scalanative_test_semaphore_acquired = -1;
        pthread_t waiter;
        if (pthread_create(&waiter, NULL, scalanative_test_semaphore_waiter,
                           NULL) == 0) {
            usleep(20000);
            for (int i = 0; i < 20; i++) {
                pthread_kill(waiter, SIGWINCH);
                usleep(1000);
            }
            semaphore_unlock(scalanative_test_semaphore);
            pthread_join(waiter, NULL);
            if (scalanative_test_semaphore_signals > 0)
                result = scalanative_test_semaphore_acquired;
        }
        sigaction(SIGWINCH, &previous, NULL);
    }
    sem_close(scalanative_test_semaphore);
    sem_unlink(name);
    return result;
}
#else
int scalanative_test_semaphore_wait_with_signals(void) { return -1; }
#endif
