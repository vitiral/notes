#ifndef POOL_H
#define POOL_H

#include <semaphore.h> /* required for semaphores */
#include <stdbool.h>
#include <stddef.h>
#include "trie.h"

#define POOL_FILES (256)

// A pool of work. The workers take a reference to this struct to get thier
// work.
//
// Basic operation:
// - Access to _any_ non-semaphore requires `lock`.
// - Any number of producers can push files onto the work via `work_push`,
//   which decrements `files_free` and increments `files_avail`.
// - Workers wait for files using `work_pop` which decrements
//   `files_avail` and increment `files_free`.
//   Note: Workers are responsible for freeing the fpath.
// - When the producer is done, it calls `work_done` which simply increments
//   `files_avail` by the number of workers.
// - If a worker finds files_len == 0 it ends. This can only happen after
//   `work_done` has been called.
// - When a worker quits, it must call `worker_done`
struct Work_s {
    // all the locks
    sem_t* lock;
    sem_t* files_avail;
    sem_t* files_free;
    sem_t* worker_done;

    // available workers/work
    size_t workers;
    char* files[POOL_FILES];
    size_t files_len;

    // where to put data
    TsTrie* ts;
};

typedef struct Work_s Work;

Work work_new();
TsTrie* work_result(Work* work);
void work_push(Work* work, char* fpath);
char* work_pop(Work* work);
void work_done(Work* work);
void worker_start(Work* work, pthread_t* th, void* (*fn)(Work*));
void worker_done(Work* work);

#ifdef TESTS_RUN
void test_work_init();
void test_work_working();
#endif

#endif
