#include "work.h"
#include <pthread.h>
#include <semaphore.h> /* required for semaphores */
#include <stdbool.h>
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include "assert.h"
#include "common.h"
#include "trie.h"

Work work_new(size_t workers)
{
    Work work = {
        .lock = sem_new(1),
        .files_avail = sem_new(0),
        .files_free = sem_new(POOL_FILES),
        .worker_done = sem_new(0),

        .workers = workers,
        .files = {0},
        .files_len = 0,

        .ts = ts_trie_new(),
    };
    return work;
}

// Push the file path `fpath` as work to do.
void work_push(Work* work, char* fpath)
{
    // Wait for there to be room for a file
    sem_wait(work->files_free);

    // Put the file into the queue
    sem_t* lock = work->lock;
    sem_wait(lock);
    assert(work->files_len <= POOL_FILES);
    work->files[work->files_len] = fpath;
    (work->files_len)++;
    sem_post(lock);

    // Let workers know there is a file available
    sem_post(work->files_avail);
}

// Pop a file path off of `fpath`. It is the worker's job to
// `free(fpath)`.
//
// If NULL is returned that means there is no more work.
char* work_pop(Work* work)
{
    // wait for work to be available
    sem_wait(work->files_avail);

    // Pop a file off the queue.
    sem_t* lock = work->lock;
    sem_wait(lock);
    assert(work->files_len <= POOL_FILES);
    if (work->files_len == 0) {
        // No more work. The worker must end after receiving the NULL.
        sem_post(lock);
        return NULL;
    }

    size_t findex = work->files_len - 1;
    char* fpath = work->files[findex];
    work->files[findex] = NULL;
    (work->files_len)--;

    sem_post(lock);

    // Let the pool know there is free space.
    sem_post(work->files_free);
    return fpath;
}

// Start a worker by spawning a thread.
//
// It is the users job to handle joining the thread and freeing it's data.
void worker_start(Work* work, pthread_t* th, void* (*fn)(Work*))
{
    void* (*start_routine)(void*) = (void* (*)(void*))fn;
    int ret = pthread_create(
        /*pthread_t =*/th,
        /*attr= */ NULL,
        /*start_routine =*/start_routine,
        /*arg = */ (void*)work);
    assert(ret == 0);
}

// Signal that the worker is done and cleaned up.
//
// Note: This will allow `work_done` to end.
void worker_done(Work* work)
{
    sem_t* lock = work->lock;

    sem_wait(lock);
    (work->workers)--;
    sem_post(lock);

    sem_post(work->worker_done);
}

// Signal to all workers that it's time to be done and
// wait for them to all be done.
//
// This will cause work_pop to return NULL when there
// is no work left.
//
// Finally, it will call `work_result`.
//
// Using the Work object after this is UB.
void work_done(Work* work)
{
    sem_t* lock = work->lock;
    sem_wait(lock);
    for (size_t i = 0; i < work->workers; i++) {
        sem_post(work->files_avail);
    }
    sem_post(lock);

    while (work->workers) {
        sem_wait(work->worker_done);
    }
}

// Get the result of the work, which is the Trie.
TsTrie* work_result(Work* work)
{
    sem_free(work->lock);
    sem_free(work->files_avail);
    sem_free(work->files_free);
    sem_free(work->worker_done);
    for (size_t i = 0; i < work->files_len; i++) {
        free(work->files[i]);
    }
    return work->ts;
}

// ************************************
// * TESTS
#ifdef TESTS_RUN
#include <string.h>
#include "Unity/src/unity.h"

char WORKER_X_PATH[] = "X-worker.txt";
void* worker_x(Work* work);
void* worker_finish_up(Work* work);

void test_work_working()
{
    // This test puts in 4 items of work and manually takes 3 out.
    //
    // The 4th item goes to a specific thread.
    //
    // It then checks against the possible deadlock condition to make
    // sure that all resources finish.
    size_t num_workers = 4;
    char fpath[] = "N-testing-path.txt";
    size_t fpath_len = strlen(fpath);

    Work work = work_new(num_workers);

    char* file_x = malloc(fpath_len + 1);
    char* file_a = malloc(fpath_len + 1);
    char* file_b = malloc(fpath_len + 1);
    char* file_c = malloc(fpath_len + 1);

    TEST_ASSERT(file_a);
    TEST_ASSERT(file_b);
    TEST_ASSERT(file_c);

    strcpy(file_x, WORKER_X_PATH);

    fpath[0] = 'a';
    strcpy(file_a, fpath);

    fpath[0] = 'b';
    strcpy(file_b, fpath);

    fpath[0] = 'c';
    strcpy(file_c, fpath);

    work_push(&work, file_x);
    work_push(&work, file_a);
    work_push(&work, file_b);
    work_push(&work, file_c);
    TEST_ASSERT_EQUAL_UINT(work.files_len, num_workers);

    if (1) {
        char* result = work_pop(&work);
        TEST_ASSERT_EQUAL_STRING(fpath, result);
        free(result);
    }

    if (1) {
        fpath[0] = 'b';
        char* result = work_pop(&work);
        TEST_ASSERT_EQUAL_STRING(fpath, result);
        free(result);
    }

    if (1) {
        fpath[0] = 'a';
        char* result = work_pop(&work);
        TEST_ASSERT_EQUAL_STRING(fpath, result);
        free(result);
    }

    TEST_ASSERT_EQUAL_UINT(work.files_len, 1);

    pthread_t* threads = malloc(sizeof(pthread_t) * num_workers);

    worker_start(&work, &threads[0], &worker_x);

    while (work.files_len) {
        usleep(100000);
        int lock_v;
        sem_getvalue(work.lock, &lock_v);
    }
    TEST_ASSERT_EQUAL_UINT(work.files_len, 0);

    for (size_t i = 1; i < num_workers; i++) {
        worker_start(&work, &threads[i], &worker_finish_up);
    }

    work_done(&work);
    for (size_t i = 0; i < num_workers; i++) {
        pthread_join(threads[i], NULL);
    }
    free(threads);
    ts_trie_free(work_result(&work));
}

void test_work_init()
{
    if (1) {
        Work work = work_new(10);
        ts_trie_free(work_result(&work));
    }

    if (1) {
        Work work = work_new(0);
        work_done(&work);
        ts_trie_free(work_result(&work));
    }

    if (1) {
        Work work = work_new(2);
        worker_done(&work);
        worker_done(&work);
        work_done(&work);
        ts_trie_free(work_result(&work));
    }
}

// Run worker_x.
//
// This expects there is exactly one item of work left.
void* worker_x(Work* work)
{
    char* fpath = work_pop(work);
    assert(str_eq(fpath, WORKER_X_PATH));
    free(fpath);

    fpath = work_pop(work);
    assert(fpath == NULL);
    worker_done(work);
    return NULL;
}

// This expects there is no more work to do.
void* worker_finish_up(Work* work)
{
    char* fpath = work_pop(work);
    assert(fpath == NULL);
    worker_done(work);
    return NULL;
}

#endif
