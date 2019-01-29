#include <assert.h>
#include <ctype.h>
#include <errno.h>
#include <pthread.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

// directory operations
// - http://pubs.opengroup.org/onlinepubs/7908799/xsh/dirent.h.html
// - http://pubs.opengroup.org/onlinepubs/7908799/xsh/stat.html
// - http://pubs.opengroup.org/onlinepubs/7908799/xsh/sysstat.h.html
#include <dirent.h>  // directory entries

#include "common.h"
#include "trie.h"
#include "work.h"

#define BUF_SIZE (4096)

static void parse_path_internal(Work* work, char path[MAX_PATH_LEN],
                                size_t path_hi);
static void parse_dir(Work* work, char path[MAX_PATH_LEN], size_t path_hi);
static DIR* open_dir(char* path);
static void log_ignoring_path(char* path);
static void work_alloc_and_push(Work* work, char* path, size_t path_hi);

static void parse_file(TsTrie* ts, char* fpath);
static void parse_words(TsTrie* ts, char* word, size_t* word_index, char* buf,
                        const size_t buf_hi);
static bool parse_word(char* word, size_t* word_index, char* buf,
                       size_t* buf_li, const size_t buf_hi);
static void check_word(char* word, size_t word_index);
static void* parse_worker(Work* work);

TsTrie* parse_path(char* p, size_t num_workers)
{
    char path[MAX_PATH_LEN];
    strcpy(path, p);

    Work work = work_new(num_workers);

    // start the threads
    pthread_t* threads = calloc(num_workers, sizeof(pthread_t));
    for (size_t i = 0; i < num_workers; i++) {
        worker_start(&work, &threads[i], &parse_worker);
    }

    parse_path_internal(&work, path, strlen(path));

    work_done(&work);
    for (size_t i = 0; i < num_workers; i++) {
        pthread_join(threads[i], NULL);
    }
    free(threads);

    return work_result(&work);
}

/// Recursively parse the path, inserting any words found into the trie.
///
/// path_hi points to the current high index.
static void parse_path_internal(Work* work, char path[MAX_PATH_LEN],
                                size_t path_hi)
{
    assert(path[path_hi] == 0);

    FType ftype = file_type(path, path_hi);

    if (ftype == FTypeTxt) {
        // .txt file
        parse_file(work->ts, path);
    } else if (ftype == FTypeDir) {
        parse_dir(work, path, path_hi);
    } else {
        log_ignoring_path(path);
    }
}

static void* parse_worker(Work* work)
{
    while (true) {
        char* fpath = work_pop(work);
        if (fpath == NULL) {
            worker_done(work);
            return NULL;
        }
        parse_file(work->ts, fpath);
        free(fpath);
    }
}

static void parse_dir(Work* work, char path[MAX_PATH_LEN], size_t path_hi)
{
    assert(path[path_hi] == 0);

    trim_right(path, &path_hi, '/');
    DIR* dirp = open_dir(path);

    while (dirp != NULL) {
        struct dirent* dp;
        errno = 0;
        if ((dp = readdir(dirp)) != NULL) {
            if (str_eq(".", dp->d_name) || str_eq("..", dp->d_name)) {
                continue;
            }
            size_t new_path_hi = path_join(path, path_hi, dp->d_name);
            FType ftype = file_type(path, new_path_hi);
            if (ftype == FTypeTxt) {
                work_alloc_and_push(work, path, new_path_hi);
            } else if (ftype == FTypeDir) {
                parse_dir(work, path, new_path_hi);
            } else {
                log_ignoring_path(path);
            }
            path[path_hi] = 0;
        } else {
            // done
            closedir(dirp);
            dirp = NULL;
            if (errno) {
                fprintf(stderr, "!! failure while reading dir %s\n", path);
                exit(1);
            }
        }
    }
}

static DIR* open_dir(char* path)
{
    DIR* dirp = opendir(path);
    if (dirp == NULL) {
        fprintf(stderr, "!! failed to open dir: %s\n", path);
        exit(1);
    }
    return dirp;
}

static void log_ignoring_path(char* path)
{
    /* fprintf(stderr, "\%\% Ignoring path: %s\n", */
    /*         path); */
}

static void work_alloc_and_push(Work* work, char* path, size_t path_hi)
{
    char* fpath = malloc(path_hi + 1);
    strcpy(fpath, path);
    work_push(work, fpath);
}

// Count the file, inserting found words into the trie.
static void parse_file(TsTrie* ts, char* fpath)
{
    char buf[BUF_SIZE];
    // Note: +2 so that we can always safely set wi+1=255
    char word[WORD_LENGH_MAX + 2];
    size_t word_index = 0;

    FILE* stream = fopen(fpath, "r");
    while (true) {
        size_t num_read = freadc(buf, BUF_SIZE, stream);
        if (num_read) {
            parse_words(ts, word, &word_index, buf, num_read - 1);
        }
        if (num_read != BUF_SIZE) {
            // EOF or error
            break;
        }
    }
    if (ferror(stream)) {
        fprintf(stderr, "!! error reading from file: %s\n", fpath);
        exit(1);
    }
    if (word_index) {
        // Hit EOF, insert whatever we have.
        word[word_index] = 0;
        ts_trie_insert(ts, word);
    }
    fclose(stream);
}

// Parse the words from `buf[0 buf_hi]` storing them in `ts`.
//
// If a word is unfinished it is kept in `word`.
static void parse_words(TsTrie* ts, char* word, size_t* word_index, char* buf,
                        const size_t buf_hi)
{
    size_t buf_li = 0;
    while (buf_li <= buf_hi) {
        if (parse_word(word, word_index, buf, &buf_li, buf_hi)) {
            ts_trie_insert(ts, word);
            *word_index = 0;
        }
    }
}

// Parse a word from `buf` between [li hi], storing the word in `word`
// starting at `word_index` which is mutated.
//
// return true if the word is ready to be inserted.
static bool parse_word(char* word, size_t* word_index, char* buf,
                       size_t* buf_li, const size_t buf_hi)
{
    // printf("parse_word: wi=%lu; buf_li=%lu; buf_hi=%lu\n", *word_index,
    // *buf_li, buf_hi);
    while (*buf_li <= buf_hi) {
        check_word(word, *word_index);

        char c = buf[*buf_li];
        (*buf_li)++;

        if (isupper(c)) {
            c = tolower(c);
        }

        if (isdigit(c) || islower(c)) {
            word[*word_index] = c;
            (*word_index)++;
        } else if (*word_index) {
            word[*word_index] = 0;
            return true;
        }
    }
    return false;
}

static void check_word(char* word, size_t word_index)
{
    if (word_index >= WORD_LENGH_MAX + 1) {
        word[WORD_LENGH_MAX + 1] = 0;
        fprintf(stderr, "!! word is longer than %ul: %s\n", WORD_LENGH_MAX,
                word);
        exit(1);
    }
}

// ************************************
// * TESTS
#ifdef TESTS_RUN
#include "Unity/src/unity.h"
TsTrie* create_test_multi();
TsTrie* create_test_four();

// Do the test against the path.
//
// This method will free `expected`
void do_parse_path_test(TsTrie* expected, char* p, size_t num_workers)
{
    char path[MAX_PATH_LEN];
    strcpy(path, p);

    Work work = work_new(num_workers);

    pthread_t* threads = calloc(num_workers, sizeof(pthread_t));
    for (size_t i = 0; i < num_workers; i++) {
        worker_start(&work, &threads[i], &parse_worker);
    }

    TsTrie* result = work.ts;

    parse_path_internal(&work, path, strlen(path));

    work_done(&work);
    for (size_t i = 0; i < num_workers; i++) {
        pthread_join(threads[i], NULL);
    }
    free(threads);

    TEST_ASSERT(ts_trie_equal(expected, result));

    ts_trie_free(expected);
    ts_trie_free(work_result(&work));
}

void test_parse_path()
{
    if (1) {
        printf("\n\n##### data/multi: 1 Worker\n");
        TsTrie* expected = create_test_multi();
        do_parse_path_test(expected, "data/multi", 1);
    }

    if (1) {
        printf("\n\n##### data/multi: 2 Workers\n");
        TsTrie* expected = create_test_multi();
        do_parse_path_test(expected, "data/multi", 2);
    }

    if (1) {
        printf("\n\n##### data/multi: 4 Workers\n");
        TsTrie* expected = create_test_multi();
        do_parse_path_test(expected, "data/multi", 4);
    }

    if (1) {
        printf("\n\n##### data/recursive: 4 Workers\n");
        TsTrie* expected = create_test_multi();
        TsTrie* with = create_test_multi();
        ts_trie_update(expected, expected);
        ts_trie_free(with);

        do_parse_path_test(expected, "data/recursive", 4);
    }
}

void test_parse_path_simple()
{
    // Parse using path (resolves to path)
    if (1) {
        TsTrie* expected = create_test_four();
        do_parse_path_test(expected, "data/simple/four.txt", 1);
    }

    // Parse using dir
    if (1) {
        TsTrie* expected = create_test_four();
        do_parse_path_test(expected, "data/simple/", 1);
    }
}

void test_parse_file()
{
    if (1) {
        TsTrie* expected = create_test_four();
        TsTrie* result = ts_trie_new();
        parse_file(result, "data/simple/four.txt");
        TEST_ASSERT(ts_trie_equal(expected, result));
        ts_trie_free(result);
        ts_trie_free(expected);
    };

    if (1) {
        TsTrie* expected = ts_trie_new();
        TsTrie* result = ts_trie_new();
        parse_file(result, "data/simple/empty.txt");
        TEST_ASSERT(ts_trie_equal(expected, result));
        ts_trie_free(result);
        ts_trie_free(expected);
    };
}

void test_parse_word()
{
    // test complicated
    if (1) {
        const char* ex = "1. this -^has words. I LOVE eggs.!$# 1nDoNe";
        const size_t ex_len = strlen(ex);

        char buf[BUF_SIZE];
        char word[WORD_LENGH_MAX + 2];
        size_t word_index;
        size_t buf_li = 0;
        size_t buf_hi = ex_len - 1;
        char* expected;

        // prepare the buffer
        strcpy(buf, ex);
        TEST_ASSERT_EQUAL_UINT(ex_len, strlen(buf));

        // "1."
        expected = "1";
        word_index = 0;
        TEST_ASSERT(parse_word(word, &word_index, buf, &buf_li, buf_hi))
        TEST_ASSERT_EQUAL_STRING(expected, word);
        TEST_ASSERT_EQUAL_UINT(strlen(expected), word_index);

        expected = "this";
        word_index = 0;
        TEST_ASSERT(parse_word(word, &word_index, buf, &buf_li, buf_hi));
        TEST_ASSERT_EQUAL_STRING(expected, word);
        TEST_ASSERT_EQUAL_UINT(strlen(expected), word_index);

        // "has "
        expected = "has";
        word_index = 0;
        TEST_ASSERT(parse_word(word, &word_index, buf, &buf_li, buf_hi));
        TEST_ASSERT_EQUAL_STRING(expected, word);
        TEST_ASSERT_EQUAL_UINT(strlen(expected), word_index);

        // "words."
        expected = "words";
        word_index = 0;
        TEST_ASSERT(parse_word(word, &word_index, buf, &buf_li, buf_hi));
        TEST_ASSERT_EQUAL_STRING(expected, word);
        TEST_ASSERT_EQUAL_UINT(strlen(expected), word_index);

        // "I "
        expected = "i";
        word_index = 0;
        TEST_ASSERT(parse_word(word, &word_index, buf, &buf_li, buf_hi));
        TEST_ASSERT_EQUAL_STRING(expected, word);
        TEST_ASSERT_EQUAL_UINT(strlen(expected), word_index);

        // "LOVE "
        word_index = 0;
        expected = "love";
        TEST_ASSERT(parse_word(word, &word_index, buf, &buf_li, buf_hi));
        TEST_ASSERT_EQUAL_STRING(expected, word);
        TEST_ASSERT_EQUAL_UINT(strlen(expected), word_index);

        // "eggs."
        expected = "eggs";
        word_index = 0;
        TEST_ASSERT(parse_word(word, &word_index, buf, &buf_li, buf_hi));
        TEST_ASSERT_EQUAL_STRING(expected, word);
        TEST_ASSERT_EQUAL_UINT(strlen(expected), word_index);

        // "1nDoNe" + EOF
        expected = "1ndone";  // note: ! means the word isn't complete yet
        word_index = 0;
        TEST_ASSERT(!parse_word(word, &word_index, buf, &buf_li, buf_hi));
        word[word_index] = 0;
        TEST_ASSERT_EQUAL_STRING(expected, word);
    }
}

void test_str_basic()
{
    do {
        char result[] = "a///";
        size_t hi = strlen(result);
        trim_right(result, &hi, '/');
        TEST_ASSERT_EQUAL_STRING("a", result);
    } while (0);

    do {
        char result[] = "ab";
        size_t hi = strlen(result);
        trim_right(result, &hi, '/');
        TEST_ASSERT_EQUAL_STRING("ab", result);
    } while (0);

    do {
        char* s = "abc.txt";
        TEST_ASSERT(ends_with_txt(s, strlen(s)));
    } while (0);

    do {
        char* s = "abc.tx";
        TEST_ASSERT(!ends_with_txt(s, strlen(s)));
    } while (0);

    do {
        char* s = "data/simple/four.txt";
        TEST_ASSERT(ends_with_txt(s, strlen(s)));
    } while (0);
}

// ************************************
// * Test Helpers
TsTrie* create_test_multi()
{
    TsTrie* out = create_test_four();
    ts_trie_insert_num(out, "1", 1);
    ts_trie_insert_num(out, "2", 2);
    ts_trie_insert_num(out, "3", 3);
    ts_trie_insert_num(out, "4", 4);
    return out;
}

TsTrie* create_test_four()
{
    TsTrie* out = ts_trie_new();
    ts_trie_insert(out, "one");
    ts_trie_insert_num(out, "two", 2);
    ts_trie_insert_num(out, "three", 3);
    ts_trie_insert_num(out, "four", 4);
    return out;
}

#endif
