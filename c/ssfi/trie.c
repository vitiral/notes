//! An implementation of a trie.

#include <assert.h>
#include <ctype.h>
#include <semaphore.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>

#include "common.h"
#include "trie.h"

static Trie *trie_new();
static bool trie_equal(Trie *left, Trie *right);
static void trie_free(Trie *trie);
static void trie_walk_inner(word_stack word, const size_t wi, TsTrie *ts,
                            Trie *trie, void *state,
                            void (*fn)(char *, TsTrie *, Trie *, void *));

static void trie_print(Trie *trie);
static size_t char_to_index(char c);
static char index_to_char(size_t index);

// Get the child, creating if it doesn't exist.
//
// This object is threadsafe:
// - Simply incrementing/traversing is lock-free using atomics.
// - This method uses `ts` to be threadsafe.
// - `c` is used to determine which `ts->locks` to get.
// - This means that you can grow the number of leaves of this object with up
//   to 36 threads simultaniously.
//
// # Performance Notes:
// This would have poor locking performance if all words had permutations of
// only a few characters. However, in that case the number of locks required
// would be small since counting is lock-free. This is should therefore give a
// good balance of performance across almost all workloads.
Trie *trie_ensure_child(TsTrie *ts, Trie *trie, char c);

TsTrie *ts_trie_new()
{
    TsTrie *ts = calloc(1, sizeof(Trie));
    if (NULL == ts) {
        fprintf(stderr, "!! OOM\n");
        exit(1);
    }
    ts->root = trie_new();
    return ts;
}

void ts_trie_free(TsTrie *ts)
{
    trie_free(ts->root);
    free(ts);
}

static Trie *trie_new()
{
    // Get all zeros
    Trie *trie = (Trie *)calloc(1, sizeof(Trie));
    if (NULL == trie) {
        fprintf(stderr, "!! OOM\n");
        exit(1);
    }
    return trie;
}

static void trie_free(Trie *trie)
{
    for (size_t i = 0; i < TRIE_CHILDREN_NUM; i++) {
        if (trie->children[i] != NULL) {
            trie_free(trie->children[i]);
            trie->children[i] = NULL;
        }
    }
    trie->count = 0;
    free(trie);
}

// insert a string into the trie, increasing it's count by 1.
void ts_trie_insert(TsTrie *ts, char *word) { ts_trie_insert_num(ts, word, 1); }

// Insert a string to a trie and increase it's count by `insert_num`.
void ts_trie_insert_num(TsTrie *ts, char *word, size_t insert_num)
{
    assert(*word != 0);
    assert(ts != NULL);
    assert(ts->root != NULL);
    size_t num = 0;
    char *s = word;

    Trie *trie = ts->root;
    while (*s) {
        num++;
        assert(num < WORD_LENGH_MAX);
        trie = trie_ensure_child(ts, trie, *s);
        s++;
    }
    trie_increment(trie, insert_num);
}

// Get the length (number of strings with count >= 1) of the trie.
size_t trie_len(Trie *trie)
{
    size_t len = 0;
    void count_len(char *_w, TsTrie *_ts, Trie *trie, void *state)
    {
        size_t *len = (size_t *)state;
        if (trie->count) {
            (*len)++;
        }
    }
    trie_walk(NULL, trie, &len, &count_len);
    return len;
}

// Determine whether the two tries are equal.
bool ts_trie_equal(TsTrie *left, TsTrie *right)
{
    return trie_equal(left->root, right->root);
}

static bool trie_equal(Trie *left, Trie *right)
{
    if (left == NULL && right == NULL) {
        return true;
    } else if (left == NULL || right == NULL) {
        return false;
    }

    if (left->count != right->count) {
        return false;
    }

    for (size_t i = 0; i < TRIE_CHILDREN_NUM; i++) {
        if (!trie_equal(left->children[i], right->children[i])) {
            return false;
        }
    }

    return true;
}

// print all words of the trie with count > 0 in form "$word\t$count"
void ts_trie_print(char *name, TsTrie *ts)
{
    if (name != NULL) {
        printf("TRIE %s:\n", name);
    }
    trie_print(ts->root);
}

static void trie_print(Trie *trie)
{
    void print_count(char *word, TsTrie *_ts, Trie *trie, void *state)
    {
        if (trie->count) {
            printf("%s\t%lu\n", word, trie->count);
        }
    }
    trie_walk(NULL, trie, NULL, &print_count);
}

// Walk the trie using pre-order traversl
//
// Calls `fn(word, ts, trie_node, state)` at each node.
void trie_walk(TsTrie *ts, Trie *trie, void *state,
               void (*fn)(char *, TsTrie *, Trie *, void *))
{
    word_stack word = {0};
    trie_walk_inner(word, 0, ts, trie, state, fn);
}

static void trie_walk_inner(word_stack word, const size_t wi, TsTrie *ts,
                            Trie *trie, void *state,
                            void (*fn)(char *, TsTrie *, Trie *, void *))
{
    assert(trie);
    fn(word, ts, trie, state);
    for (size_t i = 0; i < TRIE_CHILDREN_NUM; i++) {
        if (trie->children[i]) {
            assert(wi < WORD_LENGH_MAX);
            word[wi] = index_to_char(i);
            word[wi + 1] = 0;
            trie_walk_inner(word, wi + 1, ts, trie->children[i], state, fn);
        }
    }
}

// Return the child of `trie` at node-index `c`, creating it if it doesn't
// exist.
Trie *trie_ensure_child(TsTrie *ts, Trie *trie, char c)
{
    assert(trie);
    size_t index = char_to_index(c);
    Trie *child = trie->children[index];
    if (child == NULL) {
        child = trie_new();
        bool swapped =
            __sync_bool_compare_and_swap(&trie->children[index], NULL, child);
        if (!swapped) {
            // Another thread beat us, free what we have.
            free(child);  // note: not trie_free as we _know_ there are no
                          // children.
            child = trie->children[index];
        }
    }
    assert(child);
    return child;
}

// Increment the count of the trie atomically.
inline void trie_increment(Trie *trie, size_t increment_num)
{
    __atomic_fetch_add(&trie->count, increment_num, __ATOMIC_SEQ_CST);
}

static size_t char_to_index(char c)
{
    assert(c >= '0' || c <= '9' || c >= 'a' || c <= 'z');

    const size_t digit_offset = '0';
    const size_t chr_offset = 'a' - ('9' - '0' + 1 /* one digit past '0' */);

    if (c <= '9') {
        if (isdigit(c)) {
            return c - digit_offset;
        }
    } else if (islower(c)) {
        return c - chr_offset;
    }
    fprintf(stderr, "!! invalid child index requested: %c\n", c);
    exit(1);
}

// convert an index to a character
static char index_to_char(size_t index)
{
    if (index < 10) {
        return '0' + index;
    } else if (index < 36) {
        return 'a' + (index - 10);
    } else {
        fprintf(stderr, "!! invalid index->char: %lu\n", index);
        exit(1);
    }
}

// ************************************
// * TESTS
#ifdef TESTS_RUN
#include "Unity/src/unity.h"

TsTrie *new_test_ts();
Trie *trie_get_child(Trie *trie, char c);
size_t trie_get(Trie *trie, char *s);

void test_trie_len()
{
    TsTrie *ts = new_test_ts();
    TEST_ASSERT_EQUAL_UINT(3, trie_len(ts->root));
    ts_trie_free(ts);
}

void test_trie_update()
{
    TsTrie *expected = new_test_ts();
    ts_trie_insert(expected, "a");
    ts_trie_insert(expected, "ac");
    ts_trie_insert(expected, "ac");
    ts_trie_insert(expected, "zy");

    TsTrie *result = new_test_ts();
    ts_trie_update(result, result);

    TEST_ASSERT(ts_trie_equal(expected, result));

    ts_trie_free(result);
    ts_trie_free(expected);
}

void test_trie_insert()
{
    TsTrie *expected = new_test_ts();
    TsTrie *result = ts_trie_new();
    ts_trie_insert(result, "a");
    ts_trie_insert(result, "ac");
    ts_trie_insert(result, "ac");
    ts_trie_insert(result, "zy");

    TEST_ASSERT(ts_trie_equal(expected, result));

    ts_trie_free(expected);
    ts_trie_free(result);
}

void test_trie_equal()
{
    TsTrie *ts = new_test_ts();
    Trie *empty = trie_new();
    TEST_ASSERT(ts_trie_equal(ts, ts));
    TEST_ASSERT(trie_equal(empty, empty));
    TEST_ASSERT(!trie_equal(empty, ts->root));
    trie_free(empty);
    ts_trie_free(ts);
}

void test_trie_new()
{
    Trie *trie = trie_new();
    trie_free(trie);
}

void test_trie_get()
{
    TsTrie *ts = new_test_ts();

    TEST_ASSERT(trie_get_child(ts->root, 'a') != NULL);
    TEST_ASSERT(trie_get_child(ts->root, 'f') == NULL);

    TEST_ASSERT_EQUAL_UINT(1, trie_get(ts->root, "a"));
    TEST_ASSERT_EQUAL_UINT(2, trie_get(ts->root, "ac"));

    TEST_ASSERT_EQUAL_UINT(0, trie_get(ts->root, "z"));
    TEST_ASSERT_EQUAL_UINT(1, trie_get(ts->root, "zy"));

    // DNE
    TEST_ASSERT_EQUAL_UINT(0, trie_get(ts->root, "f"));
    TEST_ASSERT_EQUAL_UINT(0, trie_get(ts->root, "g"));
    TEST_ASSERT_EQUAL_UINT(0, trie_get(ts->root, "zzz"));

    ts_trie_free(ts);
}

void test_trie_child_index()
{
    TEST_ASSERT_EQUAL_UINT(0, char_to_index('0'));
    TEST_ASSERT_EQUAL_UINT(1, char_to_index('1'));
    TEST_ASSERT_EQUAL_UINT(5, char_to_index('5'));
    TEST_ASSERT_EQUAL_UINT(9, char_to_index('9'));
    TEST_ASSERT_EQUAL_UINT(10, char_to_index('a'));
    TEST_ASSERT_EQUAL_UINT(11, char_to_index('b'));
    TEST_ASSERT_EQUAL_UINT(35, char_to_index('z'));
    TEST_ASSERT_EQUAL_UINT(TRIE_CHILDREN_NUM - 1, char_to_index('z'));

    TEST_ASSERT_EQUAL_UINT8('0', index_to_char(char_to_index('0')));
    TEST_ASSERT_EQUAL_UINT8('1', index_to_char(char_to_index('1')));
    TEST_ASSERT_EQUAL_UINT8('8', index_to_char(char_to_index('8')));
    TEST_ASSERT_EQUAL_UINT8('9', index_to_char(char_to_index('9')));
    TEST_ASSERT_EQUAL_UINT8('a', index_to_char(char_to_index('a')));
    TEST_ASSERT_EQUAL_UINT8('b', index_to_char(char_to_index('b')));
    TEST_ASSERT_EQUAL_UINT8('z', index_to_char(char_to_index('z')));
}

// ************************************
// * Test Helpers

// Trie with following children:
// - a    = 1;
// - ac   = 2;
// - zy   = 1;
TsTrie *new_test_ts()
{
    TsTrie *ts = ts_trie_new();

    Trie *a = trie_ensure_child(ts, ts->root, 'a');
    Trie *ac = trie_ensure_child(ts, a, 'c');

    Trie *z = trie_ensure_child(ts, ts->root, 'z');
    Trie *zy = trie_ensure_child(ts, z, 'y');

    a->count = 1;
    ac->count = 2;

    z->count = 0;
    zy->count = 1;

    assert(ts->root->children[char_to_index('a')] == a);

    return ts;
}

size_t trie_get(Trie *trie, char *s)
{
    assert(*s != 0);
    assert(trie != NULL);
    size_t num = 0;

    Trie *next = trie;
    while (*s) {
        num++;
        assert(num < WORD_LENGH_MAX);
        next = trie_get_child(next, *s);
        if (next == NULL) {
            return 0;
        }
        s++;
    }

    return next->count;
}

Trie *trie_get_child(Trie *trie, char c)
{
    size_t child = char_to_index(c);
    return trie->children[child];
}

// Update one trie with the values from another.
void ts_trie_update(TsTrie *ts, TsTrie *other)
{
    void update(char *word, TsTrie *other, Trie *node, void *void_ts)
    {
        TsTrie *ts = (TsTrie *)void_ts;
        if (node->count) {
            ts_trie_insert_num(ts, word, node->count);
        }
    }
    trie_walk(other, other->root, ts, &update);
}
#endif
