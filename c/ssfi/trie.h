#ifndef TRIE_H
#define TRIE_H
#include <semaphore.h> /* required for semaphores */
#include <stdbool.h>
#include <stddef.h>

#define TRIE_CHILDREN_NUM (36)
#define WORD_LENGH_MAX (255)

struct Trie_s {
    // The count at this index
    size_t count;

    // The children of the node.
    struct Trie_s* children[TRIE_CHILDREN_NUM];
};
typedef struct Trie_s Trie;

// Threadsafe trie root.
//
// # Notes:
// At one time this kept an array of semaphore locks
// that were looked up by the index.
//
// However, it was discovered that it was pretty
// easy to make this data structure lock free,
// so that is now done instead.
struct TsTrie_s {
    // The root node.
    Trie* root;
};
typedef struct TsTrie_s TsTrie;
typedef char word_stack[WORD_LENGH_MAX + 1];

TsTrie* ts_trie_new();
void ts_trie_free(TsTrie* ts);
void ts_trie_insert(TsTrie* ts, char* word);
void ts_trie_insert_num(TsTrie* ts, char* word, size_t insert_num);
size_t trie_len(Trie* root);
void trie_increment(Trie* parent, size_t increment_num);
bool ts_trie_equal(TsTrie* left, TsTrie* right);
void ts_trie_print(char* name, TsTrie* ts);
void trie_walk(TsTrie* ts, Trie* trie, void* state,
               void (*fn)(char*, TsTrie*, Trie*, void*));

#ifdef TESTS_RUN

// Insert a string multiple times into a trie.
void ts_trie_insert_num(TsTrie* ts, char* s, size_t num);
void ts_trie_update(TsTrie* ts, TsTrie* other);

void test_trie_len();
void test_trie_update();
void test_trie_insert();
void test_trie_equal();
void test_trie_get();
void test_trie_new();
void test_trie_child_index();
#endif

#endif
