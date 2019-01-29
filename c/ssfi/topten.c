#include <assert.h>
#include <ctype.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "topten.h"
#include "trie.h"

void top_sort_rev(TopTen* top);
void update_min_i(TopTen* top);
void put_word(TopTen* top, char* word, size_t count, size_t index);

void top_free(TopTen* top)
{
    for (size_t i = 0; i < top->num; i++) {
        free(top->words[i]);
    }
    free(top);
}

void top_print(TopTen* top)
{
    for (size_t i = 0; i < top->num; i++) {
        printf("%s\t%lu\n", top->words[i], top->counts[i]);
    }
}

// Recursively get the top ten items in the trie.
//
// This does this in the simplest possible way: by just keeping a list
// of ten items and swapping the minimums.
//
// Theoretically a minheap is faster, but I doubt the performance
// differences are signfiicant for only ten items.
TopTen* top_ten(TsTrie* ts)
{
    void walk_top(char* word, TsTrie* _ts, Trie* node, void* void_top)
    {
        TopTen* top = (TopTen*)void_top;
        size_t count = node->count;
        if (count) {
            if (top->num < 10) {
                put_word(top, word, count, top->num);
                (top->num)++;
                if (top->num >= 10) {
                    update_min_i(top);
                }
            } else {
                if (count > top->counts[top->min_i]) {
                    put_word(top, word, count, top->min_i);
                    update_min_i(top);
                }
            }
        }
    }

    TopTen* top = calloc(1, sizeof(TopTen));
    trie_walk(ts, ts->root, top, &walk_top);
    top_sort_rev(top);
    return top;
}

// Reverse-sort the top-ten via insertion sort (the fastest sort for small
// arrays).
//
void top_sort_rev(TopTen* top)
{
    for (size_t i = 1; i < top->num; i++) {
        // grow the array, keeping it sorted at each step

        for (size_t hi = i; hi > 0 && top->counts[hi] > top->counts[hi - 1];
             hi--) {
            // while the value is smaller than the value below it,
            // keep swapping it down.
            size_t tmp_c = top->counts[hi];
            top->counts[hi] = top->counts[hi - 1];
            top->counts[hi - 1] = tmp_c;

            char* tmp_w = top->words[hi];
            top->words[hi] = top->words[hi - 1];
            top->words[hi - 1] = tmp_w;
        }
    }
}

void put_word(TopTen* top, char* word, size_t count, size_t index)
{
    if (top->words[index] != NULL) {
        free(top->words[index]);
    }
    top->words[index] = malloc(strlen(word) + 1);
    strcpy(top->words[index], word);
    top->counts[index] = count;
}

// update the minimum index.
void update_min_i(TopTen* top)
{
    size_t min_i = 0;
    for (size_t i = 1; i < top->num; i++) {
        if (top->counts[i] < top->counts[min_i]) {
            min_i = i;
        }
    }
    top->min_i = min_i;
}
