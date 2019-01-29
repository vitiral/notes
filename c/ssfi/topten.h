#ifndef TENTEN_H
#define TENTEN_H

#include "trie.h"

struct TopTen_s {
    char* words[10];
    size_t counts[10];
    size_t min_i;
    size_t num;
};

typedef struct TopTen_s TopTen;
void top_free(TopTen* top);
void top_print(TopTen* top);
TopTen* top_ten(TsTrie* ts);

#endif
