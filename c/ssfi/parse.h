#ifndef COUNT_FILE_H
#define COUNT_FILE_H

#include "trie.h"

TsTrie* parse_path(char* p, size_t num_workers);

#ifdef TESTS_RUN
void test_parse_path();
void test_parse_path_simple();
void test_parse_file();
void test_parse_word();
void test_str_basic();
#endif

#endif
