#include "Unity/src/unity.h"
#include "parse.h"
#include "trie.h"
#include "work.h"

void setUp(void)
{
    // set stuff up here
}

void tearDown(void)
{
    // clean stuff up here
}

int main(void)
{
    UNITY_BEGIN();

    if (1) {
        // TRIE
        RUN_TEST(test_trie_len);
        RUN_TEST(test_trie_update);
        RUN_TEST(test_trie_insert);
        RUN_TEST(test_trie_equal);
        RUN_TEST(test_trie_get);
        RUN_TEST(test_trie_new);
        RUN_TEST(test_trie_child_index);

        // PARSE
        RUN_TEST(test_parse_path);
        RUN_TEST(test_parse_path_simple);
        RUN_TEST(test_parse_file);
        RUN_TEST(test_parse_word);
        RUN_TEST(test_str_basic);

        // POOL
        RUN_TEST(test_work_working);
        RUN_TEST(test_work_init);
    }

    return UNITY_END();
}
