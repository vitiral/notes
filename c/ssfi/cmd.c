#include <argp.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include "parse.h"
#include "topten.h"
#include "trie.h"

const struct argp argp;

/* Used by run_cmd to communicate with parse_opt. */
struct arguments {
    char *args[1]; /* PATH */
    int full;
    char *threads;
};

error_t parse_opt(int key, char *arg, struct argp_state *state);

// The primary entry point into the program.
int run_cmd(int argc, char **argv)
{
    struct arguments arguments;

    /* Default values. */
    arguments.full = 0;
    arguments.threads = "4";

    /* Parse our arguments; every option seen by parse_opt will
       be reflected in arguments. */
    argp_parse(&argp, argc, argv, 0, 0, &arguments);

    char *path = arguments.args[0];
    int threads = atoi(arguments.threads);
    if (threads <= 0) {
        printf("Invalid number of threads: %s", arguments.threads);
    }

    /* fprintf(stderr, "PATH = %s\nTHREADS = %i\nFULL = %s\n", path, threads, */
    /*         arguments.full ? "yes" : "no"); */

    TsTrie *ts = parse_path(path, threads);

    if (arguments.full) {
        ts_trie_print(NULL, ts);
    } else {
        TopTen *top = top_ten(ts);
        top_print(top);
        top_free(top);
    }

    ts_trie_free(ts);

    return 0;
}

// Argparse template taken from:
// http://www.gnu.org/software/libc/manual/html_node/Argp-Example-3.html#Argp-Example-3
const char *argp_program_version = "ssfi-v0.1.0";
const char *argp_program_bug_address = "<bgarrett@netapp.com>";

/* Program Documentation */
const char doc[] =
    "\
ssfi: an overly threaded application to count words in directories.\n\
\n\
Recursively search PATH for *.txt files which must contain ASCII text\n\
and no sym/hard links.\n\
\n\
The words (case insensitive [0-9a-z]+) in each file are then counted. When all\n\
files have been processed, the top 10 words are printed out with their counts\n\
in this form:\n\n\
  oneword\\t1000\n\
  twoword\\t30\n\
\n\
";

/* A description of the arguments we accept. */
const char args_doc[] = "PATH";

/* The options we understand. */
const struct argp_option options[] = {
    {"full", 'f', 0, 0, "Produce full list of words."},
    {"threads", 't', "THREADS", 0,
     "Number of worker threads to use. Default is 1."},
    {0}};

const struct argp argp = {
    // const struct argp_option *options
    options,

    // argp_parser_t parser
    parse_opt,

    // const char *args_doc
    args_doc,

    // const char *doc
    doc,

    // const struct argp_child *children
    // char *(*help_filter)(int key, const char *text, void *input)
    // const char *argp_domain
};

error_t parse_opt(int key, char *arg, struct argp_state *state)
{
    /* Get the input argument from argp_parse, which we
       know is a pointer to our arguments structure. */
    struct arguments *arguments = state->input;

    switch (key) {
        case 't':
            arguments->threads = arg;
            break;

        case 'f':
            arguments->full = 1;
            break;

        case ARGP_KEY_ARG:
            if (state->arg_num >= 2) {
                /* Too many arguments. */
                argp_usage(state);
            }

            arguments->args[state->arg_num] = arg;
            break;

        case ARGP_KEY_END:
            if (state->arg_num < 1) {
                /* Not enough arguments. */
                argp_usage(state);
            }
            break;

        default:
            return ARGP_ERR_UNKNOWN;
    }
    return 0;
}
