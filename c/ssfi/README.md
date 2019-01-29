This is my submission for the REDACTED development assignment.

The c source files are in the main directory, with the coresponding
`Makefile`.

If you have the standard build tools installed you should be
able to use:
- `git clone this repo and cd here
- `make` to build the binary
- `./bin/ssfi --help` to run the binary
- `make test` to run the tests. Requires python3 and valgrind. Tests include:
  - c unit tests run inside of valgrind.
  - python driven end-to-end and performance tests.
  - one end-to-end test run inside of valgrind.

The tests auto-create a randomly generated (and very large) folder at
`data/random-shallow`. An example run might look like the following (note: I
removed several `INFO` lines from python for brevity)

```
##### Running python tests:
OK data/simple                      threads=4 full=4.8268ms partial=4.2703ms
OK data/multi                       threads=4 full=4.3094ms partial=3.4273ms
OK data/recursive                   threads=4 full=3.305ms partial=4.1621ms
OK data/random1                     threads=4 full=4.1022ms partial=3.7518ms
## NOTE: Generating random data at data/random-shallow
OK data/random-shallow              threads=1 full=494.65ms partial=521.26ms
OK data/random-shallow              threads=2 full=255.53ms partial=248.82ms
OK data/random-shallow              threads=4 full=140.15ms partial=138.44ms
OK data/random-shallow              threads=8 full=91.082ms partial=90.2ms
- INFO: python parsed data/random-shallow in 1514.8062705993652ms
```

As you can see, increasing the number of threads has dramatic impact on
performance.

> Note: `full` is with the `-f` flag, which prints ALL words and their counts.
> (not just the top 10). This is used for validation testing.


# Basic Architecture

My `ssfi` implementation has the following architecture:
- **Threadsafe Trie for couting** (`trie.h`): uses a threadsafe lockless
  `Trie` for counting the words efficiently.
  - Traversing and incrementing do not require a lock (incrementing is atomic).
  - Inserting allocates and then attempts an atomic swap. If it succeeds the
    node is used, else it is freed.
    - Originally, inserting a leaf requires a lock. However, there were 36
      locks. If you are inserting an `a` you will take the `a` lock, `b` the
      `b` lock, etc. Note that lockless approach was only _marginally_ faster,
      but is probably not as succeptible to slowdown by character choice.
- **Work Distribution** (`work.h`): Work is distributed via a simple locked
  stack. This requires a lock for both the producers and consumers.
  - Performance could probably be improved using a queue where
    producers/consumers use different locks.
  - Obviously a lock-free queue would be an improvement as well.
  - It is not believed that this has significant impact on performance except
    for a large amount of small files, since I believe the primary constraint
    is the speed of the directory walker. This may be incorrect however.
  - The `Work` object also helps manage waiting for all threads to finish
    with `work_done`.
- **Parsing** (`parse.h`): parsing combines the above to execute on multiple
  threads. The threadsafe `TsTrie` makes most of the logic pretty
  straight forward.
- **Testing**: sanity-level unit tests as well as extensive end to end testing.
  - Each layer has sanity unit tests. See `test.c`
  - The python `ssfi_py` module reimplements the logic in about 50 lines of python
    for verification. The output of `ssfi` and `ssfi_py` are compared and expected
    to be equal for all tests.
  - `sssfi_py` can also output a random folder which is extremely large. This folder
    is used for performance and scale testing.


# Known Issues / Edge Cases / Improvements
- A file with name `.txt` will be included in the search. It wasn't specified
  whether this is correct behavior or not.
- Performance is poor when there is a large amount of small files, being
  constrained by the speed of directory walker (additional threads do not
  increase performance).
  - Could maybe be improved through multiple walkers and probably through a
    lock-free queue.
  - May want to profile the walking code. I have heard you can make
    non-blocking syscalls to "look ahead" for instance. See
    https://github.com/BurntSushi/walkdir
- Randomized testing has been done end-to-end, but I would like property based
  testing for the lower layers. This has been left unfinished.
- I would like to clean up the build process. Move src files to `src/`, build
  object files in a `build/` directory, etc.
- Memory use could be extremely poor for certain sets of words. A more
  efficient implementation could be made where each TrieNode starts out
  supporting only a couple of nodes and then expands to supporting all 36 if
  they are utilized.  This would reduce memory for the end of long words where
  there is little overlap.
- The maximum word length is capped at 255 bytes for storing words on the stack
  in traversal alorithms. Change `trie.h::WORD_LENGTH_MAX` to change.
- The maximum path length is capped at 1028 bytes. Change
  `common.h::MAX_PATH_LEN` to change.


# Conclusion
**It works** (yay!) and I believe it is well tested. This is my first time
using C in over 3 years and I previously only used it for microcontrollers.
Basically I had to learn how to use `make` as well as the entire c stdlib.

It was definitly challenging, but a ton of fun! Sorry if there is anything
difficult to read about my code due to my greenness. It was a lot of fun to
do though and I had a blast.

I hope you enjoy :)
