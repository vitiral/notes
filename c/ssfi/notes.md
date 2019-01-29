# Makefile
`make` is  the program that orchestrates compiling c files. It includes such features as:
- Each compilation results in an object (`*.o`) file that coresponds to the source file.
- Incremental compilation: detection of whether files or their dependencies have changed
  and recompilation if so.
- If any source files have been recompiled then all object files must be linked together
  to produce a new executable.

## Rules
A simple makefile consists of "rules" with the following syntax:

```
target ... : prerequisites ...
        recipe
        ...
        ...
```

- target is normally the name of the file that is *generated* by the recipe.
  - can also be the name of an action.
- a prerequisite is a file that is used to create the target.
- a recipe is an action that `make` caries out. Must start with a tab.

# Performance Data
Trie with 26 locks by char-index
```
OK data/random-shallow              threads=1 full=471.23ms partial=450.21ms
OK data/random-shallow              threads=2 full=235.02ms partial=236.11ms
OK data/random-shallow              threads=4 full=125.13ms partial=127.78ms
OK data/random-shallow              threads=8 full=87.988ms partial=87.025ms
```

**Lockless Trie**
```
OK data/random-shallow              threads=1 full=456.04ms partial=441.97ms
OK data/random-shallow              threads=2 full=230.52ms partial=231.95ms
OK data/random-shallow              threads=4 full=133.91ms partial=134.3ms
OK data/random-shallow              threads=8 full=86.928ms partial=87.623ms
```

So slightly faster for lockless, but not a ton.
