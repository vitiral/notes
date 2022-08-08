Things I learned:

Even as a semi-experienced C-programmer, it turns out I never really had a good
mental model of how header and c files interacted -- and more importantly, how
they _interact with the linker_.  There is _nothing magical_ about header files.
_By convention_ they are simply files which are most typically part of
`#include` directives.

There are a few rules at play:

- A function can be _declared_ in any number of source files. So any number of
  `.c` files can `#include` identical forward declarations, 
  i.e. `int foo(int x);`
- The linker requires any function is only _defined_ in a single source file,
  i.e. `int foo(int x) { return x + 3; }`
- The linker matches all uses of a symbol to it's definition at link time.

Header files make it easier to ensure all source files agree on the
declarations. They are simply for _convienience_, and the guards/etc are just
conventions to allow multiple source files to include the same header without
worrying about order of inclusion.

