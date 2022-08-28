Current place:

https://www.learncpp.com/cpp-tutorial/value-categories-lvalues-and-rvalues/

## Notes
Mostly, things made a lot of sense comming from C so far.


- Use `g++` to compile (instead of `gcc`)
- `./2/README.md` I finally got a decent mental model of the linker and the
  compiler.
- There are three forms of variable initialization:
  - `int a = 7`: copy assignment, should only use for native/const types.
  - `int a (7)`direct initialization, used in legacy code and a specific case
    (?).
  - brace initialization, modern approach. There are three forms
    - `int a {7}` direct brace initilization
    - `int a = {7}` copy brace initiilzation
    - `int a {}` value brace initialization (typically zero)
