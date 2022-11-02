
lvalue and rvalue:
  - `lvalue`: an expression that evaluates to function or object that has an
    _identity_, i.e. it is a variable, named memory address or named function.
    identifiable objects _persist beyond the scope of the expression_.
    - can be non-modifyable (const)
  - `rvalue`: an expression which is not an `lvalue`, typically constants and the
    return values of functions or operators. They only exist within the scope of
    the expression they are used.
  - `lvalue` will implicitly convert to an `rvalue`, i.e. `y = x`

There are two types of _references_ as well! lvalue and rvalue references.

An `lvalue reference` used to the be the only one that existed (prior to C++11).
- declare with `int&`
- cannot have multiple lvalue references. Literally... `int&&` is an `rvalue
  reference` (... um...)
- appears to not actually be a pointer even... is really just an alias? Not sure
  yet.
- There is also a `lvalue reference to a const`, called a `const reference`.
  This can even reference rvalues, which creates a "long lived" temporary
  object. Makes sense.

**Pass by reference**. Okay... it really is a pointer under the hood.

Pointers
- Most things are same as C
- `const int* ptr` is a **pointer to const**, it can point to constants _or non
  constants_. It can _also be changed_. However, the value it points to cannot
  be changed.
- `int const* ptr` is a **const pointer**. The pointer's address can't be
  changed _but the value pointed to can be_.
- `const int const* ptr` is a **const pointer to const**. It's totally const.

Really, pointers are just more explicit references. References do
auto-referencing and dereferencing and have less power.

Reference to pointer: `int*& ref`: allows you to change the pointer.

TLDR; references are almost identical to pointers, with the following
differences:

* No need to de-refeference. `int a& = 8; a = 7;` is valid.
* Once initialized, a reference cannot be reseated, meaning it can not be
  changed to reference another object (!). This is quite tricky. The below code
  doesn't change ref to point at y, it sets x to y (!). I.e. `ref = y` is the
  same thing as `x = y`.  `int& ref{ x }; ref = y;`
* Cannot have a null-reference, can only have a `nullptr`.
* `auto` will drop references (what!?). You must use `auto& myRef`
  if you want the reference to stay.
* The way const gets applied is weird. Refer to 9.12 when this matters.

