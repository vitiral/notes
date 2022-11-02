Unscoped enums:
- Same as C's. Use `static_cast<MyEnum>(4)` to create from integers.
- Polutes the namespace. Can use `MyEnum::variant` also.
- Can use `enum MyEnum: uint8_t { a, b};` to use a type other than int.

Scoped (class) enums
- `enum class MyEnum { red, blue };`
- Although these use `class` they are not (!!) the "class type" (which are only
  structs, classes and unions).

Structs:
- Basically the same as C with better names.
- Can provide defaults. `struct Foo { int a {1}; int b {}; };`

## Struct Template

```C++
template <typename T>
struct Pair {
  T first{};
  T second{};
};

Pair<int>       p1{5, 6};
Pair<double>    p2{1.2, 3.4};
Pair<Pair<int>> p3{};
Pair            p4{1, 2}; // Pair<int> using CTAD
```

For that last part to work you need a template deduction guide:
```
template <typename T>
Pair(T, T) -> Pair<T>;
```
