Unscoped enums:
- Same as C's. Use `static_cast<MyEnum>(4)` to create from integers.
- Polutes the namespace. Can use `MyEnum::variant` also.
- Can use `enum MyEnum: uint8_t { a, b};` to use a type other than int.

Scoped (class) enums
- `enum class MyEnum { red, blue };`
- Although these use `class` they are not (!!) the "class type" (which are only
  structs, classes and unions).
