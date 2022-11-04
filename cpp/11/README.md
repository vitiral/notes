# Arrays
Basically the same as C arrays.


```

int arr[]{ 30, 50, 20, 10, 40 };

std::size(arr); // 5, number of ELEMENTS
std::sort(std::begin(arr), std::end(arr));
```

Pointer arithmetic:

> If ptr points to an integer, `ptr + 1` is the address of the next integer in
> memory after ptr. `ptr - 1` is the address of the previous integer before ptr.

You can get new memory using `int* v = new int {7};`

You delete it with `delete v;`

## new Arrays

```
int length{8};
int* array{ new int[length]{} }; // length is NOT constant
array[0] = 5;
delete[] array;
```


## for each

```
// copies each element
for (auto cpy : array)

// gets references
for (auto& ref: array)
```


## std::array
Just like rust's array type, it must have a known size. Bizarely this also
includes references to it (!).


## Sidenote
Ultra simple defer:
https://www.gingerbill.org/article/2015/08/19/defer-in-cpp/

```
template <typename F>
struct privDefer {
  F f;
  privDefer(F f) : f(f) {}
  ~privDefer() { f(); }
};

template <typename F>
privDefer<F> defer_func(F f) {
  return privDefer<F>(f);
}

#define DEFER_1(x, y) x##y
#define DEFER_2(x, y) DEFER_1(x, y)
#define DEFER_3(x)    DEFER_2(x, __COUNTER__)
#define defer(code)   auto DEFER_3(_defer_) = defer_func([&](){code;})
```
