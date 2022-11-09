/************
 * Types and macros for handling resources from C.
 *
 * By wrapping C resources in the Resource class, we can achieve parity with C++
 * idioms at no cost.
 *
 * When that would require too much boilerplate or more than a pointer is being
 * managed, DEFER can be used for ensuring resources are destroyed properly.
 */

// Implicit copying of types leads to many of the footguns in C++ including
// object slicing, performance hits and confusing logic.
//
// Opinion: most types should not support copying, especially not implicit
// copying. This macro should be called for nearly all types in the game. If
// copying is useful, then define an explicit `copy()` method to achieve the
// same thing.
#define NO_COPY(C) \
  C(const C& a)            = delete; \
  C& operator=(const C& a) = delete;

// Resource class: a unique_ptr with customized destructor.
template<typename T, void D(T*)>
class Resource final {
  NO_COPY(Resource);
  T* res;
public:

  Resource(T* ptr = nullptr) : res(ptr) {}
  ~Resource() { destroy(); }

  // Get the resource, it's your job to manage it.
  T* unwrap() noexcept {
    T* out = res;
    res = nullptr;
    return out;
  }

  void destroy() noexcept {
    if(not res) return;
    D(unwrap());
  }

  // Move constructor
  Resource& operator=(Resource&& a) noexcept {
    if (&a == this) return *this;
    destroy(); // destroy our own resource
    res = a.unwrap();
    return *this;
  }

  // Direct initialization from a pointer.
  //
  // This is a convienience for both init and destroy. Attempting to init a
  // resource when it already has a value will result in an error.
  Resource& operator=(T* res) noexcept {
    if(not res) { destroy(); return *this; }
    assert(not this->res);
    this->res = res;
  }

  T& operator*() const  { return *res; }
  T* operator->() const { return res; }
  bool isNull() const { return res == nullptr; }
};

// Defer some code execution using RAII. Use like:
//
//   auto r = myResource();
//   DEFER(r.close(); cout << "Resource closed\n");
//   ... other code
//
// Everything inside the DEFER parens will be run when the DEFER statement
// goes out of scope.
template<typename F>
class _Defer final {
  F f;
public:
  _Defer(F f): f(f) {};
  ~_Defer() { f(); }
};
template <typename F>  _Defer(F) -> _Defer<F>;
#define DEFER(CODE)  _Defer __defer ## __COUNTER__{ [&](){CODE;} }
