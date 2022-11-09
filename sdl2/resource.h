
// Implicit copying of types which you don't want copied lead to many of the
// footguns in C++ including object slicing and performance hits.
//
// Any SDL resource should not be copied anyway, so this is especially
// important.
#define NO_COPY(C) \
  C(const C& a)            = delete; \
  C& operator=(const C& a) = delete;

// Resource class: a unique_ptr with customized destructor.
//
// SDL2 is a C library and thus does not have RAII resource handling.
// By wrapping the resources in this class, we can achieve parity with C++
// idioms at no cost.
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
  // This is a convienience function for both initialization and freeing.
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
//   DEFER(r.close());
//   ... other code
//
// `r.close()` will be called when the DEFER statement goes out of scope.
template<typename F>
class _Defer final {
  F f;
public:
  _Defer(F f): f(f) {};
  ~_Defer() { f(); }
};
template <typename F>  _Defer(F) -> _Defer<F>;
#define DEFER(CODE)  _Defer __defer ## _ ## __COUNTER__{ [&](){CODE;} }
