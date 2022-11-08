Current place:

https://www.learncpp.com/

## Learn C++ In Y Minutes
https://learnxinyminutes.com/docs/c++/

Some minor differences with C
- Use `#include <cstdio>` instead of `#include <stdio.h>`
- Can use `and or not` instead of `&& || !` (can use latter also)


Function overloading:
- Rather complex process. Basically tries to do it directly, else tries to
  convert arguments, else fails.
- Supports default arguments.

Strings:
- C++ strings are mutable
- Support insertion (`<<`) and extraction (`>>`) operators.

References (`&`):
- Like pointers, but cannot be "unseated" (can not change address they point
  to).
- Syntax essentially treats them like an _alias_. When passed to functions they
  behave like pointers.
- There are also `&&` which are specifically for rvalues (temporary objects).

Classes are similar to java in some ways... but wow is there complexity:
- Initializer lists You use `MyClass(int a=4): m_a{4} { ... impl ... }`
- Calling dependent constructors and virtual classes (diamond problem). I'm not
  going to go into detail here because hopefully this is rarely a problem, but
  needless to say if you _don't_ use virtual classes then you can have _multiple
  copies of inherited classes_. Yikes.
- there is syntax like `MyClass() = default;` which creates a default
  constructor, or `MyClass* = delete

There are a LOT of uses of `const`. Just in a function:

```C++
// Const reference to const Int pointer
//                     reference to a const and const int
//                                                constexptr body
int const* const&  foo(const int& a, const int b) const { ... impl ... };
```

The use of a constexpr body is that the function can then be used in a
constexpr.

Lambdas are
```
// capture      args            body
   [=copy, &ref](int a, int& b) { return copy + ref + a + b; }
```

You can use `new Type` and `delete Type`. Typically better is to use
`unique_ptr` or `shared_ptr`

- `auto myUnique = make_unique<Type>(arg1, arg2)`
- `auto myShared = make_shared<Type>(arg1, arg2)`

Tuples
```
Create:
auto first = make_tuple(10, 'A');

Get:
assert(get<0>(first) == 10);

Unpack:
int firstInt; char firstChar;
tie(firstInt, firstChar) = first;
```

Classes
```
// In .h file
class Dog {
private: // redundant (default)
  std::string name   {"Bubsie"}; // default value
  int         weight {40};

public:
  Dog() = default; // default constructor

  // If defined in a header file, methods are inline
  void setName(const std::string& name) { this->name = name; }
  void setWeight(const int weight) { this->weight = weight; }

  // When defined in a Cpp file, the method is not inline
  // Use const so that it can be used with a const reference.
  void print() const;

  // Virtual functions can get overriden by children classes.
  // and a reference to the children will use the overriden class.
  virtual void bark();

  // Destructor. If we had pointers, we should free them.
  virtual ~Dog();
}; // don't forget semi-colon

// In .cc file:
void Dog::print() const {
  std::cout << "Dog is " << name << " and weights " << weight << "kg\n";
}

void Dog::bark() {
  std::cout << "Dog " << name << " says woof!\n";
}

Dog::~Dog() { std::cout << "Goodbye " << name << '\n'; }

// Inherits everything from Dog but cannot access private.
class OwnedDog : public Dog {
public:
  void setOwner(const std::string& owner) { this->owner = owner; }


  // Demonstrate how to disable copy constructor and assignment operator
             OwnedDog(const OwnedDog& a) = delete;
  OwnedDog& operator=(const OwnedDog& a) = delete;
private:
  std::string owner;
};


void OwnedDog::bark() {
  std::cout << "Dog " << name << " owned by " << owner << " says woof!\n";
}
```

Now if you pass an OwnedDog as a `Dog&`:
- Calling `dog.bark()` will call `OwnedDog::bark`
- Calling `dog.print()` will call **`Dog::print`** since it is not virtual (!!)
- Initializing a `Dog` by copying an `OwnedDog` will cause **object slicing**
  - note: we only disabled copying for fun, but this can really happen in the
    wild!
  - in general: always accept a non-final object by reference or unique_ptr -- never the
    whole thing!




## Side Notes
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

