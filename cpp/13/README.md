# OO Programming
With this chapter, I'm starting to understand a bit more of the memory model.

Class constructors are run with _memory already reserved_, but the object in a
(possibly) invalid state.

There is a LOT of complexity with how to write a constructor.

1. You can provide a default value for paramaters. These will be used with the
   default constructor
   - The default constructor is provided automatically if you don't define a
     constructor.
   - Else you need to do `MyClass = default();` -- yuck!
2. You can write the constructor inputs manually (like a function). These can
   have default values. This behaves much like a function with implicit `this`
   (very similar to java).
3. There is some oddness about setting values within the constructor body,
   specifically regarding references (which must be set immediately) and const
   (which cannot be assigned in a function body). Therefore Cpp added additional
   syntax for "member initilizer lists".
   `MyClass(int a, int b): m_a{a}, m_b{b} { /*body*/ }` Yikes!

So in summary:
```
class MyClass {
private:
  int m_a {7};
  int m_b {5};
public:
  // "Normal" constructor, no defaults
  MyClass(int a, int b)
    : m_a{a}, m_b{b}
  {
    assert(m_a > m_b); // some random assertion
  }

  // Explcit default constructor with no code.
  // This will NOT get the assertion/etc.
  // MyClass() = default;

  // Delegated constructor with one argument
  MyClass(int a): MyClass{a, a - 1} {};

  // Delegated constructor with no arguments
  MyClass(): MyClass{7, 5} {};
};
```

## Destructors
Destroctors are much less confusing:

1. It is named `~MyClass() { ... code ... }`
2. there can be only one of them.


## Adding class methods ("member functions")

You can define the public methods of a class in a .h file, then add any number
of (defined) public or (defined or undefined) private methods in a .c file

```
void MyClass::foo(int a) { ... }
```

1. methods defined in a header file are _imlicitly inline_ (!!)
2. methods defined in a cc file are like "regular" functions.

Therefore only define trivial functions in header files.

## Const methods
Just throw that `const` in an awkward spot, why not
```
void MyClass::foo(int a) const { ... }
```

## Static variables
They are _weird_.

The work roughly like you might expect (as namespaced globals). You can declare
them in the class, but sometimes not initialize them -- that has to be done
outside the class.

Initializing them can be complex -- requiring use of a lambda (?) or some bizare
inner-class dance.

