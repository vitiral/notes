
# Basics

Writing/Compiling:
- Each java `Class` should be written in a single file, `Class.java`.
- When compiled it becomes `Class.class`. When called with `java Class` it will
  execute `Class::main`
- Several files can be recompiled at the same time. `javac -depend Class.java`
  will compile `Class.java` as well as every file it depends on.
- If files are in the same directory they will automatically "import"
  eachother (dependencies are automatically resolved I guess).

Running:
- Run with `java CLASSNAME`
- All java programs are bytecode.
  - Performance-critical pieces can be JIT compiled to machine code.
  - This _cannot_ happen upfront because certain checks may need to happen at
    runtime.

## Object Oriented Programming (OOP)

All java programs are object oriented.

A program can be organized two ways:
- Around it's code (what it is _doing_, i.e. functions). This is structured programming like C.
- Around it's data (what it is affecting, i.e. methods). This is OOP like Java.

Three principles:
- **Encapsulation**: binds together code and the data it manipulates and keeps both
  safe from outside interference and misuse.
  - creates a _black box_, within which are all necessary data and code.
- **Polymorphism**: (defintion: many forms) the quality that allows one
  interface to access a general class of action.
- **Inheritance**: the process by which one object can aquire properties of
  another object.

