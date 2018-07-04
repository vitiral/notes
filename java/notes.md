
# Basics

Writing/Compiling:
- Each java `Class` should be written in a single file, `Class.java`.
- When compiled it becomes `Class.class`. When called with `java Class` it will
  execute `Class::main`
- Several files can be recompiled at the same time. `javac -depend Class.java`
  will compile `Class.java` as well as every file it depends on.
- If files are in the same directory they will automatically "import"
  eachother (dependencies are automatically resolved I guess).


