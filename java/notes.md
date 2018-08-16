
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

# Advanced Java

## Builtin Methods
You can implement the following builtin methods (to all Object types):
- `clone()`: tricky to get right as you probably don't want it to return
  references to it's subobjects. You also want to call `(TYPE)super.clone()`
  first and then override any attached objects.
- `finalize()`: called by GC before desruction. *Here be dragons.*
- `equals(Object)`: by default compares ids, override to do more complex equality.
  Note that `==` does NOT call `equals`.
- `toString()`: debug form of object
- `hash()`: the hash code of the object. Call `Objects.hash(a, b, c, ...)` to hashh
  multiple objects when overriding `hash`.

## Interfaces
An interface is a way to do *contract-driven development*. Interfaces can have
default methods (since Java 8) as well as default constants.

All methods on an interface are `public abstract` and constants are `static
final`, regardless of whether they say so in code.

Also, there is a `@FunctionalInterface` for any interfaces with the method `run()`.
These can take lambdas `() -> System.out.println("foo bar")`

## Abstract Classes
Abstract classes are very similar to interfaces except theny can use privacy
rules, etc as well as implement pretty much anything a class can.

## Iheritance
- `public`: anybody can access/call the variable/method
- `protected`: only subclasses can access/call.
- `private`: only the file can access/call.
- `final`: makes it impossible to override the variable/method.

Some rules:
- Every subclass can override any inherited method of it's parent unless it was
  specified as `final`.
  - However: please use `@Override` when doing so.
- Prefer interfaces to classes/abstract-classes whenver possible.

# Generics
They are _very_ similar to abstract classes in python or even Traits in rust.
The syntax is almost identical to rust.

```java
public class GenericMethods<T> {
  public GenericMethods(final T initialAction) {
    // implementation here
  }

  public<J> GenericMethods(final T initialAction, final J nextAction) {
    // implementation here
  }
}
```

## Limitations of Generics
There are some _major_ limitations:
- Primitive types (int, long, byte, ...) are not allowed to be used so you have
  to use Integer, Long, Byte, etc. This causes _implicit_ boxing/unboxing of
  primitive values.
- Type erasure: you cannot use generics for overloading methods since the
  actual type will be _erased_ at runtime. This is because the JVM bytecode has
  _all_ concrete types erased and replaced with the `Object` class.
  - You cannot use `isinstanceof` for generic types.
- You cannot create an array of instances of a generic type.

## Bounded Types
Use the `extends` keyword to restrict the type parameter to some other class or
to require an interface.

`extends` class:
```
public<T extends InputStream> void read(final T stream) {
  // Some implementation here
}
```

`extends` interfaces:
```
public<T extends Serializable> void store(final T object) {
  // Some implementation
}
```

multiple stuff
```
public<T extends Serializable & Externalizable & Cloneable> void persist(final T object) {
  // implementation
}
```

If you don't care about the type name:
```
public void store(final Collection<? extends Serializable> objects) // ...

// or if we don't care about that even
public void store(final Collection<?> objects) // ...
```

There is also `super`:
```
public void interate(final Collection<? super Integer> objects) {
  // some implementation
}
```

Basically, these are opposites of eachother:
- `void doThing(Collection<? extends Other> thing)`: thing is a **subclass** of `Other`, i.e. `class Thing extends Other`.
- `void doThing(Collection<? super Other> thing)`: thing is a **superclass** of `Other`, i.e. `class Other extends Thing`.

# Enums and Annotations
Full enum declaration:

```
public enum DaysOfWeek {
  // variants can use the constructor, but external files cannot.
  MONDAY(false),
  TUESDAY(false),
  WEDNESDAY(false),
  ...(false),
  SATURDAY(true),
  SUNDAY(true);

  private bool final isWeekend;

  // constructors **must** be private
  private DaysOfWeek( final boolean isWeekend ) {
    this.isWeekend = isWeekend;
  }

  public boolean isWeekend() {
    return isWeekend;
  }


}
```

Enums are actually implemented as an object which extends `Enum<Self>`

Some methods:
- `String name()`: get the name of this instance.
- `int ordinal()`: get the ordinal value of this instance.
- `T[] values()`: get all enum values possible.
- `T ValueOf(String name)`: convert `name -> T`.

enums can also be used in switch/case statements:
```
swith(instance) {
  case MONDAY:
    // Do something
    break;

  case TUESDAY:
    // Do something
    break;

  // ...
}

```

## Annotations
Annotations are not all that interesting but I can see that they are incredibly useful.
Their main benefit is that they allow for signaling to the compiler and tooling
how things should be.

They are defined like so:
```
public @interface SimpleAnnotation {
  String name();
  int order() default 0;
}
```

They can then be used lke so:
```
@SimpleAnnotation(name="foo")
```

Several useful points:
- `@Retention(RetentionPolicy.<CLASS|RUNTIME|SOURCE>)`
- `@Target( {ElementType.[CONSTRUCTOR, FIELD, METHOD, etc]} )`
- `@Inherited`: forces subclasses to inherit the annotation

# Writing Methods Efficiently
Most of this stuff was already known by me.

Some newish stuff:
- Covariant return types: when `@Override`ing a parent's method you can return
  a narrower type. For instance if your parent returned `Animal` you can return
  `Dog`.
- You can have one generic method and overload other concrete implementations.
- You can pass a method around using `Object::methodName` -- agh, why???
- Immutability is not well supported but recommended anyway.
- Method documentation is in it's own language man. (not html, not markdown,
  yay?)

# Exceptions
There are two classes of exceptions:
- those derived from `Exception`: must be explicitly caught or declared in methods.
- those derived from `RuntimeException`: can be uncaught/undeclard in methods.

More points:
- It is common practice for all exceptions to be RuntimeExceptions.
- If a method might throw an exeption use the javadoc line `@throws
  SomeException if thing occurs`.
- If the exception must be caught, use
    `public void myFunction(final int arg) throws Somexception { /* ... */ }`

# Concurency
Every java thread (lightweight process) exists inside the JVM and may not
reflect any operating systme threads.

Threads are an instance of the `Thread` class. It is not recommended to create/manage
threads yourself but it can be done.

```
public static main(String[] args) {
    new Thread( new Runnable() {
        @Override
        public void run() {
            System.out.println("Hello from thread.");
        }
    } ).start()

    new Thread( () -> { System.out.println("Hello from lambda.") } ).start();
}
```

## `synchronized` keyword

Java provides a keyword for synchronizing methods or arbitrary code blocks.
This guarantees that only one thread at a time can execute it.

```
public synchronized vod performAction() {
    // implementation here
}

public void performActionBlock() {
    synchronized(this) {
        // Implementation here
    }
}
```

This is intended to provide _automatic concurrency management_. Basically
`this._lock.lock()` is called before the code block is enetered and `unlock()`
is called on exit. This is great, except that is a _lot_ of implicit
locking/unlocking. I can't imagine trying to analyze java code for where
deadlocks might be happening.

## Executors
These are basically threadpools. The basic idea is that they are intended for
_short lived threads_. It is also probably a bad idea for there to be resource
sharing between threads in the executor (although this wasn't mentioned).

The state of the thread can be in one of these states:
- NEW: thread not started
- RUNNABLE: the thread is running
- BLOCKED: waiting on a monitor ONLY.
- WAITING/TIMED_WAITING: waiting for another thread's resource.
- TERMINATED: thread complete.

```
ExecutorService executor = Executors.newFixedThreadPool( 10 );

executor.execute( () -> {
  // Implementation here
})

Future< Long > result = executor.submit( new Callable<Long>() {
    @Override
    public Long call throws Exception {
        return 42;
    }
} );
```


## Completeable Future
There is a completeable future class with associated methods:

```
final Collections<String> strings = new ArrayList<>();

final int sumOfLengths = strings.parallelStream()
    .filter( str -> !str.isEmpty())
    .mapToInt( str -> str.length() )
    .sum();
```




