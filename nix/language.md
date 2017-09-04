First I gotta say boom:

> nix-install-package — install a Nix Package file

```
$ nix-install-package --non-interactive -p /nix/var/nix/profiles/eelco \
    --url http://nix.cs.uu.nl/dist/nix/nixpkgs-0.10pre6622/pkgs/subversion-1.4.0-i686-linux.nixpkg
```

# Nix build language

The nix build langauge is a:
- functional
- statically typed (?)
    - might not be static... seeing some things like `null` values and the
      `?` operator that says otherwise.
    ```
    e ? attrpath	none	Test whether set e contains the attribute denoted by attrpath; return true or false.
    ```
- specific purpose: just for defining builds. NOT general purpose.

Overall I'm pretty happy with the syntax and design philosphy. First of all, it's
pretty clear that a **full language is necessary** to create even remotely
dynamic build scripts. Simple JSON files were never going to be enough, and
would have resulted in horrific abuses of specific keys to create logic instead
of represent data.



## Types
- string: these have some pretty cool ideas actually, like that you can have
  ANY expression inside them with `${}` and you don't have to do special escaping.
    - "this is a string"
    - "this uses ${foo} **expression**"
    - "expressions ${"can" + " be complex"}"
- paths: these are basically strings but I *think* `++` is basically `path.join`
  paths can be encased in `""`, or just left plain (but must have a `/` in them).
- integer: pretty self explanatory
- boolean: true or false
- null: boooooo... kinda. It's fine for this application I think...

### Lists
```
[ 123 ./foo.nix "abc" (f { x = y; }) ]
```

The last one is calling the function `f` setting variable `x` to `y`.

### Sets
```
{ x = 123;
  text = "Hello";
  y = f { bla = 456 };
}
```

Access a single attribute with `.` operator
```
y = foo.bar
z = foo."bar"
h = foo.${bar}
```

Sets can also be recursive using the `rec` keyword, where attributes refer to
eachother. Why this exists...  ??? I have no idea.

```
assert rec {
  x = y;
  y = 123;
}.x == 123
```

### let expression

works the same as elm
let
  x = "foo";
  y = "bar";
in x + y;

### inherit

`inherit foo;` is fancy for `foo = foo;` in a set.

Inherit from another set: `inherit (other) attribute`

### Functions

`pattern: body`

Curried inputs:

```
let concat = x: y: x + y
in assert concat "foo" "bar" == "foobar"
```

Provide default values with `var ? default`

`args@{ x, y, z, ...}`: `args` lets you pull keys out of the `...`

### Assertions

`assert e1; e2`: e2 is returned if e1 is true, else an error is raised w/stacktrace.

Makes heavy use of logical implication `e1 -> e2`, (equivalent to `!e1 || e2`).

### With-expression

`with e1; e2` is basically `from e1 import *` for e2.

This can be used for imports to be an equivalent of bash `source`

### Derivations
The most important built-in function, which describes a single derevation (a build action).
- Takes as input a set, the attributes specify the inputs of the build. All attributes
  are passed as an *environment variable* (yuck? let's see...)
    - `system` is a string like "i686-linux"
    - `name` is a string for the name used by `nix-env`
    - any paths are copied to the store and converted to that full path location.
    - other derivations will be built before this derevation, and their value
      converted to their resulting full path (fucking brilliant).
    - lists are allowed but are concatenated (??) and separated by spaces (??)
    - true is passed as the string `"1"`, false and null are passed as the empty
      string `""`
