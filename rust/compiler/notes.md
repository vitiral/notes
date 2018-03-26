
# How to run:
@vitiral for testing -Zdump-dep-graph I would not globally set RUSTFLAGS="-Zdump-dep-grah" as this will cause the flag to be set for any compiler invocation from that terminal
Michael Woerister
@michaelwoerister
03:48
@vitiral I usually use a workflow like the following for something like this:

    set up a custom rustup toolchain that points to my locally built compiler (you only have to do this once)
    write a small Rust program that I want to dump the dep-graph for
    run rustup override add my-local-rustc-toolchain in the directory of my sample program
    run rustc ./my-test-program.rs -Zincremental=/tmp/abcdef -Zdump-dep-graph or, if you are using cargo, CARGO_INCREMENTAL=1 cargo rustc -- -Zdump-dep-graph

@vitiral the rustup part is optional but you only have to set it up once and it allows you to use cargo and quickly switch between local, nightly, stable, and other toolchains. It's awesome :smile:
@vitiral of course, even with rustup you have to recompile your local compiler after every change you make to it. Usually running x.py build --stage 1 src/libtest is sufficient though (unless you are doing something with procedural macros or plugins).

# 2017-10-24 work items

@vitiral You can get a core dump for Rust programs like for any other native
program (you might have to run ulimit -c unlimited) @vitiral But your mileage
my vary on how far you get inspecting that in a debugger

@vitiral Usually running with RUST_BACKTRACE=1 will give you a good idea of
what's wrong


@michaelwoerister @vitiral There are two things to note though: libbacktrace
sometimes is very slow and it can take a minute for it to actually produce the
backtrace you have to compile the compiler with debug symbols enabled to get
quality backtraces.

@vitiral I usually have the following settings in my
config.toml:

    codegen-units = 16
    debug-assertions = true
    debuginfo-lines = true

@vitiral Then you can just run RUST_BACKTRACE=1 ./x.py test --stage 1 src/test/incremental
Michael Woerister
@michaelwoerister
03:20

@vitiral Regarding the trait ICE in the test framework: I would really try just
wrapping the force_from_dep_node call in an if !dep_node.is_input() { ... }.
That might solve the whole problem.  @vitiral Regarding your work with
graphviz: It would be great if you were interested in improving our graphviz
output. dot often squashes the graphs into a hard to read layout and I bet we
could do better here (especially because we've gotten rid of cycles in the
graph meanwhile).


@michaelwoerister is there a doc on how I can export the graph as it exists today, and generally feature issues around it?
I'll give all of that a try sometime this week. Would like to get those tests fully functional as well
Michael Woerister
@michaelwoerister
08:39
@vitiral the existing documentation is here: https://github.com/rust-lang/rust/blob/master/src/librustc/dep_graph/README.md#debugging-the-dependency-graph
@vitiral without filtering, the graph of even tiny programs is too much for graphviz, I think
@vitiral I don't think anyone has opened github issues yet though, right @nikomatsakis?


# Older Stuff
nodes:
src/librustc/hir/map/mod.rs#L44-L68 (`enum Node`)
 I'll take call_expressions.rs as part of the next stage

I'm also taking:

- [x] consts.rs
- [x] enum_constructors.rs
- [x] inherent_impls.rs
- [x] statics.rs
- [x] struct_constructors.rs
- BLOCKED trait_defs.rs
- BLOCKED trait_impls.rs
- [x] type_defs.rs
- [ ] enum_defs.rs
- [ ] extern_mods.rs

# TODO
- extern_mods.rs

## Copy/Pasted from main issue

### Free-standing Functions and Methods

These represent executable code, so we want to test their MIR:

    MirValidated
    MirOptimized

Callers will depend on the signature of these items, so we better test

    TypeOfItem,
    GenericsOfItem,
    PredicatesOfItem, and
    FnSignature.

And a big part of compilation (that we eventually want to cache) is type inference information:

    TypeckTables

### For methods, we can also check

    AssociatedItems

which is a bit misnamed and actually describes the ty::AssociatedItem descriptor of the method.

### Struct, Enum, and Union Definitions

For these we should at least test

    TypeOfItem,
    GenericsOfItem, and
    PredicatesOfItem.

in addition to Hir and HirBody. Note that changing the type of a
field does not change the type of the struct or enum, but adding/removing
fields or changing a fields name or visibility does.
### Struct/Enum/Unions Fields

Fields are kind of separate from their containers, as they can change independently from them. We should at least check

    TypeOfItem for these.

### Trait Definitions

For these we'll want to check

    TraitDefOfItem,
    TraitImpls,
    SpecializationGraph,
    ObjectSafety,
    AssociatedItemDefIds,
    GenericsOfItem, and
    PredicatesOfItem

### (Trait) Impls

For impls we'll want to check

    ImplTraitRef,
    AssociatedItemDefIds, and
    GenericsOfItem.

### Associated Items

For associated items (types, constants, and methods) we should check

    TraitOfItem,
    AssociatedItems.

