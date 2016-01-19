use std::fmt::Debug;

mod where_clause;
mod associated;
mod constrain;
mod add;
mod phantom;

// Basics
// concrete type A, value is always ()
struct A;

// in defining Single the first use of A is not preceded by <A>,
// therefore Single is concrete
struct Single(A);

// here <T> preceeds the first use of `T`, so `SingleGen` is generic
// Here `T` could be literally anything (any struct? -- can it be a function?)
struct SingleGen<T>(T);

fn learn_basics() {
    println!("Learning basics");

    // single is a concrete ype and explicityl takes `A`.
    // essentially it HAS to take the empty tuple type A. Seems really odd to me...
    let _s = Single(A);

    // type can be explicit
    let _char: SingleGen<char> = SingleGen('a');
    // or inferred
    let _char = SingleGen('a');
    let _t = SingleGen(A);
    let _i32 = SingleGen(42);
}

// Generic functions
struct T;       // concrete type
struct S(T);    // concrete type
struct SGen<T>(T); // Generic type

// these take ownership of the variable passed into them and then
// immediately go out of scope, freeing them.
// regular function
fn die_regular(s: S) {}

// has a <T> but it isn't preceeded by a <T> to make it generic.
// This is a regular function which takes SGen<T> which has been
// specialized to type `T` defined above (kind of confusing)
// A more explicit example might be to use SGen<i32> or something
fn die_generic_specialized_t(s: SGen<T>) {}

// regular funciton taking SGen<T> specialized to i32
fn die_generic_specialized_i32(s: SGen<i32>) {}

// <T> is preceeded by <T>. This function is generic over <T>
fn die_gen_generic<T>(s: SGen<T>) {}

// truly generic -- NOT POSSIBLE
// fn die_generic<T>(s: <T>) {}

fn learn_generic_functions() {
    die_regular(S(T));
    die_generic_specialized_t(SGen(T));
    die_generic_specialized_i32(SGen(5));

    // explicity specify generic functions
    die_gen_generic::<char>(SGen('a'));
    // implicity specify
    die_gen_generic(SGen('a'));
}


// # Implementation
struct N; // a null struct
struct GenericVal<T>(T);

// impl of Generic val we specifically specialize
impl GenericVal<f32> {} // specialize to `f32`
impl GenericVal<N> {}   // specialize to `N` defined above

// <T> must preceed the type to remain generic
impl<T> GenericVal<T> {}

struct Val(f64);
struct GenVal<T>(T);

// impl of Val
impl Val {
    fn value(&self) -> &f64 {
        &self.0
    }
}

impl<T> GenVal<T> {
    fn value(&self) -> &T {
        &self.0
    }
}


fn learn_implementation() {
    let x = Val(3.0);
    let y = GenVal(3i32);
    println!("Values: {}, {}", x.value(), y.value());
}

// # Bounds
// when working with generics, you can use bounds to specify what kind
// of functionality the type needs to implement.
// This allows generic instances to access the methods defined by the
// specified traits

trait HasArea {
    fn area(&self) -> f64;
}

impl HasArea for Rectangle {
    fn area(&self) -> f64 {
        self.length * self.height
    }
}

#[derive(Debug)]
struct Rectangle {
    length: f64,
    height: f64,
}

#[derive(Debug)]
struct Triangle {
    length: f64,
    height: f64,
}

fn print_debug<T: Debug>(t: &T) {
    println!("{:?}", t);
}

fn area<T: HasArea>(t: &T) -> f64 {
    t.area()
}

trait EmptyBounds {
// nothing
}

impl EmptyBounds for Rectangle {
    // nohing
}

// also implements empty bounds
fn empty_bounds<T: EmptyBounds + Debug>(t: T) {
    println!("gave me something with no bounds: {:?}", t);
}

fn learn_bounds() {
    let rectangle = Rectangle {
        length: 3.0,
        height: 4.0,
    };
    let triangle = Triangle {
        length: 3.0,
        height: 4.0,
    };
    print_debug(&rectangle);
    print_debug(&triangle);
    println!("Area: {}", area(&rectangle));
}

fn main() {
    learn_basics();
    learn_generic_functions();
    learn_implementation();
    learn_bounds();
    where_clause::function();
    associated::function();
    associated::function2();
    constrain::function();
    add::function();
    phantom::function();
}
