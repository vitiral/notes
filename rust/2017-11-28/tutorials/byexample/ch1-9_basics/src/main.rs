#![allow(dead_code)]
use std::fmt;
use std::ops::{Add, Mul, Sub};

// # Learn formatting
#[derive(Debug)]
struct List(Vec<i32>);

impl fmt::Display for List {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let List(ref vec) = *self;  // destructure List into Vec
        try!(write!(f, "[")); // try is a macro that bubbles Err up if it occurs
        for (count, v) in vec.iter().enumerate() {
            if count != 0 {try!(write!(f, ", "));}
            try!(write!(f, "{}", v));
        }
        write!(f, "]")
    }
}

fn learn_formatting() {
    println!("# learn formatting");
    let v = List(vec![1,2,3]);
    println!("Debug: {:?}", v);
    println!("Display: {}", v);
}

// # Learn arrays + slices
fn analyze_slice(slice: &[i32]) {
    println!("first element is: {}, len={}", slice[0], slice.len());
}

fn learn_arrays() {
    println!("# learn arrays");
    let xs: [i32; 5] = [1,2,3,4,5];
    let ys: [i32; 10] = [500; 10];  // makes slice of [500, 500, 500, ...]
    println!("regular array: {:?}", xs);
    println!("500 slice: {:?}", ys);
    analyze_slice(&ys);  // analyze the slice
    analyze_slice(&xs);  // borrow the array as a slice
    analyze_slice(&xs[1..3]);  // borrow a slice of the array
}

// # structs and de-struct-uring  (haha, just got that pun)

#[derive(Debug)]
struct Point {
    x: f64,
    y: f64,
}

fn learn_structs() {
    println!("# learn structs");
    let p1 = Point{x: 1.0, y: 2.3};
    println!("point: {:?}", p1);
    let Point{x: x1, y: y1} = p1;
    println!("x, y: {}, {}", x1, y1)
}

// # enums

#[derive(Debug)]
enum Person {
    // can be "unit" like. Each of these is (), but specify a different type
    Skinny,
    Fat,
    // like a tuple struct
    Height(i32),
    Weight(i32),
    // or like structures
    Info {name: String, height: i32},
}

// c like enums
#[derive(Debug)]
enum Number {
    Zero,
    One,
    Two,
    Three,
}

#[derive(Debug)]
enum Color {
    Red = 0xff0000,
    Green = 0x00ff00,
    Blue = 0x0000ff,
}

fn inspect(p: Person) {
    match p {
        Person::Skinny => println!("they are skinny!"),
        Person::Height(h) => println!("height is {}", h),
        Person::Info{..} => println!("info {:?}", p),  // for single items use "_", for multiple use ".."
        _ => println!("I don't know...'")
    }
}

fn learn_enums() {
    use Number::*;
    use Color::*;

    println!("# learn enums");
    let slim = Person::Skinny;
    inspect(slim);
    let tall = Person::Height(42);
    inspect(tall);
    let bob = Person::Info{name: "bob".to_string(), height: 42};
    inspect(bob);
    let unknown = Person::Weight(22);
    inspect(unknown);  // not implemented on purpose

    println!("Here are some fun numbers: {:?}={}, {:?}={}, {:?}={}",
             Zero, Zero as i32, One, One as i32, Two, Two as i32);
    println!("Here are some fun colors: {:?}={:06X}, {:?}={:06X}, {:?}={:06X}",
             Red, Red as i32, Green, Green as i32, Blue, Blue as i32);
}

// # use: easier scoping
enum Status {
    Rich,
    Poor,
}

enum Work {
    Civilian,
    Soldier,
}


fn learn_use() {
    use std::io::Write; // necessary to use stdout().write (kind weird...)
    use Status::{Poor, Rich};  // no need to type Status::Poor in code
    use Work::*; // shorthand for the above

    println!("# learn use");
    let status = Poor;
    let work = Civilian;

    // These prints could have just been done with println! done 4 times,
    // but I wanted a challenge or something
    std::io::stdout().write(match status {
        Rich => "yay! rich",
        Poor => "boo, poor",
    }.as_ref()).unwrap();
    std::io::stdout().write("\n".as_ref()).unwrap();
    std::io::stdout().write(match work {
        Civilian => "civy",
        Soldier => "dat fightin spirit",
    }.as_ref()).unwrap();
    std::io::stdout().write("\n".as_ref()).unwrap();
}

// # enum linked list
// Can't seem to get this thing to work: http://rustbyexample.com/custom_types/enum/testcase_linked_list.html
// enum Linked {
//     Cons(u32, Box<List>),
//     Nil,
// }
// impl Linked {
//     fn new() -> Linked {
//         Nil  // Nil has type Linked
//     }
//     fn prepend(self, elem: u32) -> Linked {
//         Cons(elem, Box::new(self))
//     }
// }
// fn learn_linked() {
// }

// # casting
fn learn_casting() {
    println!("# learn casting");
    let decimal = 64.234_124; // notice the "_" to format the number better
    // let integer = decimal; // error! not allowed
    // let character = decimal as char;  // error: only `u8` can be cast as `char`, not `f64`
    let int8 = decimal as u8;
    let character = int8 as char;
    // OR
    let character2 = (decimal as u8) as char;
    assert_eq!(character, character2);
    println!("Casting {} -> {} -> {}", decimal, int8, character);
}

// # type aliasing
type NanoSecond = u64;
type Inch = u64;

// without this attribute, you can't use lower case letter for types
#[allow(non_camel_case_types)]
type u64_t = u64;

fn learn_aliasing() {
    println!("# learn aliasing");
    let nanoseconds: NanoSecond = 5;
    let inches: Inch = 2 as u64_t;
    println!("{} nanoseconds + {} inches = {} (unit?)", nanoseconds, inches, nanoseconds + inches);
    // note that the aliases didn't provide type safety because aliases are NOT new types
    // (isn't 'type' a bad name for them then? haha)
    // -- main use is to reduce typing, for example IoResult<T> is an alias for Result<T, IoError>
}

// # nesting and labels

fn learn_nesting() {
    println!("# learn nesting");
    let mut looped = 0;
    'outer: loop {
        println!("looping outer");
        'inner: loop {
            looped += 1;
            println!("looping inner");
            if looped > 5 {
                println!("Breaking out of outer");
                break 'outer;
            } else if looped > 3 {
                println!("Breaking out of inner");
                break 'inner;
            }
        }
    }
    
}

// # match
fn learn_match() {
    println!("# learn matching");
    let num_type = |n| {
        match n {
            1 => "one",
            2 | 3 | 5 | 7 | 11 => "this is a prime",
            _ => "Aint special",
        }
    };
    for i in 0..14 {
        println!("{}", num_type(i));
    }
}

// # pointers/ref
fn learn_ref() {
    println!("# learn pointers ref");
    let reference = &4; // get a reference of 4
    match *reference {  // you can dereference before matching
        val => println!("Got a value: {}", val),
    }

    let value = 5;  // not a reference
    let ref _ref_value = 5; // same as let _ref_value = &5;
    let mut mut_value = 6;

    match value {
        ref r => println!("Got a reference to: {:?}", r),
    }

    match mut_value {
        ref mut m => {
            // got a reference, you can now modify it in place
            *m += 10;
            println!("We've added 10 to mut_value: {:?}", m);
            assert_eq!(*m, 16);
        },
    }
}

fn age(i: i32) -> i32 {
    i
}

// # bindings (the @ symbol)
fn learn_binding() {
    println!("# learn binding");
    for i in 11..20 {
        match age(i) { // 
            0 => println!("I'm not born yet"),
            // bind a to the age
            a @ 1...12 => println!("I'm a child of age {}", a),
            a @ 13...19 => println!("I'm an teen of age {}", a),
            a => println!("I'm an adult of age {}", a),
        }
    }
}

// # if let -- the single use case of match
// note that `while let` also exists
fn learn_iflet() {
    println!("# learn iflet");
    let number = Some(7);
    let nonumber: Option<i32> = None;
    if let Some(i) = number {
        println!("i is a number: {}", i);
    }

    if let Some(n) = nonumber {
        println!("what te heck n: {}", n);
        panic!();
    } else {
        println!("nonumber isn't a number");
    }

    // allows you to chain else-ifs
    if let Some(n) = nonumber {
        println!("what te heck n: {}", n);
        panic!();
    } else if false {
        panic!(); // could put something in here...
    } else {
        println!("final branch-down");
    }
}

// # methods

impl Point {
    // static method
    fn origin() -> Point {
        Point { x: 0.0, y: 0.0}
    }

    fn new(x: f64, y: f64) -> Point {
        Point{x: x, y: y}
    }
}

impl fmt::Display for Point{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "({}, {})", self.x, self.y)
    }
}

struct Rectangle {
    p1: Point,
    p2: Point,
}

impl Rectangle{
    fn new(p1: Point, p2: Point) -> Rectangle {
        Rectangle{p1: p1, p2: p2}
    }

    fn area(&self) -> f64 {
        let s = self;
        ((s.p1.x - s.p2.x) * (s.p1.y - s.p2.y)).abs()
    }

    fn perimeter(&self ) -> f64 {
        let s = self;
        2.0 * ((s.p1.x - s.p2.x).abs() + (s.p1.y - s.p2.y).abs())
    }

    fn translate(&mut self, x: f64, y: f64) {
        self.p1.x += x;
        self.p2.x += x;
        self.p1.y += y;
        self.p2.y += y;
    }
}

impl fmt::Display for Rectangle {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Rectangle<{}, {}>[area={}, perimeter={}]",
               self.p1, self.p2, self.area(), self.perimeter())
    }
}

fn learn_method() {
    println!("# learn method");
    let rec = Rectangle::new(Point::origin(), Point::new(3.0, 4.0));
    assert_eq!(rec.area(), 12.0);
    assert_eq!(rec.perimeter(), 14.0);
    println!("{}", rec);
}

// # closures
fn learn_closures() {
    use std::mem;
    println!("# learn closures");
    fn function (i: i32) -> i32 {i + 1} // can define functions inside
    let closure_annotated = |i: i32| -> i32 {i + 1};
    let closure_inferred = |i| i + 1;

    let i = 1;
    assert_eq!(2, function(i));
    assert_eq!(2, closure_annotated(i));
    assert_eq!(2, closure_inferred(i));
    let one = || 1;
    assert_eq!(1, one());

    let professor_x = "Charles Xavier";
    let print_x = || println!("Professor X's name is: {}", professor_x); // notice no move!
    print_x();

    // lesson 2 -- closures automatically do whatever borrowing they need to do
    // They sometimes move and sometimes borrow
    let color = "green";
    // this will borrow color 
    let print_color = || println!("color: {}", color);
    print_color();
    print_color();

    let mut count = 0;
    { // scope is required to reference it after the borrows
        // mut is required on inc because it stores &mut inside of it.
        // thus calling the closure mutates it.
        let mut inc = || count += 1;
        inc();
        inc();
    }
    assert_eq!(count, 2);

    // non copy-able type
    let movable = Box::new(5);
    let consume = || {
        println!("movable: {:?}", movable);
        mem::drop(movable);
    };

    // consume consumes the variable so can only be called once
    consume();
    // consume(); // can't do this

    let moveable = Box::new(7);
    let moved = move || {
        println!("all variables moved! movable={:?}, count={}, color={}",
                 moveable, count, color);
    };
    moved();
    // println!("moveable={:?}", moveable); // invalid, was moved by `move` keyword
    // Apparently "moving" is the same thing as "capturing by value"
}

// # learn closure inputs -- closures as input parameters
// Functions can take closures as inputs, however the *captre method* must be
// defined.
// - `Fn` takes capture by &T
// - `FnMut` takes capture by &mut T
// - `FnOnce` takes capture by value T -- the moved value!
// it is important to note that the machinery of closures requires type anonymity:
// a struct has to be created to store the local variables of the enclosed scope!
//
// Also, any function which can take a closure as an argument can also take a function,
// since functions *never* capture variables (Fn is more than sufficient always)

// function that takes a closure which takes no parameters and may have
// to move the variables
fn apply<F>(f: F) where F: FnOnce() {
    f()
}

fn apply_mut<F>(mut f: F) where F: FnMut() {
    f()
}

// function that takes a closure which takes an i32 and returns an i32
fn apply_to_3<F>(f: F) -> i32 where
    F: Fn(i32) -> i32 {
    f(3)
}

fn learn_closure_inputs() {
    use std::mem;
    println!("# learn closure inputs");
    let greeting = "hello";
    let mut farewell = "goodbye".to_owned();

    // capture two variables, greeting by reference and farewell by value
    let diary = || {
        // `greeting` is by reference, requres Fn
        println!("Entry1: I said {}", greeting);
        // mutation requires `farewell` to be captured by mutable
        // reference, now requires `FnMut`
        farewell.push_str("!!!");
        println!("Then I screamed {}", farewell);
        println!("Now I can sleep. zzzzz.");

        // manually dropping forces `farewell` to be captured by value
        // now requires `FnOnce`
        mem::drop(farewell);
    };


    apply(diary);

    let mut thoughts = "Things were good".to_owned();
    // I would have thought it would be mut diary2, but that might be ONLY if you
    // move the values and then mutate them
    let diary2 = || { 
        println!("Entry2: Don't have a lot to say");
        thoughts.push_str(", but the weather sucked.");
        println!("These are my thoughts: {}", thoughts);
    };
    apply_mut(diary2);

    let double = |x| 2 * x;
    println!("3 doubled is {}", apply_to_3(double));
}


// # Closures as outputs
// - returning closures could be problematic because rust only supports returning concrete
//    (non-generic) types. Annonymous closure types are by definition unknown, so we
//    must make it concrete. This can be done via boxing.
// Valid traits for returns are Fn, FnMut (as  before), and also FnOnce -> FnBox
//    (which is unstable). In the future FnBox is expected to change.
// Other than that, the closure must be Boxed and moved, like so

fn create_fn() -> Box<Fn()> {
    let text = "Fn".to_owned();
    Box::new(move || println!("This is a: {}", text))
}

fn create_fnmut() -> Box<FnMut()> {
    let mut text = "FnMut".to_owned();

    Box::new(move || {
        text.push_str("... And I changed it!");
        println!("This is a: {}", text);
    })
}


fn learn_closure_outputs() {
    println!("# learn closure outputs");
    let fn_plain = create_fn();
    let mut fn_mut = create_fnmut();

    fn_plain();
    fn_mut();
}

// # Closures as outputs
fn learn_closure_stdlib() {
    println!("# learn closure stdlib");
    println!("iterator::any");
    let vec1 = vec![1,2,3];
    let vec2 = vec![4,5,6];
    // iter yeilds &, so destructuring is required
    println!("2 in vec1: {}", vec1.iter().any(|&x| x == 2));
    // into_iter yeilds the values, so no destructuring
    println!("2 in vec2: {}", vec2.into_iter().any(|x| x == 2));

    let array1 = [1,2,3];
    // for arrays, into_iter also yeilds reference (for some reason?)
    // note to self: only use .iter() -- what is the point of into_iter
    println!("2 in array1: {}", array1.into_iter().any(|&x| x == 2));

    // iterator::any
    let vec1 = vec![1,2,3];
    let vec2 = vec![4,5,6];
    let mut iter = vec1.iter();  // yields &i32
    let mut into_iter = vec2.into_iter(); // yields i32
    println!("Find 2 in vec1: {}", iter.find(|&&x| x == 2).unwrap());
    assert!(into_iter.find(|&x| x == 2).is_none());
}


// # higher order functions

fn is_odd(n: u32) -> bool {
    n % 2 == 1
}

fn learn_higher_order_functions() {
    println!("# learn higher order functions");
    let upper = 1000;
    println!("finding sum of all odd square numbers under {}", upper);
    println!("imperative approach:");
    let mut acc = 0;
    for n in 0.. { // iterate from 0 to infinity
        let n_squared = n * n;
        if n_squared >= upper {
            break;
        } else if is_odd(n_squared) {
            acc += n_squared;
        }
    }
    println!("Imperative answer: {}", acc);

    // Functional approach
    let sum_of_squared_odd: u32 =
        (0..).map(|n| n * n)
        .take_while(|&n| n < upper)
        .filter(|&n| is_odd(n))
        .fold(0, |sum, i| sum + i);
    println!("Functional syle: {}", sum_of_squared_odd);
}

// # macro rules

macro_rules! say_hello {
    // `()` indicates the macro takes no argument
    () => (
        // the macro expands into the contents of this block
        println!("Hello from macro!");
    )
}

macro_rules! create_function {
    // the arguments of a macro are prefixed with $ and type annoted
    // with a designator
    // This macro takes 
    ($func_name:ident) => (
        fn $func_name() {
            // the `stringify` macro converts an ident into a string
            println!("You called {}()", stringify!($func_name))
        }
    )
}

create_function!(macrod_foo);
create_function!(macrod_bar);

macro_rules! print_result {
    // this macro will take an expression of type `expr` and prints
    // it as a string along with it's result.
    // the `expr` designator is used for expressions
    ($expression:expr) => (
        println!("The expression {:?} == {:?}",
                 stringify!($expression),
                 $expression)
    )
}

// # Writing tests using macros

// overloading macros and defining your own syntax within them
macro_rules! print_cmp {
    // Arguments don't need to be separated by coma... any template can
    // be used.
    ($left:expr; and $right:expr) => (
        println!("{:?} and {:?} is {:?}",
                 stringify!($left),
                 stringify!($right),
                 $left && $right)
    );
    ($left:expr; or $right:expr) => (
        println!("{:?} or {:?} is {:?}",
                 stringify!($left),
                 stringify!($right),
                 $left || $right)
    );
}

macro_rules! find_min {
    // base case
    ($x:expr) => ($x);
    // x followed by at least 1 y
    ($x:expr, $($y:expr), +) => (
        // call `find_min` on the tail `$y`
        std::cmp::min($x, find_min!($($y),+))
    )
}

macro_rules! assert_equal_len {
    // the `tt` (token tree) designator is used for operators and tokens
    ($a:ident, $b:ident, $func:ident, $op:tt) => (
        assert!($a.len() == $b.len(),
                "{:?}: dimension mismatch: {:?} {:?} {:?}",
                stringify!($func),
                ($a.len(),),
                stringify!($op),
                ($b.len(),));
    )
}

macro_rules! op {
    ($func:ident, $bound:ident, $op:tt, $method:ident) => (
        fn $func<T: $bound<T, Output=T> + Copy>(xs: &mut Vec<T>, ys: &Vec<T>) {
            assert_equal_len!(xs, ys, $func, $op);
            for (x, y) in xs.iter_mut().zip(ys.iter()) {
                *x = $bound::$method(*x, *y);
            }
        }
    )
}


op!(add_assign, Add, +=, add);
op!(mul_assign, Mul, *=, mul);
op!(sub_assign, Sub, -=, sub);

mod test {
    use std::iter;
    macro_rules! create_test {
        ($func: ident, $x: expr, $y:expr, $z:expr) => {
            #[test]
            fn $func() {
                for size in 0usize..10 {
                    let mut x: Vec<_>= iter::repeat($x).take(size).collect();
                    let mut y: Vec<_>= iter::repeat($y).take(size).collect();
                    let mut z: Vec<_>= iter::repeat($z).take(size).collect();

                    super::$func(&mut x, &y);
                    assert_eq!(x, z);
                }
            }
        }
    }

    create_test!(add_assign, 1u32, 2u32, 3u32);
    create_test!(mul_assign, 2u32, 3u32, 6u32);
    create_test!(sub_assign, 3u32, 2u32, 1u32);
}


fn learn_macro_rules() {
    println!("# learn macros");
    say_hello!();
    macrod_foo();
    macrod_bar();
    // recall that blocks are expressions too
    print_result!({
        let x = 1u32;
        x * x + 2 * x - 1
    });
    print_cmp!(1i32 + 1 == 2i32; and 2i32 + 2 == 4i32);
    print_cmp!(true; or false);
    println!("min is: {}", find_min!(1u32));
    println!("min is: {}", find_min!(1u32 + 2, 2u32));
    println!("min is: {}", find_min!(5u32, 2u32 * 3, 4u32));
}


fn main() {
    println!("Hello, world!");
    learn_formatting();
    learn_arrays();
    learn_structs();
    learn_enums();
    learn_use();
    learn_casting();
    learn_aliasing();
    learn_nesting();
    learn_match();
    learn_ref();
    learn_binding();
    learn_iflet();
    learn_method();
    learn_closures();
    learn_closure_inputs();
    learn_closure_outputs();
    learn_closure_stdlib();
    learn_higher_order_functions();
    learn_macro_rules();
}
