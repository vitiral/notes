#![allow(dead_code)]

use std::fmt;

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
}
