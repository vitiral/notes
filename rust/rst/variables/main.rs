
fn print_number(x: i32) {
    println!("x is: {}", x);
}

fn add_one(x: i32) -> i32 {
    x + 1
}

fn fails() -> ! {  // ! as a type specifies that the function never returns
    panic!("something failed!");
}


fn main() {
    let x = 5;   // variable definition
    let (y, z) = (1, 2);  // using "patterns"
    let n: i32 = 5;  // specifying types

    // immutability
    //x = 7; // WILL NOT WORK
    let mut mx = 5;
    mx = 9; 

    println!("Printing..."); // "function!" is a macro
    println!("x={}", x);

    // initialization
    let notinit: i32;  // so we can't use this, and also can't mutate it... seems stupid
    //println!("invalid={}", notinit);  // ERROR
    
    // functions
    print_number(y);
    println!("added one: {}", add_one(y));

    //fails();  // panics
    //let x: i32 = fails();
    
    // function pointers
    let f: fn(i32) -> i32 = add_one;  // this syntax is WAY better than C!
    let f2 = add_one;
    assert_eq!(f(2), 3);
    assert_eq!(f2(2), 3);

    
    // char are runes (unicode)
    // arrays are fixed-width elements of the same type
    //let s = 'not a string?';  // cannot use '' for strings (only "") 
    let a = [1,2,3];
    println!("array-0: {}", a[0]);
    //println!("array: {}", a.to_string());  // still can't print arrays...
    let s = &a[1..]; // slice of array
    println!("slice-0: {}", s[0]);

    // tuples
    let t = (1, "hello");  // note the different types
    let t: (i32, &str) = (1, "hello");  // note the different types
    println!("tuple-0, 1: {}, {}", t.0, t.1);
    //println!("tuple: {}", tuple);  // can't print tuples
    
    // if statements
    if x == 5 {
        // always
    } else {
        // never
    }
    // cooler. At first glance kind of complex, but simple
    let x2 = if x == 5 {10} else {0};
    assert_eq!(x2, 10);

    println!("array: {:?}", a);
}
