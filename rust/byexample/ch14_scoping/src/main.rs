

// Ownership and moves
// This really highlights the differences between passing values by value and reference.
// When you pass by value you pass OWNERSHIP. That is the most important point.
// By passing ownership, it means that the variable gets destroyed when ownership ends.
// (unless it implements the Copy trait, in which case it gets copied and a copy gets
// destroyed)
//
// A type (struct) can only implement Copy if it's members implement copy.
// You cannot copy mutable references. You also cannot copy String because it
// would result in two attempts to free the same buffer.
// A good way to think of it is that you can't copy anything that implements Drop
// because it is managing resource behind it's own `size_of::<T>()` bytes

// Resources can only have one owner. When doing assignments `let x = y` or passing
// function arguments by value, the *ownership* of the resource, if any, is transferred.
// This is known as "move" in rust speak.
// After moving the previous owner can no longer use the resource

fn destroy_box(c: Box<i32>) {
    println!("Destorying box: {:?}", c);
}

fn learn_heap() {
    println!("Learn heap");
    let x = 5u32;

    // copy x into y
    let y = x;

    println!("x, y = {}, {}", x, y);

    // a is a pointer to heap allocated memory
    let a = Box::new(5i32);
    println!("a contains: {}", a);

    // move a into b
    // This copies ONLY the address on the heap, NOT the data.
    // now both are pointers to the same heap allocated data,
    // but now b **owns** the data. b is now in charge of freeing
    // the memory and will do so at the end of it's lifetime
    let b = a;

    // a can now no longer be used. This fails:
    // println!("a contains: {}", a);
    // I wonder if the a pointer is even on the heap anymore after
    // optimization...

    // move b into the function. `b` gives up ownership of the heap data
    destroy_box(b);

    // since the heap memory has been freed, this action would result in
    // dereferencing freed memory, but it is forbidden by the compiler
    // println!("b contains: {}", b);
}

fn learn_mutability() {
    println!("# learn mutability");
    let immutable = Box::new(5u32);
    println!("immutable box contains: {}", immutable);

    // mutability error -- notice the automatic dereferencing
    // *immutable += 3

    // hand over the box, changing mutability
    let mut mutable = immutable;

    println!("immutable box contained: {}", mutable);
    *mutable += 4;
    println!("I guess it's mutable, now it is: {}", mutable);
}


// # Learn borrowing

fn eat_box(boxint: Box<i32>) {
    println!("eating box: {}", boxint);
}

fn peepinbox(boxint: &Box<i32>) {
    println!("looking it box it is: {}", boxint);
}

fn learn_borrowing() {
    let boxint = Box::new(5i32);
    peepinbox(&boxint); // ownership is  not taken
    peepinbox(&boxint); // can be borrowed again (ininitely, in fact)

    {
        let refint: &i32 = &boxint; // reference to data INSIDE the box
        // can no longer destroy box int -- a reference exists
        // eat_box(boxint);
        println!("data in box is: {}", refint);
    }
    eat_box(boxint); // give up ownership
}

// # Learn mutably borrowing

#[allow(dead_code)]
#[derive(Debug, Clone, Copy)]
struct Book {
    author: &'static str,
    title: &'static str,
    year: u32,
}

fn borrow_book(book: &Book) {
    println!("I borrowed book: {:?}", book);
}

fn new_edition(book: &mut Book) {
    book.year = 2015; // fields of a mut can be modified
}

fn learn_mut_borrow() {
    let geb = Book {
        author: "James Holmes",
        title: "The bird who ran a mile",
        year: 1437,
    };

    // immutably borrow
    borrow_book(&geb);

    // obviously can't use an immutable as mutable
    // new_edition(&geb);  // throws error

    // we can take ownership mutably
    let mut mutable_geb = geb;

    new_edition(&mut mutable_geb);

    borrow_book(&mutable_geb);
}

// # Freezing
// when data is borrowed it is also FROZEN. Frozen data cannot be
// mutably borrowed until all immutable borrows go out of scope.
// similarily, if data is borrowed as mutable it can no longer
// be borrowed by anything. I like to think of this as floating.
// floating things cannot be claimed by anything until they land,
// landed things can be claimed by anyone, but if they don't
// float they must be frozen!

fn main() {
    learn_heap();
    learn_mutability();
    learn_borrowing();
    learn_mut_borrow();
}
