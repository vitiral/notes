
struct Fib {
    curr: u32,
    next: u32,
}

// implemen `Iterator` for Fib
impl Iterator for Fib {
    type Item = u32;
    // The Iterator trait only requires the `next` method to
    // be defined. Return type must be Option<T>

    fn next(&mut self) -> Option<u32> {
        let next = self.curr + self.next;
        self.curr = self.next;
        self.next = next;

        // `Some` is always returned, even when it wraps.
        Some(self.curr)
    }
}

fn fib() -> Fib {
    Fib {curr: 1, next: 1}
}

pub fn function() {
    println!("# Learning iter");
    let mut sequence = 0..3;
    println!("Four consequtive `next` calls on 0..3");
    println!("> {:?}", sequence.next());
    println!("> {:?}", sequence.next());
    println!("> {:?}", sequence.next());
    println!("> {:?}", sequence.next());

    println!("Iterate over 0..3 using for");
    for i in 0..3 {
        println!("> {:?}", i);
    }

    println!("Use `take(4)` to get first 4 terms of fib");
    for i in fib().take(4) {
        println!("> {:?}", i);
    }

    println!("Use `skip` to drop the first terms");
    println!("Skipping 5 of 0..3 is... nothing");
    for i in (0..3).skip(5).take(10) {
        println!("> {:?}", i);
    }

    println!("Skipping 5 of fib and then taking 4");
    for i in fib().skip(5).take(4) {
        println!("> {:?}", i);
    }

    let array = [1u32, 3, 3, 7];
    // The `iter` method produces an `Iterator` over an array slice
    println!("Iterate the following array: {:?}", array);
    for i in array.iter() {
        println!("> {:?}", i);
    }
}
