// functionality depending on associated types may require applying bounds

// fn apply_bounds<C>(c: C) where
//     C: Contains,
//     C::A Display { ... }

use std::fmt::Display;

// container that can take any type
struct Container<T>(T);

// Trait that returns whatever is inside
trait Contains {
    type A;
    // return the inner element
    fn inner(&self) -> Self::A;
}

impl<T:Clone> Contains for Container<T> {
    type A = T;
    // clone to prevent move
    fn inner(&self) -> Self::A { self.0.clone() }
}

// Bounds on associated type
fn printer<C>(c: C) where
        C: Contains,
        C::A: Display {
    println!("{} is in container", c.inner());
}

// the assignment shorthand
fn num_small<C>(c: C) -> i32 where
        C: Contains<A = i32> {
    c.inner()
}

pub fn function() {
    println!("# learn constrain");
    printer(Container(12799));
    println!("I got: {}", num_small(Container(4123)));
}
