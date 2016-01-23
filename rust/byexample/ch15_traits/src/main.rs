
mod operator;
mod bounds;
mod drop;
mod iter;
mod clone;

// A trait is a collection of methods for an unknown datatype, Self.
// Traits can be implemented for ANY datatype

trait Animal {
    // static method signature; Self refers to the implementor type
    fn new(name: &'static str) -> Self;

    // Instance methods, only signatures
    fn name(&self) -> &'static str;
    fn noise(&self) -> &'static str;

    // a trait can also povide default method definitions
    fn talk(&self) {
        // These definitions can access other methods available
        // in the trait. However, it looks like it *cannot*
        // access member variables
        println!("{} says {}", self.name(), self.noise());
    }
}

struct Dog {
    name: &'static str,
}

impl Dog {
    fn wag_tail(&self) {
        println!("{} wags tail", self.name);
    }
}

impl Animal for Dog {
    fn new(name: &'static str) -> Dog {
        Dog { name: name }
    }

    fn name(&self) -> &'static str {
        self.name
    }

    fn noise(&self) -> &'static str {
        "woof!"
    }

    // default trait method can be over-ridden
    fn talk(&self) {
        self.wag_tail();
        println!("{} says {}", self.name(), self.noise());
    }
}

struct Sheep {
    naked: bool,
    name: &'static str,
}

impl Sheep {
    fn is_naked(&self) -> bool {
        self.naked
    }

    fn shear(&mut self) {
        if self.naked {
            println!("{} is already naked!", self.name);
        } else {
            println!("{} gets a haircut", self.name);
            self.talk();
            self.naked = true;
        }
    }
}

impl Animal for Sheep {
    fn new(name: &'static str) -> Sheep {
        Sheep {
            name: name,
            naked: false,
        }
    }

    fn name(&self) -> &'static str {
        self.name
    }

    fn noise(&self) -> &'static str {
        // can also use just self.naked
        if self.is_naked() {
            "baaah"
        } else {
            "baaaaaaaaaah"
        }
    }
}

fn learn_traits() {
    println!("# Learning traits");
    // notice we can use the Animal constructor
    let mut dolly: Sheep = Animal::new("Dolly");
    let bambi = Sheep::new("Bambi");

    let spike: Dog = Animal::new("Spike");
    dolly.talk();
    dolly.shear();
    dolly.talk();
    bambi.talk();
    spike.talk();

}

fn main() {
    learn_traits();
    operator::function();
    bounds::function();
    drop::function();
    iter::function();
    clone::function();
}
