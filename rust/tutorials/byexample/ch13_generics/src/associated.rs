
struct Container(i32, i32);

trait Contains<A, B> {
    fn contains(&self, &A, &B) -> bool; // explicitly requires A and B
    fn first(&self) -> i32;             // doesn't require A or B
    fn last(&self) -> i32;              // doesn't require A or B
}

impl Contains<i32, i32> for Container {
    // True if numbers stored are equal
    fn contains(&self, n1: &i32, n2: &i32) -> bool {
        (&self.0 == n1) && (&self.1 == n2)
    }

    // Grab the first number
    fn first(&self) -> i32 {
        self.0
    }

    // Grab the last number
    fn last(&self) -> i32 {
        self.1
    }
}

// C contains A and B. In light of that, having to express
// A and B again is a nuisance (especially when we never even use them!)
// Associated types will allow us to get around this
fn difference<A, B, C>(container: &C) -> i32 where
    C: Contains<A, B> {
    container.last() - container.first()
}


pub fn function() {
    println!("# learning associated");
    let c = Container(1, 2);
    assert!(c.contains(&1, &2));
    assert!(!c.contains(&1, &3));
    assert_eq!(c.first(), 1);
    assert_eq!(c.last(), 2);
    assert_eq!(difference(&c), 1);
}


trait Contains2 {
    type A;
    type B;
    fn contains2(&self, &Self::A, &Self::B) -> bool;
    fn first2(&self) -> i32;
    fn last2(&self) -> i32;
}

// now we can do something like:
// fn difference<C: Contains>(container: &C) -> i32 { ... }

impl Contains2 for Container {
    type A = i32;
    type B = i32;

    fn contains2(&self, n1: &i32, n2: &i32) -> bool{
        (&self.0 == n1) && (&self.1 == n2)
    }
    /// Grab the first number
    fn first2(&self) -> i32 {
        self.0
    }

    /// Grab the last number
    fn last2(&self) -> i32 {
        self.1
    }
}

fn difference2<C: Contains2>(c: &C) -> i32 {
    c.last2() - c.first2()
}

pub fn function2() {
    println!("# learning associated 2");
    let c = Container(1, 2);
    assert!(c.contains2(&1, &2));
    assert!(!c.contains2(&1, &3));
    assert_eq!(c.first2(), 1);
    assert_eq!(c.last2(), 2);
    assert_eq!(difference2(&c), 1);
}
