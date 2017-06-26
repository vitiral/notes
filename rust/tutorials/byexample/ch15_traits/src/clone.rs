
// A unit struct without resources
#[derive(Debug, Clone, Copy)]
struct Nil;

// A tuple struct with resources that implements the `Clone` trait
#[derive(Debug, Clone)]
struct Pair(Box<i32>, Box<i32>);

pub fn function() {
    println!("# Learning clone");
    let nil = Nil;
    // copy nil
    let copied_nil = nil;

    // Both of nil's values can be used because they were
    // copied, not moved
    println!("nil = {:?}", nil);
    println!("copied_nil = {:?}", copied_nil);

    // create a Pair
    let pair = Pair(Box::new(1), Box::new(2));
    println!("original = {:?}", pair);

    // copy pair into `moved_pair`, moves resources
    let moved_pair = pair;
    println!("moved = {:?}", moved_pair);

    // ! error, pair has lost it's resource
    // println!("original = {:?}", pair);

    // "clone" `moved_pair` into `cloned_pair` (resources included)
    let cloned_pair = moved_pair.clone();
    println!("stil have moved = {:?}", moved_pair);
    println!("and have cloned = {:?}", cloned_pair);
    // drop the original pair
    drop(moved_pair);

    println!("dropped the original, still have cloned = {:?}", cloned_pair);
}
