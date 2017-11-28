
// where is an alternate way to express a bound. 
// There are a few reasons to use a where clause:
// - it can be easier to specify the types and bounds separately rather than
//     together. 
//     impl <A: TraitB + TraitC, D: TraitE> MyTrait<A, D> for YourType {}
// OR  impl <A, D> MyTrait<A, D> for YourType where
//         A: TraitB + TraitC,
//         D: TraitE {}
// - where clauce can be more expressive than normal syntax as they can apply
//   to arbitrary types rather than just type parameters.

use std::fmt::Debug;
use std::fmt;

trait PrintInOption {
    fn print_in_option(self);
}

impl <T> PrintInOption for T
    // without `where` we would have to use something like T:Debug
    // and in fact that works!
    // however, we don't actually care if T implements debug, we only
    // care if Option<T> implements Debug

    where Option<T>: Debug {
    fn print_in_option(self) {
        println!("{:?}", Some(self));
        // println!("{:?}", self);
        // > error: the trait `core::fmt::Debug` is not implemented for the type `T`
    }
}

// see that this works JUST AS WELL for this case, however,
// I think where certainly has it's charms.
struct C;
// #[derive(debug)] doesn't exist for null types for some reason
impl Debug for C {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "C")
    }
}


pub fn function() {
    println!("calling from where::function()");
    let vec = vec![1,2,3];
    vec.print_in_option();
    let c = C;
    C.print_in_option();
}
