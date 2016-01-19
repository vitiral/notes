
use std::ops::Add;
use std::marker::PhantomData;

// PhantomData allows us to construct types without needing to actually create types.
// Storage is not allocated for phantom data, it is a compiler check ONLY.
// You can use different Phantom types to specify what the class works with.
// Generic tuple struct with same types in both
// generic and definition

struct Tuple<A>(A,);

// phantom tuple. Storage is allocated for A but not for B.
// Therefore B cannot be used for computations
#[derive(PartialEq)]
struct PhantomTuple<A, B>(A, PhantomData<B>);

// similar phantom struct
#[derive(PartialEq)]
struct PhantomStruct<A, B>{ first: A, phantom: PhantomData<B> }

// # Unit conversions
// A useful case for phantom types is in unit conversions 

#[derive(Debug, Clone, Copy)]
struct Inch;
#[derive(Debug, Clone, Copy)]
struct Mm;

// Length is a phantom type with hidden parameter Unit
#[derive(Debug, Clone, Copy)]
struct Length<Unit>(f64, PhantomData<Unit>);

// the `Add` trait implements +
impl <Unit>Add for Length<Unit> {
    type Output = Length<Unit>;

    // `add()` returns a new Length struct containing the sum
    fn add(self, rhs: Length<Unit>) -> Length<Unit> {
        Length(self.0 + rhs.0, PhantomData)
    }
}



pub fn function() {
    println!("# Learning phantom");
    // we can create similar types without having to carry around extra info
    let tuple_a: PhantomTuple<char, f32> = PhantomTuple('Q', PhantomData);
    let tuple_b: PhantomTuple<char, f32> = PhantomTuple('Q', PhantomData);
    let _tuple_c: PhantomTuple<char, f64> = PhantomTuple('Q', PhantomData);
    assert!(tuple_a == tuple_b);
    // assert!(tuple_a == _tuple_c); // error: mismatched types

    println!("learning units");
    let one_foot: Length<Inch> = Length(12.0, PhantomData);
    let one_meter: Length<Mm> = Length(1000.0, PhantomData);

    let two_feet = one_foot + one_foot;
    let two_meters = one_meter + one_meter;

    println!("one foot plus one foot = {:?}", two_feet);
    println!("one meter plus one meter = {:?}", two_meters);
}
