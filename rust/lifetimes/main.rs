
/// lifetimes in structs ensure that any reference to Foo does not
/// outlive a reference to x
struct Foo<'a> {  // struct with lifetime 'a
    x: &'a i32,   // vaiable with lifetime 'a
}


///implemnt a method on Foo
impl<'a> Foo<'a> {
    fn x(&self) -> &'a i32 { self.x }
}


fn main(){
    let y = &5;   // this is the same as `let _y = 5; let y = &_y;
    let f = Foo { x: y };
    println!("{}", f.x);        
    println!("{}", f.x());      // wow, screwy... now x is both a method and a variable???
}
