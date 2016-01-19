fn steal_scope(v1: Vec<i32>) -> Vec<i32>{
    v1
}

fn dont_steal_scope(v1: &Vec<i32>) -> &str {
    "not a theif"
}


fn main(){

    // simple ownership
    let v = vec![1,2,3];
    let v2 = v;  // takes ownership
    //println!("{}", v[0]);  // ! use of moved value
    println!("{}", v2[0]);  // ! use of moved value
    let x = 5;
    let x2 = x;
    println!("{}", x); // works because x implements copy trait
    println!("{}", steal_scope(v2)[1]);  // steals scope
    //println!("{}", v2[1]);  // ERROR: use of moved value
    let v3 = vec![1,2,3];
    let v3 = steal_scope(v3);  // doesn't loose ownership because rebinds it. HACK, use borrow instead
    println!("{}", v3[1]);

    // borrowing
    // A binding that borros something does not go out of scope
    let v4 = vec![1,2,3];
    println!("{}", dont_steal_scope(&v3));
    println!("{}", v4[1]);  // works

    // mutable borrowing
    let mut x = 5;
    x = 7;  // mutable
    {  // need these or else more than one KIND of borrow in scope
        let y = &mut x;
        *y += 1;
    }
    println!("borrow mut int: {}", x);
    let mut m = vec![1,2,3];
    m[0] = 10;  // can mutate (everything is immutable normally)
    {
        let m0 = &mut m;
        m0[1] = 20;  // note that it does NOT need to be dereferenced with *
    }
    println!("{}", m[1]);

    // The rules
    // - any borrow must last for a scope no greater than that of the owner
    //   - so we can't return a reference to y in the above (I think) 
    // - you can only have one kind of borrow at a time, but not both
    //   - one or more references (& T) to a resource
    //   - EXACTLY one mutable reference (&mut T)
    
    // start of lifetimes
    let x3 = 5;  // variables are freed in REVERSE order. So this HAS to be declared before y
    let y1: &i32;
    //let x3 = 5;  // if declared here -- error: `x3` does not live long enough
    y1 = &x3;  // wtf... y is immutable. I guess only the UNDERLYING data is immutable?
    println!("{}", *y1);

}
