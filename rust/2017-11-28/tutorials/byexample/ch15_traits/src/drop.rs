

struct D {
    name: &'static str,
}

// This `drop` doesn't actually free resources, but reports how the drop works
impl Drop for D {
    fn drop(&mut self) {
        println!("> Dropping {}", self.name);
    }
}

pub fn function() {
    println!("# Learning drop");
    let _all = D{ name: "all" };
    let a = D{name: "a"};
    // Block A
    {
        let _b = D{name: "b"};
        // Block B
        {
            let _c = D{name:"c"};
            let _d = D{name:"d"};
            println!("Exiting block B");
        }
        println!("Just exited block B");
        println!("Exiting block A");
    }
    println!("Just exited block A");
    println!("dropping a prematurely");
    drop(a);
    println!("Exiting function");
}
