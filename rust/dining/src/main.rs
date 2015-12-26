use std::thread;
use std::sync::{Mutex, Arc};

struct Table {
    forks: Vec<Mutex<()>>,
}

#[derive(Debug, Clone)]
struct Philosopher {
    name: String,
    left: usize,
    right: usize,
}

impl Philosopher {
    fn new(name: &str, left: usize, right: usize) -> Philosopher {
        Philosopher {
            name: name.to_string(),
            left: left,
            right: right,
        }
    }

    fn eat(&self, table: &Table) {
        let _left = table.forks[self.left].lock().unwrap();
        thread::sleep_ms(150);
        let _right = table.forks[self.right].lock().unwrap();
        println!("{} is eating", self.name);

        thread::sleep_ms(1000);

        println!("{} is done eating", self.name);
    }
}


fn main() {
    // create names
    let names = vec![
        "Judith Butler",
        "Gilles Deleuze",
        "Karl Marx",
        "Emma Goldman",
        "Michel Foucault",
    ];

    // create philosophers
    let length = names.len();
    let mut philosophers: Vec<Philosopher> = vec![];
    for (i, name) in names.into_iter().enumerate() {
        let left = i % length;
        let right = (left + 1) % length;
        philosophers.push(Philosopher::new(name, left, right));
    }
    // make the last philosopher left handed (switch forks)
    {
        // Ok, several things...
        // 1. can't seem to do:
        //   let p = philosophers[length -1]
        //   and then use p -- it is "borrowing" from philosophers,
        //   so I can't modify philsophers after getting it
        // 2. Instead I have to implement the clone method and then clone it
        //   This is rather annoying, but it might make SOME sense... no, it
        //   really doesn't.
        //let p = philosophers[length - 1].clone();
        let ref p = philosophers[length -1];
        philosophers[length - 1] = Philosopher{left: p.right, right: p.left, name: p.name.clone()};
        //philosophers[length - 1] = Philosopher{
        //    left: philosophers[length - 1].right, 
        //    right: philosophers[length - 1].left, 
        //    name: philosophers[length -1].name.clone(),
        //};
    }

    // print philosophers, requires Debug trait to be defined
    println!("# Here are our philosophers, ready to eat!");
    for p in &philosophers {
        println!("{:?}", p);
    }

    // create forks
    let mut forks: Vec<Mutex<()>> = vec![];
    for _ in 0..length {
        forks.push(Mutex::new(()));
    }

    // create table
    let table = Arc::new(Table { forks: forks });

    // create and start threads
    println!("# Begin the eating!");
    let handles: Vec<_> = philosophers.into_iter().map(|p| {
        let table = table.clone();
        thread::spawn(move || {
            p.eat(&table);;
        })
    }).collect();

    // join threads
    for h in handles {
        h.join().unwrap();
    };
}
