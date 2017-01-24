extern crate rand;

use std::io;
use std::cmp::Ordering;
use rand::Rng;

fn main() {
    println!("Guess a number!");
    let secret = rand::thread_rng().gen_range(1, 101);
    println!("The secret number is: {:?}", secret);

    loop {
        println!("\n% Please input your guess:");
        let mut guess = String::new();  // new is a associated function (static method) of String,
                                        // instead of a method of an instance of String

        io::stdin().read_line(&mut guess)
            .ok().expect("Failed to read line");

        let guess: i32 = match guess.trim().parse() {
            Ok(num) => num,
            Err(_) => {
                println!("Please guess a number!");
                continue;
            },
        };

        println!("You guessed: {}", guess); 
        match guess.cmp(&secret) {
            Ordering::Less => println!("Too small"),
            Ordering::Greater => println!("Too big"),
            Ordering::Equal => {
                println!("You win!");
                break;
            },
        }
    }
}
