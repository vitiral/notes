#![allow(dead_code)]

use std::num::ParseIntError;
use std::result;

mod more_errors;
mod try;
mod better_err;
mod better_err2;
mod box_err;

#[derive(Debug)]
enum Food {
    Apple,
    Carrot,
    Potato,
    CordonBlue,
    Steak,
    Sushi,
}
#[derive(Debug)]
struct Peeled(Food);
#[derive(Debug)]
struct Chopped(Food);
#[derive(Debug)]
struct Cooked(Food);

/// peel the food, if there isn't any just return None
fn peel(food: Option<Food>) -> Option<Peeled> {
    match food {
        Some(food) => Some(Peeled(food)),
        None => None,
    }
}

fn chop(peeled: Option<Peeled>) -> Option<Chopped> {
    match peeled {
        Some(Peeled(food)) => Some(Chopped(food)),
        None => None,
    }
}

// map takes an option and returns None if the value is None,
// else it calls the closure with the value
fn cook(chopped: Option<Chopped>) -> Option<Cooked> {
    chopped.map(|Chopped(food)| Cooked(food))
}

// Simplify the process even further
fn process(food: Option<Food>) -> Option<Cooked> {
    food.map(|f| Peeled(f))
        .map(|Peeled(f)| Chopped(f))
        .map(|Chopped(f)| Cooked(f))
}

fn eat(food: Option<Cooked>) {
    match food {
        Some(food) => println!("Mmm, I love {:?}", food),
        None => println!("Oh no! it wasn't edible"),
    }
}


fn learn_map() {
    println!("# Learning map");
    let apple = Some(Food::Apple);
    let carrot = Some(Food::Carrot);
    let potato = Some(Food::Potato);


    let cooked_apple = cook(chop(peel(apple)));
    let cooked_carrot = cook(chop(peel(carrot)));
    // simpler "process"
    let cooked_potato = process(potato);

    eat(cooked_apple);
    eat(cooked_carrot);
    eat(cooked_potato);

    eat(None);
    eat(process(None));
}

#[derive(Debug)]
enum Day {
    Monday,
    Tuesday,
    Wednesday,
}

/// we don't have the ingredients for sushi
fn have_ingredients(food: Food) -> Option<Food> {
    match food {
        Food::Sushi => None,
        _ => Some(food),
    }
}

/// can cook anything but cordon blue
fn can_cook(food: Food) -> Option<Food> {
    match food {
        Food::CordonBlue => None,
        _ => Some(food),
    }
}

/// to make a meal we need the ingredients and the ability to cook it.
/// This can be better written with `and_then`
fn cookable_v1(food: Food) -> Option<Food> {
    match have_ingredients(food) {
        None => None,
        Some(food) => {
            match can_cook(food) {
                None => None,
                Some(food) => Some(food),
            }
        }
    }
}

fn eat_v1(food: Food, day: Day) {
    match cookable_v1(food) {
        Some(food) => println!("Yay! On {:?} we eat {:?}", day, food),
        None => println!("Oh no we didn't get to eat on {:?}!", day),
    };
}

/// can be done using map
fn cookable_v2(food: Food) -> Option<Food> {
    match have_ingredients(food).map(|f| can_cook(f)) {
        Some(food) => food,
        None => None,
    }
    // // Why isn't this right????
    // -- `map` automatically wraps in Option, so it really isn't the
    //    right choice here!
    // match have_ingredients(food).map(|f| can_cook(f)) {
    //     Some(food) => Some(food),
    //     None => None,
    // }
}

fn eat_v2(food: Food, day: Day) {
    match cookable_v2(food) {
        Some(food) => println!("Yay! On {:?} we eat {:?}", day, food),
        None => println!("Oh no we didn't get to eat on {:?}!", day),
    };
}


fn cookable_v3(food: Food) -> Option<Food> {
    have_ingredients(food).and_then(can_cook)  // and_then does NOT wrap with Some
}

fn eat_v3(food: Food, day: Day) {
    match cookable_v3(food) {
        Some(food) => println!("Yay! On {:?} we eat {:?}", day, food),
        None => println!("Oh no we didn't get to eat on {:?}!", day),
    };
}

fn learn_and_then() {
    println!("# Learning and_then");
    let (cordon_blue, steak, sushi) = (Food::CordonBlue, Food::Steak, Food::Sushi);
    eat_v1(cordon_blue, Day::Monday);
    eat_v1(steak, Day::Tuesday);
    eat_v1(sushi, Day::Wednesday);

    let carrot = Food::Carrot;
    eat_v2(carrot, Day::Tuesday);

    let potato = Food::Potato;
    eat_v3(potato, Day::Wednesday);
}


// # Use Err

/// can do things explicitly with match
fn double_number(number_str: &str) -> Result<i32, ParseIntError> {
    match number_str.parse::<i32>() {
        Ok(n) => Ok(2 * n),
        Err(e) => Err(e),
    }
}

// showing how to use an alias
type IntResult<T> = result::Result<T, ParseIntError>;

/// or can do things more simply with map like sematics
fn double_number_map(number_str: &str) -> IntResult<i32> {
    number_str.parse::<i32>().map(|n| n * 2)
}

fn print(result: IntResult<i32>) {
    match result {
        Ok(n) => println!("n is {}", n),
        Err(e) => println!("Error: {}", e),
    }
}


fn double_first_v1(vec: &Vec<&str>) -> i32 {
    // what if the vector is empty?
    let first = vec.first().unwrap();
    // what if it doesn't parse?
    2 * first.parse::<i32>().unwrap()

}
// we now want to eliminate unwrap from the above.
// first pass we will just use a String Err
type StrResult<T> = Result<T, String>;

fn double_first_v2(vec: &Vec<&str>) -> StrResult<i32> {
    // convert the Option to a Result if there is a value, else convert to Err
    // must convert the ParseIntError to String
    vec.first()
       .ok_or("Vector len < 1".to_owned())
       .and_then(|s| {
           s.parse::<i32>()
            .map_err(|e| e.to_string())
            .map(|n| n * 2)
       } /* Apply the double */)
}

fn learn_err() {
    println!("# Learning Err");
    print(double_number("2"));
    print(double_number_map("2"));
    print(double_number_map("hello"));

    let numbers = vec!["93", "18"];
    let empty: Vec<&str> = vec![];
    let strs = vec!["tofu", "93"];
    println!("The first doubled is: {}", double_first_v1(&numbers));
    // These panic
    // println!("The first doubled is: {}", double_first_v1(empty));
    // println!("The first doubled is: {}", double_first_v1(strs));
    println!("v2 double numbers: {:?}", double_first_v2(&numbers));
    println!("v2 double empty: {:?}", double_first_v2(&empty));
    println!("v2 double strs: {:?}", double_first_v2(&strs));
}





fn main() {
    learn_map();
    learn_and_then();
    learn_err();
    more_errors::function();
    try::function();
    better_err::function();
    better_err2::function();
    box_err::function();
}
