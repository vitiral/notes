//! stack.rs
#![allow(dead_code)]

#[derive(Debug)]
enum Food {
    CordonBlue,
    Steak,
    Sushi,
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


/// can be done using map
fn cookable(food: Food) -> Option<Food> {
    match have_ingredients(food).map(|f| can_cook(f)) {
        // Some(food) => food,  // Why is this correct???
        // - It is because `map` automatically wraps the output in an Option
        //   whereas `and_then` does not
        Some(food) => Some(food),
        None => None,
    }
}

fn eat(food: Food, day: Day) {
    match cookable(food) {
        Some(food) => println!("Yay! On {:?} we eat {:?}", day, food),
        None => println!("Oh no we didn't get to eat on {:?}!", day),
    };
}


fn main() {
    let (cordon_blue, steak, sushi) = (Food::CordonBlue, Food::Steak, Food::Sushi);
    eat(cordon_blue, Day::Monday);
    eat(steak, Day::Tuesday);
    eat(sushi, Day::Wednesday);
}
