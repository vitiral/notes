
use std::ops::{Add, Sub, Mul};

#[derive(Debug, Clone, Copy)]
struct Vec2<T> {
    x: T,
    y: T,
}

// Apply bounds to `T` at first instance of `T`
// `T` must implement the Add trait
impl<T: Add<T, Output=T>> Add<Vec2<T>> for Vec2<T> {
    type Output = Vec2<T>;
    fn add(self, rhs: Vec2<T>) -> Vec2<T> {
        Vec2 {
            // x and y are of type `T` and implement the `add` method
            x: self.x.add(rhs.x),
            // this line is identical in function
            y: self.y + rhs.y,
        }
    }
}

impl<T: Sub<T, Output=T>> Sub<Vec2<T>> for Vec2<T> {
    type Output = Vec2<T>;
    fn sub(self, rhs: Vec2<T>) -> Vec2<T> {
        Vec2 {
            x: self.x - rhs.x,
            y: self.y - rhs.y,
        }
    }
}

impl<T> Vec2<T>
        where T: Add<T, Output=T> + Mul<T, Output=T> {

    fn dot(self, rhs: Vec2<T>) -> T{
        (self.x * rhs.x) + (self.y * rhs.y)
    }
}



pub fn function() {
    println!("# learning bounds");
    let v1 = Vec2{x: 1.2_f32, y:3.4 };
    let v2 = Vec2{x:5.6_f32, y:5.2};
    println!("{:?} + {:?} = {:?}", v1, v2, v1 + v2);
    println!("{:?} - {:?} = {:?}", v1, v2, v1 - v2);
    println!("{:?} dot {:?} = {:?}", v1, v2, v1.dot(v2));

    // error, char doesn't implement Add
    // println!("{:?}", Vec2{x: 'a', y:'b'} + Vec2{x:'k', y:'z'});

}
