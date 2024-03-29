
#[derive(Debug)]
struct I32(i32);
#[derive(Debug)]
struct I64(i64);

trait Add<Rhs> {
    type Sum;
    fn add(&self, rhs: &Rhs) -> Self::Sum;
}

impl Add<I32> for I32 {
    type Sum = I32;
    fn add(&self, rhs: &I32) -> Self::Sum { I32(self.0 + rhs.0) }
}

impl Add<I64> for I32 {
    type Sum = I64;
    fn add(&self, rhs: &I64) -> Self::Sum { I64(self.0 as i64 + rhs.0) }
}

impl Add<I32> for I64 {
    type Sum = I64;
    fn add(&self, rhs: &I32) -> Self::Sum { I64(self.0 + rhs.0 as i64) }
}

impl Add<I64> for I64 {
    type Sum = I64;
    fn add(&self, rhs: &I64) -> Self::Sum { I64(self.0 + rhs.0) }
}

pub fn function() {
    println!("# Learning how to add");
    let i = I32(42);
    let j = I64(66);
    println!("{:?} + {:?} = {:?}", &i, &i, i.add(&i));
    println!("{:?} + {:?} = {:?}", &i, &j, i.add(&j));
    println!("{:?} + {:?} = {:?}", &j, &i, j.add(&i));
    println!("{:?} + {:?} = {:?}", &j, &j, j.add(&j));
}
