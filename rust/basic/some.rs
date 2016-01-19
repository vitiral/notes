macro_rules! some {
    ($expr:expr) => (match $expr {
        Some(val) => val,
        None => return None,
    })
}

fn divide(numerator: f64, denominator: f64) -> Option<f64> {
    if denominator == 0.0 {
        None
    } else {
        Some(numerator / denominator)
    }
}

fn double_divide_1(num: f64, den: f64) -> Option<f64> {
    let value = match divide(num, den) {
        Some(value) => value,
        None => return None,
    };
    Some(value * 2f64)
}

fn double_divide_2(num: f64, den: f64) -> Option<f64> {
    let value = some!(divide(num, den));
    Some(value * 2f64)
}

fn main() {
    assert!(double_divide_1(10.0, 0.0).is_none());
    assert!(double_divide_2(10.0, 0.0).is_none());
}
