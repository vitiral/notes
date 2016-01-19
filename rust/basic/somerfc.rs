
fn double_divide(num: f64, den: f64) -> Option<f64> {
    let value = some!(divide(num, den));
    Some(value * 2f64)
}

fn double_divide(num: f64, den: f64) -> Option<f64> {
    let value = match divide(num, den) {
        Some(value) => value,
        None => return None,
    };
    Some(value * 2f64)
}

