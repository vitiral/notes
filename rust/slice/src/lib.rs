#[test]
fn it_works() {
    let v = vec![1,2,3,4];
    assert_eq!(&v[1..3], &[2,3]);
    assert_eq!(&v[2..6], &[3,4]);
}
