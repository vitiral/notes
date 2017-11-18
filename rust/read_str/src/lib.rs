use std::fs::File;
use std::io::Read;

fn read_it<R>(r: &mut R)
    where R: Read
{
    let mut buf = String::new();
    r.read_to_string(&mut buf);
    println!("read: {}", buf);
}

#[test]
fn it_works() {
    read_it(&mut File::open("example.txt").unwrap());
    read_it(&mut "read this string".as_bytes());

    let mut b = "This string will be read".as_bytes();
    let mut buffer = [0; 10];

    // read up to 10 bytes
    b.read(&mut buffer).unwrap();
}
