use std::mem;

#[derive(Eq, PartialEq)]
#[repr(packed)]
struct Bob {
    id: u32,
    age: u32,
}

unsafe fn get_type<'a, T: Sized>(p: *const u8) -> &'a T {
    mem::transmute(p)
}

unsafe fn get_type2<'a, T: ?Sized>(p: *const u8) -> &'a T {
    mem::transmute(p)
}


// let s: &[T] = std::slice::from_raw_parts(p, size);

#[test]
fn it_works() {
    let bob = Bob {
        id: 22,
        age: 445,
    };
    let bob2: &Bob = unsafe {
        let ptr: *const u8 = mem::transmute(&bob);
        get_type(ptr)
    };
    assert_eq!(&bob, bob2);
}
