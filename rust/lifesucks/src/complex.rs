use std;
use std::ops::Deref;
use std::marker::PhantomData;
use std::mem;

type Result<T> = std::result::Result<T, String>;
type TryLockResult<T> = std::result::Result<T, String>;

struct Pool<'a> {
    data: &'a [u8],
}

struct Mutex<'a, T> {
    pool: &'a Pool<'a>,
    _type: PhantomData<T>,
}

struct MutexGuard<'a, T: 'a> {
    // Maybe remove this 'a?
    __lock: &'a Mutex<'a, T>,
}

impl <'pool> Pool<'pool> {
    pub fn new(data: &'pool [u8]) -> Pool<'pool> {
        Pool {
            data: &data[..],
        }
    }

    pub fn alloc<T>(&'pool self) -> Result<Mutex<'pool, T>> {
        // Note: in real-life this will fail if not enough mem
        Ok(Mutex{pool: self, _type: PhantomData})
    }
}

impl<'pool, T> Mutex<'pool, T> {
    pub fn try_lock(&'pool self) -> TryLockResult<MutexGuard<T>> {
        // Note: in real-life this will fail if the item is in use
        Ok(MutexGuard{__lock: self})
    }
}

// maybe add + 'mutex here?
impl<'mutex, T> Deref for MutexGuard<'mutex, T> {
    type Target = T;

    fn deref(&self) -> &T {
        unsafe {
            let p: *const u8 = mem::transmute(&self.__lock.pool.data[0]);
            mem::transmute(p)
        }
    }
}

#[test]
fn it_works() {
    let data: [u8; 20] = [1; 20];
    let pool = Pool::new(&data[..]);
    let alloced = pool.alloc::<u32>();
    let unwrapped_alloc = alloced.unwrap();
    let locked = unwrapped_alloc.try_lock();
    let unwrapped_locked = locked.unwrap();
    assert_eq!(unwrapped_locked.deref(), &0x01010101);
    match pool.alloc::<u32>() {
        Ok(v) => match v.try_lock() {
            Ok(l) => {
                println!("l: {}", l.deref());
                assert_eq!(l.deref(), &0x01010101);
            }
            Err(_) => assert!(false),
        },
        Err(_) => assert!(false),
    }
}
