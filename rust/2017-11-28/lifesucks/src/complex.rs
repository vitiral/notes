use core;
use core::ops::Deref;
use core::marker::PhantomData;
use core::mem;

type Result<T> = core::result::Result<T, ()>;
type TryLockResult<T> = core::result::Result<T, ()>;

struct RawPool<'a> {
    data: &'a mut [u8],
}

struct Pool<'a> {
    raw: RawPool<'a>,
}

impl <'pool> Pool<'pool> {
    pub fn new(data: &'pool mut [u8]) -> Pool<'pool> {
        Pool { raw: RawPool { data: data } }
    }

    pub fn alloc<T>(&'pool self) -> Result<Mutex<'pool, T>> {
        // Note: in real-life this will fail if not enough mem
        Ok(Mutex{pool: self, _type: PhantomData})
    }
}

struct Mutex<'a, T> {
    pool: &'a Pool<'a>,
    _type: PhantomData<T>,
}


impl<'pool, T> Mutex<'pool, T> {
    pub fn try_lock(&'pool self) -> TryLockResult<MutexGuard<T>> {
        // Note: in real-life this will fail if the item is in use
        Ok(MutexGuard{__lock: self})
    }
}

struct MutexGuard<'a, T: 'a> {
    // Maybe remove this 'a?
    __lock: &'a Mutex<'a, T>,
}

// maybe add + 'mutex here?
impl<'mutex, T> Deref for MutexGuard<'mutex, T> {
    type Target = T;

    fn deref(&self) -> &T {
        unsafe {
            let p: *const u8 = mem::transmute(&self.__lock.pool.raw.data[0]);
            mem::transmute(p)
        }
    }
}

#[test]
fn it_works() {
    let mut data: [u8; 20] = [1; 20];
    let pool = Pool::new(&mut data[..]);
    let alloced = pool.alloc::<u32>();
    let unwrapped_alloc = alloced.unwrap();
    let locked = unwrapped_alloc.try_lock();
    let unwrapped_locked = locked.unwrap();
    assert_eq!(unwrapped_locked.deref(), &0x01010101);
    match pool.alloc::<u32>() {
        Ok(v) => match v.try_lock() {
            Ok(l) => {
                assert_eq!(l.deref(), &0x01010101);
            }
            Err(_) => assert!(false),
        },
        Err(_) => assert!(false),
    }
}
