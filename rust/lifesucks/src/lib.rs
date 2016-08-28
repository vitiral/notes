use std::ops::Deref;
use std::marker::PhantomData;
use std::mem;

type Result<T> = std::result::Result<T, String>;
type TryLockResult<T> = std::result::Result<T, String>;

struct Pool<'a> {
    data: &'a [u8],
}

struct Mutex<'a, T> {
    index: usize,
    pool: &'a Pool<'a>,
    _type: PhantomData<T>,
}

struct MutexGuard<'a, T: Sized + 'a> {
    // Maybe remove this 'a?
    __lock: &'a Mutex<'a, T>,
}

impl <'pool> Pool<'pool> {
    pub fn alloc<T>(&'pool self, i: usize) -> Result<Mutex<'pool, T>> {
        // Note: in real-life this will fail if not enough mem
        Ok(Mutex{index: i, pool: self, _type: PhantomData})
    }
}

impl<'pool, T: Sized> Mutex<'pool, T> {
    pub fn try_lock(&'pool self) -> TryLockResult<MutexGuard<T>> {
        // Note: in real-life this will fail if the item is in use
        Ok(MutexGuard{__lock: self})
    }
}

// maybe add + 'mutex here?
impl<'mutex, T: Sized> Deref for MutexGuard<'mutex, T> {
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
    let pool = Pool {
        data: &data[..],
    };
    let val = pool.alloc::<u32>(0).unwrap().try_lock().unwrap();
    assert_eq!(val.deref(), &0x01010101);
}
