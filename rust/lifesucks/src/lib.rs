use std::ops::Deref;

type Result<T> = std::result::Result<T, String>;
type TryLockResult<T> = std::result::Result<T, String>;

struct Pool<'a> {
    data: &'a [u32],
}

struct Mutex<'a> {
    index: usize,
    pool: &'a Pool<'a>,
}

struct MutexGuard<'a> {
    __lock: &'a Mutex<'a>,
}

impl <'pool> Pool<'pool> {
    pub fn alloc(&'pool self, i: usize) -> Result<Mutex<'pool>> {
        // Note: in real-life this will fail if not enough mem
        Ok(Mutex{index: i, pool: self})
    }
}

impl<'pool> Mutex<'pool> {
    pub fn try_lock(&'pool self) -> TryLockResult<MutexGuard> {
        // Note: in real-life this will fail if the item is in use
        Ok(MutexGuard{__lock: self})
    }
}

impl<'mutex> Deref for MutexGuard<'mutex> {
    type Target = u32;

    fn deref(&self) -> &u32 {
        &self.__lock.pool.data[self.__lock.index]
    }
}

#[test]
fn it_works() {
    let data: [u32; 20] = [10; 20];
    let pool = Pool {
        data: &data[..],
    };
    assert_eq!(pool.alloc(0).unwrap().try_lock().unwrap().deref(), &10);
}
