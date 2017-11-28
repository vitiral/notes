use core;
use core::ops::DerefMut;
use core::marker::PhantomData;
use core::mem;

type Result<T> = core::result::Result<T, ()>;
type TryLockResult<T> = core::result::Result<T, ()>;

struct RawPool {
    data: *mut u8,
}

struct Pool {
    raw: RawPool,
}

impl Pool {
    pub fn new(data: *mut u8) -> Pool {
        Pool { raw: RawPool { data: data } }
    }

    pub fn alloc<T>(&self) -> Result<Mutex<T>> {
        // Note: in real-life this will fail if not enough mem
        Ok(Mutex{pool: self, _type: PhantomData})
    }
}

struct Mutex<'a, T> {
    pool: &'a Pool,
    _type: PhantomData<T>,
}


impl<'a, T> Mutex<'a, T> {
    pub fn try_lock(&'a self) -> TryLockResult<MutexGuard<T>> {
        // Note: in real-life this will fail if the item is in use
        Ok(MutexGuard{__lock: self})
    }
}

struct MutexGuard<'a, T: 'a> {
    __lock: &'a Mutex<'a, T>,
}

impl <'a, T: 'a> MutexGuard<'a, T> {
    #[allow(mutable_transmutes)]
    fn get_ref(&self) -> &'a mut T {
        unsafe {
            mem::transmute(self.__lock.pool.raw.data)
        }
    }
}

// impl<'mutex, T> Deref for MutexGuard<'mutex, T> {
//     type Target = T;

//     fn deref(&self) -> &T {
//         unsafe {
//             let raw = self.__lock.pool.raw.get();
//             let p: *const u8 = mem::transmute(& (*raw).data[0]);
//             mem::transmute(p)
//         }
//     }
// }

#[test]
fn it_works() {
    let mut data: [u32; 20] = [0x01010101; 20];
    let d = unsafe { mem::transmute(&mut data[..][0]) };
    let pool = Pool::new(d);
    let alloced = pool.alloc::<u32>();
    let unwrapped_alloc = alloced.unwrap();
    let locked = unwrapped_alloc.try_lock();
    let unwrapped_locked = locked.unwrap();
    let result = unwrapped_locked.get_ref();
    let expected = &0x01010101;
    assert_eq!(&data[0], expected);
    assert_eq!(result, expected, "{:x} != {:x}", result, expected);
    println!("{}", data[0]);
    // match pool.alloc::<u32>() {
    //     Ok(v) => match v.try_lock() {
    //         Ok(l) => {
    //             assert_eq!(l.deref(), &0x01010101);
    //         }
    //         Err(_) => assert!(false),
    //     },
    //     Err(_) => assert!(false),
    // }
}
