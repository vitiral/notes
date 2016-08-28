

type Result<T> = std::result::Result<T, String>;

struct Pool<'a> {
    data: &'a [u32],
}

struct Mutex<'a> {
    index: usize,
    pool: &'a Pool<'a>,
}

struct MutextGuard<'a> {
    __lock: &'a Mutex<'a>,
}

impl <'pool> Pool<'pool> {
    pub fn alloc(&self, i: usize) -> Result<Mutex<'pool> {
        // Note: if already allocated, return Error
        Ok(Mutex{index: i, pool: self})
    }
}


#[test]
fn it_works() {
    let data: [u32; 20] = [10; 20];
    let pool = Pool {
        data: &data[..],
    };
    // assert_eq!(pool.alloc(0).unwrap(), &10);
}
