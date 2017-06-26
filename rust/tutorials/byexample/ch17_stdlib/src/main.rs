// cmd-line args
use std::env;

// filesystem operations
use std::io;
use std::fs;
use std::fs::OpenOptions;
use std::os::unix;

// pipes
use std::process::Stdio;

// command line
use std::process::Command;

// learn files
use std::error::Error;
use std::fs::File;
use std::io::prelude::*;

// learn path
use std::path::Path;

// learn channels
use std::sync::mpsc::{Sender, Receiver};
use std::sync::mpsc;

// learn thread
use std::time::Duration;
use std::thread;



fn join_children(children: Vec<thread::JoinHandle<()>>) {
    for child in children {
        // Wait for the thread to finish. Returns a result.
        let _ = child.join();
    }
}

fn learn_thread() {
    println!("# Learning thread");
    let n_threads: i32 = 10;
    let mut children = vec![];
    for i in 0..n_threads {
        children.push(thread::spawn(move || {
            thread::sleep(Duration::from_millis((100 - i * 5) as u64));
            println!("> learning thread number {}", i);
        }));
    }
    join_children(children);
}

fn learn_channels() {
    println!("# Learning channels");
    let n_threads: i32 = 3;
    // type annotation is not necessary
    let (tx, rx): (Sender<i32>, Receiver<i32>) = mpsc::channel();
    let mut children = vec![];
    for id in 0..n_threads {
        let thread_tx = tx.clone();

        // Each thread will send it's id back via the channel
        children.push(thread::spawn(move || {
            thread_tx.send(id).unwrap();
            // sending is non-blocking, it will continue immediately
            println!("thread {} finished!", id);
        }));
    }
    let mut ids = Vec::with_capacity(n_threads as usize);
    for _ in 0..n_threads {
        ids.push(rx.recv());
    }
    println!("Got: {:?}", ids);
    join_children(children);
}

fn learn_path() {
    println!("# Learning path");
    // create path from `&static str`
    let path = Path::new(".");

    // the `display` method returns a "showable" struct
    let display = path.display();
    println!("display: {}", display);
    println!("debug path: {:?}", path);

    // join merges the path with a byte container, and
    // returns a new path
    let new_path = path.join("a").join("b");

    // convert the path into a string slice
    match new_path.to_str() {
        None => panic!("new path is not valid utf-8"),
        Some(s) => println!("new path is: {}", s),
    }

    // pathbuf is really the best way to manipulate a path
    let mut new_buf = new_path.to_path_buf();
    new_buf.pop();
    new_buf.pop();
    assert_eq!(new_buf.as_path(), path);
    new_buf.push("a");
    new_buf.push("b");
    assert_eq!(new_buf.as_path(), new_path);
}

fn learn_files() {
    println!("# Learning files");
    let path = Path::new("hello.txt");
    // open the path in read-only mode
    let mut file = File::open(&path).unwrap();
    // read the contents into a string
    let mut s = String::new();
    match file.read_to_string(&mut s) {
        Err(why) => {
            panic!("couldn't read {}: {}",
                   path.display(),
                   Error::description(&why))
        }
        Ok(file) => file,
    };
    println!("> File says:\n{}", s);
    // file goes out of scope and hello.txt file get's closed

    let path = Path::new("hello.out");
    // open a file in write-only mode
    let mut file = File::create(&path).unwrap();
    let long_str = "hello there, I love you very much.\nThis is a new line. I'm not sure how it \
                    will work.\nAnother new line";
    println!(">Long string:\n{}", long_str);
    match file.write_all(long_str.as_bytes()) {
        Err(why) => panic!("Couldn't write {}: {}", path.display(), why),
        Ok(_) => println!(">successfully wrote long_str to {}", path.display()),
    }
}

fn learn_subprocess() {
    // subprocess == shell == Command
    println!("# Learning subprocess (i.e. shell, command)");
    let output = Command::new("rustc")
                     .arg("-version")
                     .output()
                     .unwrap_or_else(|e| panic!("failed to execute process: {}", e));

    if output.status.success() {
        let s = String::from_utf8_lossy(&output.stdout);
        println!("rustc succeeded and stdout was: {}", s);
    } else {
        let s = String::from_utf8_lossy(&output.stderr);
        println!("rustc failed, {}, stderr:\n{}", output.status, s);
    }
}

fn learn_pipes() {
    println!("\n# Learn pipes");
    let pangram = "the quick brown fox jumped over the lazy dog\n";
    let process = match Command::new("wc")
                            .stdin(Stdio::piped())
                            .stdout(Stdio::piped())
                            .spawn() {
        Err(why) => panic!("couldn't spawn wc: {}", Error::description(&why)),
        Ok(process) => process,
    };
    // let mut stdin = process.stdin.unwrap();
    {
        // if the commenting above/below is taken out, the program hangs indefinitely
        let mut stdin = process.stdin.unwrap();
        // write a string to the `stdin` of `wc`
        // `stdin` has type Option<ChildStdin>, but since
        // we know this instance must have one, we can directly
        // `unwrap` it.
        // match process.stdin.unwrap().write_all(pangram.as_bytes()) {
        match stdin.write_all(pangram.as_bytes()) {
            Err(why) => panic!("couldn't write to wc stdin: {}", Error::description(&why)),
            Ok(_) => println!("sent pangram to `wc`"),
        }
        // `stdin` gets `drop`ed here and the pipe is closed.
        // this is very important, as otherwise `wc` wouldn't start processing input we just
        // sent.
    }
    // let stdin = process.stdin;  // cannot do this with stdin inside the scope, as it was moved

    // `stdout` also has type Option<ChildStdout> so must be unwrapped
    // in this case we KNOW it should be there, since we created it.
    let mut s = String::new();
    match process.stdout.unwrap().read_to_string(&mut s) {
        Err(why) => panic!("couldn't read wc stdout: {}", Error::description(&why)),
        Ok(_) => println!("wc responded with: {}", s),
    }
}

// # filesystem stuff

/// simple implementation of `% cat`
fn cat(path: &Path) -> io::Result<String> {
    let mut f = try!(File::open(path));
    let mut s = String::new();
    try!(f.read_to_string(&mut s));
    Ok(s)
}


/// simple implementation of `% echo > path`
fn echo(s: &str, path: &Path) -> io::Result<()> {
    let mut f = try!(File::create(path));
    f.write_all(s.as_bytes())
}

/// a simple implementation of `% touch path` which ignores existing files
fn touch(path: &Path) -> io::Result<()> {
    OpenOptions::new().create(true).open(path).map(|_| ())
}


fn learn_filesystem() {
    println!("\n# learn filesystem");
    println!("% mkdir a");
    match fs::create_dir("a") {
        Err(why) => println!("! {:?}", why.kind()),
        Ok(_) => println!("> done"),
    }
    println!("% echo hello > a/b.txt");
    echo("hello", &Path::new("a/b.txt")).unwrap_or_else(|why| println!("! {:?}", why.kind()));

    println!("% mkdir -p a/c/d");
    // Recursively create a directory
    fs::create_dir_all("a/c/d").unwrap_or_else(|why| println!("! {:?}", why.kind()));

    println!("% touch a/c/e.txt");
    touch(Path::new("a/c/e.txt")).unwrap_or_else(|why| println!("! {:?}", why.kind()));

    println!("% ln -s hello.out a/c/b.txt");
    if cfg!(target_family = "unix") {
        unix::fs::symlink("../../hello.out", "a/c/b.txt")
            .unwrap_or_else(|why| println!("! {:?}", why.kind()));
    }

    println!("% cat a/c/b.txt");
    match cat(&Path::new("a/c/b.txt")) {
        Err(why) => println!("! {:?}", why.kind()),
        Ok(s) => println!("{}", s),
    }
    println!("% ls a");
    // Read the contents of a directory. Returns io::Result<Vec<Path>>>
    match fs::read_dir("a") {
        Err(why) => println!("! {:?}", why.kind()),
        Ok(paths) => {
            for path in paths {
                println!("> {:?}", path.unwrap().path());
            }
        }
    }

    println!("% rm a/c/e.txt");
    // remove a file, returns io::Result<()>
    // interesting, it can either take a str or a Path object... I wonder why that is
    fs::remove_file(Path::new("a/c/e.txt")).unwrap_or_else(|why| println!("! {:?}", why.kind()));
    println!("% rmdir a/c/d");
    // Remove an empty directory
    fs::remove_dir("a/c/d").unwrap_or_else(|why| println!("! {:?}", why.kind()));

}

fn learn_cmd_args() {
    let args: Vec<String> = env::args().collect();

    println!("My path is: {}", args[0]);
    println!("I got {} arguments: {:?}", args.len() - 1, &args[1..]);
}

fn main() {
    println!("Hello, world!");
    learn_thread();
    learn_channels();
    learn_path();
    learn_files();
    learn_subprocess();
    learn_pipes();
    learn_filesystem();
    learn_cmd_args();
}
