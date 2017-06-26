
// a module named `my`

mod cool {
    pub fn function() {
        println!("called cool::function");
    }
}

mod my {
    // items in modules default to private visibility
    fn private_function() {
        println!("Called my:private_function()");
    }

    // Use `pub` modifier to make public
    pub fn function() {
        println!("called my::function()");
    }

    // Items can access other items in the same module
    // even when private
    pub fn indirect_access() {
        print!("Called my::indirect access that ");
        private_function();
    }

    // modules can be nested
    pub mod nested {
        pub fn function() {
            println!("called my::nested::function()");
        }

        #[allow(dead_code)]
        pub fn private_function() {
            println!("called my::nested::private_function()");
        }
    }

    // Structs have full visibility inside a module, and only
    // public members are visible outside the module.

    // a public struct with a public field
    pub struct WhiteBox<T> {
        pub contents: T,
    }

    // a public struct with a private field of generic type `T`
    #[allow(dead_code)]
    pub struct BlackBox<T> {
        contents: T,
    }

    impl<T> BlackBox<T> {
        pub fn new(contents: T) -> BlackBox<T> {
            BlackBox { contents: contents, }
        }
    }

    // # super and self
    mod cool {
        pub fn function() {
            println!("called my::cool::function()");
        }
    }

    pub fn indirect_call() {
        println!("called my::indirect::call that:");
        // these both refer to the same function
        self::function();
        function();

        // we can also use `self` to access another module inside of my,
        // these are identical
        self::cool::function();
        { use my::cool::function as coolfun; coolfun(); }

        // the `super` keyword refers to the parent scope
        super::function();

        // this will bind to the cool::function in the *crate* scope
        // (not the `my` scope)
        {
            use cool::function as root_function;
            root_function();
        }
    }
}

fn function() {
    println!("called function()");
}

fn main() {
    // module functions
    function();
    my::function();
    my::indirect_access();
    my::nested::function();

    // module structs
    // public structs with public fields can be constructed as usual
    let white = my::WhiteBox { contents: "public information" };

    // and their fields can be normally accessed
    println!("The white box contains: {}", white.contents);

    // public structs with private fields cannot be constructed normally. i.e.:
    // let black = my::BlackBox{ contents: "private information" };  // error: field `contents` of struct `my::BlackBox` is private

    // however, public structs with private fields can be constructed ussing public constructors
    let _black = my::BlackBox::new("classified information");

    // the private fields (i.e. black.contents) still cannot be accessed though.

    // # use declaration
    {
        use my::nested::function as nesfun;
        nesfun();
    }

    println!("# indirect");
    my::indirect_call();
}
