(module
  (memory 1) ;; 64KiB of memory
  (export "main" (func $main))
  (func $main (result i32)
        call $sum_local
  )

  ;; Wasm is a stack-based language, but for returning values more complicated
  ;; than an int/float, a separate memory stack has to be manually managed. One
  ;; approach is to use a mutable global to store the stack_ptr. We give
  ;; ourselves 1MiB of memstack and grow it downwards.
  ;;
  ;; Below is a demonstration of how this C code **might** be written by hand
  ;;
  ;;   typedef struct {
  ;;       int a;
  ;;       int b;
  ;;   } sum_struct_t;
  ;;
  ;;   sum_struct_t sum_struct_create(int a, int b) {
  ;;     return (sum_struct_t){a, b};
  ;;   }
  ;;
  ;;   int sum_local() {
  ;;     sum_struct_t s = sum_struct_create(40, 2);
  ;;     return s.a + s.b;
  ;;   }

  ;; Unlike C, we must manage our own memory stack. We reserve 1MiB
  (global $memstack_ptr (mut i32) (i32.const 65536))

  ;; Structs can only be returned by reference
  (func $sum_struct_create 
        (param $sum_struct_ptr i32) 
        (param $var$a i32) 
        (param $var$b i32)
    ;; c// sum_struct_ptr->a = a;
    (i32.store
      (get_local $sum_struct_ptr)
      (get_local $var$a)
    )

    ;; c// sum_struct_ptr->b = b;
    (i32.store offset=4
      (get_local $sum_struct_ptr)
      (get_local $var$b)
    )
  )

  (func $sum_local (result i32)
    (local $var$sum_struct$a i32)
    (local $var$sum_struct$b i32)
    (local $local_memstack_ptr i32)

    ;; reserve memstack space
    (i32.sub
      (get_global $memstack_ptr)
      (i32.const 8)
    )
    tee_local $local_memstack_ptr ;; tee both stores and returns given value
    set_global $memstack_ptr

    ;; call the function, storing the result in the memstack
    (call $sum_struct_create
      ((;$sum_struct_ptr=;) get_local $local_memstack_ptr)
      ((;$var$a=;) i32.const 40)
      ((;$var$b=;) i32.const 2)
    )

    ;; retrieve values from struct
    (set_local $var$sum_struct$a
      (i32.load offset=0 (get_local $local_memstack_ptr))
    )
    (set_local $var$sum_struct$b
      (i32.load offset=4 (get_local $local_memstack_ptr))
    )

    ;; unreserve memstack space
    (set_global $memstack_ptr
        (i32.add
          (get_local $local_memstack_ptr)
          (i32.const 8)
        )
    )

    (i32.add
      (get_local $var$sum_struct$a)
      (get_local $var$sum_struct$b)
    )
  )
  (export "sum_local" (func $sum_local))
)
