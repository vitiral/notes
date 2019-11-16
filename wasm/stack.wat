(; C code
// https://mbebenita.github.io/WasmExplorer/
#include <stddef.h>

typedef struct {
    int a;
    int b;
} sum_struct_t;

__attribute__ ((noinline))
sum_struct_t sum_struct_create(int a, int b) {
  return (sum_struct_t){a, b};
}

int call_it() {
  sum_struct_t s = sum_struct_create(40, 2);
  return s.a + s.b;
}
;)

(module
  (type $type0 (func (param i32 i32 i32)))
  (type $type1 (func (result i32)))
  (table 0 anyfunc)
  (memory 1)
  (global $stack_ptr (mut i32) (i32.const 65536))
  ;; (export "memory" memory)
  ;; (export "sum_struct_create" $func0)
  (export "main" (func $main))
  (func $sum_struct_create (param $var0 i32) (param $var1 i32) (param $var2 i32)
    get_local $var0
    get_local $var2
    i32.store offset=4
    get_local $var0
    get_local $var1
    i32.store
  )
  (func $sum_local (result i32)
    (local $var0 i32) (local $var1 i32) (local $local_stack i32) 
    ;; i32.const 0
    ;; i32.load offset=4
    (i32.sub
      (get_global $stack_ptr)
      (i32.const 16)
    )
    tee_local $local_stack
    set_global $stack_ptr

    get_local $local_stack
    i32.const 8
    i32.add
    i32.const 40
    i32.const 2
    call $sum_struct_create
    get_local $local_stack
    i32.load offset=8
    set_local $var0
    get_local $local_stack
    i32.load offset=12
    set_local $var1
    i32.const 0
    get_local $local_stack
    i32.const 16
    i32.add
    i32.store offset=4
    get_local $var1
    get_local $var0
    i32.add
  )
  (func $main (result i32)
        ;; (i32.store (i32.const 0) (i32.const 64000))
        ;; "stack" is at index 4
        (i32.store (i32.const 4) (i32.const 63000))
        call $sum_local
  )
)
