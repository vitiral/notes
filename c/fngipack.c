#include <stdio.h>
#include <stdint.h>

struct __attribute__(( __packed__ )) A {
  uint32_t a0; // 4 bytes
  uint8_t a1;  // 1 byte
};

struct __attribute__(( __packed__ )) B {
  struct A b0;  uint8_t  __align0[1];
  uint16_t b1; // 2 bytes
};

struct __attribute__(( __packed__ )) C { // total size:
  struct A c0;  uint8_t __align0[3];
  struct A c1;
};

// Use this inside arrays, otherwise arrays will be mis-aligned.
struct __attribute__(( __packed__ )) C_El { struct C el; uint8_t __align0[3]; };

int main() {
  printf("sizeof(A)    = %u\n", sizeof(struct A));    // 5
  printf("sizeof(B)    = %u\n", sizeof(struct B));    // 8
  printf("sizeof(C)    = %u\n", sizeof(struct C));    // 13

  struct C arrC[4]; ssize_t arr0 = (ssize_t)arrC;
  printf("C index pointers: %u, %u\n", 0, (ssize_t)(&arrC[1]) - arr0);

  struct C_El arrC_El[4]; ssize_t arr0el = (ssize_t)arrC_El;
  printf("C_El index pointers: %u, %u\n", 0, (ssize_t)(&arrC_El[1]) - arr0el);
  return 0;
}

