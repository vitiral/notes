#include <stdio.h>
#include <stdint.h>

struct A { // total size: want 5 bytes (actually 8)
  uint32_t a0; // 4 bytes
  uint8_t a1;  // 1 byte
};

struct B { // total size: want 8 bytes (actually 12)
  struct A b0; // 6 bytes (+ 1 alignment)
  uint16_t b1; // 2 bytes
};

// Essentially just unroll(A) + B
struct AB { // total size: 8 bytes (actually 8)
  uint32_t a0; // 4 bytes
  uint8_t  a1; // 1 byte (+ 1 alignment)
  uint16_t b1; // 2 bytes
};

// Correct size but doesn't respect alignment
struct __attribute__ ((__packed__)) Ap {
  uint32_t a0; // 4 bytes
  uint8_t a1;  // 1 byte
};

// Size one less, doesn't respect alignmen
struct __attribute__ ((__packed__)) Bp {
  struct Ap b0;
  uint16_t  b1;
};

// Manually packed fields
struct Bm {
  __attribute__ ((__packed__ )) struct A b0;
  uint16_t b1;
};

struct Show {
  __attribute__ ((__packed__)) struct Bm bm;
  uint16_t u16;
};

// Using __attribute__ ((__packed__, aligned(4)))
// seems to result in same result as not using
// any attributes

int main() {
  printf("sizeof(A)    = %u\n", sizeof(struct A));    // 8 (not 5)
  printf("sizeof(B)    = %u\n", sizeof(struct B));    // 12 (not 8)
  printf("sizeof(AB)   = %u\n", sizeof(struct AB));   // 8 (same as desired)
  printf("sizeof(Ap)   = %u\n", sizeof(struct Ap));   // 5 (as desired)
  printf("sizeof(Bp)   = %u\n", sizeof(struct Bp));   // 7 (not 8)
  printf("sizeof(Bm)   = %u\n", sizeof(struct Bm));   // 10 (not 8)
  printf("sizeof(Show) = %u\n", sizeof(struct Show)); // 12 (as desired)
  return 0;
}


#define STRUCT_A  \
  uint32_t a0; \
  uint8_t a1

struct ABmacro {
  STRUCT_A;    // 6 bytes
  uint16_t b1; // 2 bytes
};

