#include <stdio.h>
#include <stdlib.h>

static unsigned long long mulIntOver(long long a, long long b) {
  return abs(a) * abs(b);
}

int main() {
  printf("5 * 5: %i\n", mulIntOver(5, 5));
  printf("1024 * 1024: %i\n", mulIntOver(1024, 1024));
  printf("-1024 * -1024: %i\n", mulIntOver(-1024, -1024));
  long long v = (1ULL << 8) * 30323432432;
  printf("v: %llu\n", v);
  printf("v * v: %llu\n", mulIntOver(v, v));

}
