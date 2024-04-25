
#include <assert.h>
#include <string.h>
#include <stdio.h>
#include <unistd.h>
#include <fcntl.h>
#include <errno.h>



int main() {
  int rw[2]; assert(!pipe(rw));
  int r = rw[0], w = rw[1];
  char buf[256];

  // printf("This will hang forever...\n");
  // read(r, buf, 10);

  fcntl(r, F_SETFL, O_NONBLOCK);
  printf("This will return EAGAIN\n");
  printf(" read -> %i\n", read(r, buf, 10));
  printf(" errno = %i \"%s\" (EAGAIN=%i)\n", errno, strerror(errno), EAGAIN);

  printf("Errno has the answers!\n");
  FILE* f = fdopen(r, "r");
  errno = 0;
  printf(" errno has been set to %i\n", errno);
  printf(" fgetc -> %i (EOF=%i)\n", fgetc(f), EOF);
  printf(" errno=%i (pre ferror)\n", errno);
  printf(" ferror -> %i\n", ferror(f));
  printf(" errno=%i\n", errno);
}

