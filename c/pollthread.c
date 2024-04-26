// Checking polling a CURRENTLY reading/writing filedescriptor (from a separate
// thread)
//
// My assumption was that this would result in the file descriptor
// not being read/writeable.
//
// Result: My assumption was wrong. The following manpage documentation is
// followed to the letter, even in situations where the fd isn't actually
// available:
//   poll()ing regular files, block devices, and other files
//   with no reasonable polling semantic always returns instantly as
//   ready to read and write.

#include <stdlib.h>
#include <stdio.h>
#include <assert.h>
#include <unistd.h>
#include <pthread.h>
#include <fcntl.h> // open()
#include <poll.h>
#include <errno.h>

// #define SIZE 0x4000     /*16 KiB*/
#define SIZE 0x1000000  /*16 MiB*/
void* readThenWriteForever(void* v) {
  int fd = *(int*)v;
  printf("readThenWrite: fd=%i\n", fd);
  char* buf = malloc(SIZE); assert(buf);
  // sleep(1);
  while(1) {
    assert(write(fd, buf, SIZE)   == SIZE);
    assert(lseek(fd, 0, SEEK_SET) == 0);
    assert(read(fd, buf, SIZE)    == SIZE);
    assert(lseek(fd, 0, SEEK_SET  == 0));
  }
  return NULL;
}

int main() {
  pthread_t th;
  int fd = open("stuff.bin", O_RDWR | O_CREAT | O_TRUNC, 0600);
  assert(fd >= 0);
  printf("opened fd=%i POLLIN=%X POLLOUT=%X\n", fd, POLLIN, POLLOUT);
  assert(fd);
  struct pollfd pfd = { .fd = fd, .events = POLLIN | POLLOUT };
  assert(!pthread_create(
    &th, NULL, readThenWriteForever, (void*)&fd));

  while(1) {
    poll(&pfd, 1, 0);
    printf("poll %X\n", pfd.revents);
    pfd.revents = 0;
  }

  return 0;
}
