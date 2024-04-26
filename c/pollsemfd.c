#include <stdlib.h>
#include <stdio.h>
#include <assert.h>
#include <unistd.h>
#include <pthread.h>
#include <fcntl.h> // open()
#include <poll.h>
#include <errno.h>

#include <sys/eventfd.h>

typedef struct _s {
  int fd; int semfd;
} S;

static uint64_t EFD_WRITE = 0xfffffffffffffffeL;
uint64_t unused;

// #define SIZE 0x4000     /*16 KiB*/
#define SIZE 0x1000000  /*16 MiB*/
void* readThenWriteForever(void* v) {
  S* s = (S*)v; int fd = s->fd;
  printf("readThenWrite: fd=%i semfd=%i\n", s->fd, s->semfd);
  char* buf = malloc(SIZE); assert(buf);
  while(1) {
    int c = read(s->semfd, &unused, 8);
    printf("thread unlocked c=%i\n", c);
    assert(c == 8);
    assert(write(fd, buf, SIZE)   == SIZE);
    assert(lseek(fd, 0, SEEK_SET) == 0);
    assert(read(fd, buf, SIZE)    == SIZE);
    assert(lseek(fd, 0, SEEK_SET  == 0));
  }
  return NULL;
}

void* runpoll(void* v) {
  S* s = (S*)v;
  struct pollfd pfd = { .fd = s->semfd, .events = POLLIN | POLLOUT };
  while(1) {
    poll(&pfd, 1, 0);
    printf("poll %X\n", pfd.revents);
    pfd.revents = 0;
  }
}


int main() {
  pthread_t th1, th2;
  S s = {
    .fd = open("stuff.bin", O_RDWR | O_CREAT | O_TRUNC, 0600),
    .semfd = eventfd(0, 0),
  };
  assert(s.fd >= 0); assert(s.semfd >= 0);
  printf("opened fd=%i semfd=%i POLLIN=%X POLLOUT=%X\n",
         s.fd, s.semfd, POLLIN, POLLOUT);
  assert(!pthread_create(
    &th1, NULL, readThenWriteForever, (void*)&s));
  assert(!pthread_create(
    &th2, NULL, runpoll, (void*)&s));

  while(1) {
    int c = write(s.semfd, &EFD_WRITE, 8);
    printf("main unlocked c=%i\n", c);
    assert(c == 8);
  }

  return 0;
}
