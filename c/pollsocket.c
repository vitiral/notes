#include <stdlib.h>
#include <stdio.h>
#include <assert.h>
#include <unistd.h>
#include <pthread.h>
#include <fcntl.h>
#include <poll.h>
#include <errno.h>

#include <sys/socket.h>


typedef struct _s {
  int fd;
  int socks[2];
} S;

char ZERO = 0;

// #define SIZE 0x4000     /*16 KiB*/
#define SIZE 0x1000000  /*16 MiB*/
void* readThenWriteForever(void* v) {
  S* s = (S*)v; int fd = s->fd;
  printf("readThenWrite: fd=%i socks=%i %i\n",
         s->fd, s->socks[0], s->socks[1]);
  char* buf = malloc(SIZE); assert(buf);
  while(1) {
    int c = read(s->socks[0], &ZERO, 1); // await signal
    printf("thread unlocked c=%i\n", c);
    assert(c == 1);
    assert(write(fd, buf, SIZE)   == SIZE);
    assert(lseek(fd, 0, SEEK_SET) == 0);
    assert(read(fd, buf, SIZE)    == SIZE);
    assert(lseek(fd, 0, SEEK_SET  == 0));
    c = write(s->socks[0], &ZERO, 1); // notify completion
    assert(c == 1);
  }
  return NULL;
}

void* runpoll(void* v) {
  S* s = (S*)v;
  struct pollfd pfd[2] = {
    { .fd = s->socks[0], .events = POLLIN },
    { .fd = s->socks[1], .events = POLLIN },
  };
  int was[2];
  int count = 0;
  while(1) {
    poll(pfd, 2, 0);
    if((pfd[0].revents != was[0]) || (pfd[1].revents != was[1])) {
      was[0] = pfd[0].revents; was[1] = pfd[1].revents;
      printf("poll %u: %X %X\n", count, was[0], was[1]);
      count = 0;
    }
    pfd[0].revents = 0; pfd[1].revents = 0;
    count ++;
  }
}


int main() {
  pthread_t th1, th2;
  S s = {
    .fd = open("stuff.bin", O_RDWR | O_CREAT | O_TRUNC, 0600),
  };
  assert(0 == socketpair(AF_UNIX, SOCK_STREAM, 0, s.socks));
  assert(s.fd >= 0); assert(s.socks[0] >= 0);
  printf("opened fd=%i socks=%i %i POLLIN=%X POLLOUT=%X\n",
         s.fd, s.socks[0], s.socks[1], POLLIN, POLLOUT);
  assert(!pthread_create(
    &th1, NULL, readThenWriteForever, (void*)&s));
  assert(!pthread_create(
    &th2, NULL, runpoll, (void*)&s));

  while(1) {
    int c = write(s.socks[1], &ZERO, 1);
    assert(c == 1);

    c = read(s.socks[1], &ZERO, 1);
    assert(c == 1);
  }

  return 0;
}

