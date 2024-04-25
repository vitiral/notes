
#include <unistd.h>
#include <stdio.h>
#include <assert.h>


int main() {
  FILE* f = tmpfile();
  fprintf(f, "This is nice isn't it?\n");
  int fd = dup(fileno(f)); assert(fd >= 0);
  assert(fileno(f) != fd);
  fclose(f);
  lseek(fd, 0, SEEK_SET);
  char buf[256]; int c = read(fd, buf, 255);
  assert(c > 0);
  printf("read: %s", buf);
  return 0;
}
