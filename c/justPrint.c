#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <termios.h>

// print to stderr
#define eprint(str)   fprintf (stderr, str)
#define eprintf(format, args...)   fprintf (stderr, format, args)

char buf[8];

void main() {
  eprint("Starting main\n");

  while (fread(buf, 1, 1, stdin)) {
    // eprintf("[C stderr] : %c (0x%X)\n", buf[0], buf[0]);
     printf("[C stdout] : %c (0x%X)\n", buf[0], buf[0]);
    fflush(stderr);
    fflush(stdout);
  }
  eprint("Done.\n");
}

