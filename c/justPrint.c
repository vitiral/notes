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
    eprintf("[stderr] C stdin: %c (0x%X)\n", buf[0], buf[0]);
    printf("[stdout] C stdin: %c (0x%X)\n", buf[0], buf[0]);
  }
  eprint("Done.\n");
}

