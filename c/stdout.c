#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <termios.h>


// print to stderr
#define eprint(str)   fprintf (stderr, str)
#define eprintf(format, args...)   fprintf (stderr, format, args)


void initTermios() {
    struct termios t;

    tcgetattr(0, &t);     // get existing
    t.c_lflag &= ~ICANON; // disable buffer
    t.c_lflag &= ~ECHO;   // disable echo
    tcsetattr(0, TCSANOW, &t); // use settings now
}

char buf[8];

void main() {
  initTermios();

  eprint("Starting main\n");

  while (fread(buf, 1, 1, stdin)) {
    eprintf("C stdin: %c (0x%X)\n", buf[0], buf[0]);
    // printf("C stdin: %c (0x%X)\n", buf[0], buf[0]);
  }
  eprint("Done.\n");
}
