#include <ctype.h>
#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <termios.h>
#include <unistd.h>

struct termios orig_termios;

void die (const char *s) {
  perror(s);
  exit(1);
}

void disableRawMode() {
  if (tcsetattr(STDIN_FILENO, TCSAFLUSH, &orig_termios) == -1) {
    die ("tcsetattr");
  }
}

void enableRawMode() {
  struct termios raw;

  if (tcgetattr(STDIN_FILENO, &raw) == -1) {
    die ("tcgetattr");
  }
  orig_termios = raw;
  atexit(disableRawMode);
  // flags that are changing
  raw.c_iflag &= ~(ICRNL | IXON);
  raw.c_oflag &= ~(OPOST);
  raw.c_lflag &= ~(ECHO | ICANON | IEXTEN | ISIG);

  // flags thare are already likely disabled
  raw.c_iflag &= ~(BRKINT | INPCK | ISTRIP);
  raw.c_cflag |= (CS8);

  raw.c_cc[VMIN] = 0;
  raw.c_cc[VTIME] = 1;
  if (tcsetattr(STDIN_FILENO, TCSAFLUSH, &raw)) {
    die ("tcsetattr");
  }
}

int main() {
  enableRawMode();

  while (1) {
    char c = '\0';
    if(read(STDIN_FILENO,&c, 1) == -1 && errno != EAGAIN) {
      die ("read stdin");
    }
    if (c == '\0') {
    } else if (iscntrl(c)) {
      printf("%d\r\n", c);
    } else {
      printf("%d ('%c')\r\n", c, c);
    }
    if (c == 'q') break;
  }
  return 0;
}
