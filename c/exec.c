#include <stdlib.h>
#include <stdio.h>
#include <assert.h>
#include <string.h>
#include <unistd.h>   // Shell


int main(int argc, char** argv) {
  int rw[2]; assert(!pipe(rw));
  int pr_r = rw[0], ch_w = rw[1];
  int pid = fork(); assert(pid != -1);
  if(pid == 0) {
    close(pr_r);
    dup2(ch_w, STDOUT_FILENO); close(ch_w);
    fprintf(stderr, "!! child executing\n");
    for(char** arg = (char**)&argv[1]; *arg; arg++) {
      fprintf(stderr, "!!   arg: %s\n", *arg);
    }
    // printf("-- child printf\n");
    execvp(argv[1], &argv[1]);
    fprintf(stderr, "!! past execvp\n");
    return 99;
  } // parent
  close(ch_w);
  fprintf(stderr, "!! parent waiting\n");
  char buf[256]; int cbuf = read(pr_r, &buf, 255); buf[cbuf] = 0;
  fprintf(stdout, "child wrote: \"%s\"\n", buf);
  siginfo_t infop = {0}; waitid(P_PID, pid, &infop, WEXITED);
  fprintf(stderr, "!! parent waited: %i\n", infop.si_status);
  return infop.si_status;
}
