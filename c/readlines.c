
#include <assert.h>
#include <string.h>
#include <stdio.h>
#include <stdlib.h>

int main() {
  char* dat = NULL; size_t len;
  printf("Enter some text, EXIT to exit\n");
  while(1) {
    len = getline(&dat, &len, stdin);
    printf("You entered [%u]: %s", len, dat);
    if(0 == strcmp("EXIT", dat)) break;
  }
  free(dat);
  return 0;
}
