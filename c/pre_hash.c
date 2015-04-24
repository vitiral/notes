
// Code from http://lolengine.net/blog/2011/12/20/cpp-constant-string-hash
#include <string.h>
#include <stdint.h>
#include <stdio.h>
#include <time.h>

#define H1(s,i,x)   (x*65599u+(uint8_t)s[(i)<strlen(s)?strlen(s)-1-(i):strlen(s)])
#define H4(s,i,x)   H1(s,i,H1(s,i+1,H1(s,i+2,H1(s,i+3,x))))
#define H16(s,i,x)  H4(s,i,H4(s,i+4,H4(s,i+8,H4(s,i+12,x))))
#define H64(s,i,x)  H16(s,i,H16(s,i+16,H16(s,i+32,H16(s,i+48,x))))
#define H256(s,i,x) H64(s,i,H64(s,i+64,H64(s,i+128,H64(s,i+192,x))))

#define HASH(s)    ((uint32_t)(H256(s,0,0)^(H256(s,0,0)>>16)))



#define short_str   ("short hash")
#define long_str    ("this is a pretty long hash string")

int main(void)
{
    uint32_t start;
    uint32_t h1, h2;
    start = clock();
    h1 = HASH(short_str);
    h2 = HASH(long_str);
    start = clock() - start;
    printf("clocks=%u\n", start);
    printf("short=0x%x\n", h1);
    printf("long=0x%x\n", h2);
}

