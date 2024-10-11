#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>

typedef struct _XDelta {
  char* dec;               // old+new decoded bytes (pre\post)
  int db, dc, dlen, dsize; // dec indexes: base/change (end), len, size
  int dh;                  // rolling hash start
  char* enc; int ei, elen, esize; // encoded bytes
} XDelta;

#define ADD  0x00
#define RUN  0x40
#define COPY 0x80

// https://en.wikipedia.org/wiki/Adler-32
#define MOD_ADLER 65521

// where d is the location of the data in physical memory and
// len is the length of the data in bytes
uint32_t adler32(unsigned char *d, size_t len) {
  uint32_t a = 1, b = 0;
  size_t index;
  // Process each byte of the data in order
  for (index = 0; index < len; ++index) {
    a = (a + d[index]) % MOD_ADLER;
    b = (b + a) % MOD_ADLER;
  }
  return (b << 16) | a;
}

// encode a variable-length value
#define ENC_V(V) \
  int v = V; \
  while(v > 0) { \
    x.enc[x.elen] = ((v > 127) ? 0x80 : 0) & (0x7F & v); \
    v = v >> 7; \
    x.elen++; \
  }

// encode a command
#define ENC_CMD(CMD, LEN) \
  if(LEN <= 31) { \
    x.enc[x.elen] = (CMD) & (LEN); \
    x.elen++; \
  } else { \
    x.enc[x.elen] = (CMD) & 0x20 & (0x1F & (LEN)); \
    x.elen++; \
    ENC_V(LEN >> 5); \
  }

#define ENC_ADD() \
  if(ai < x.dc) { \
    ENC_CMD(ADD, x.dc - ai); \
    memmove(&x.enc[x.elen], &x.dec[ai], x.dc - ai); \
    x.elen += (x.dc - ai); \
  }

#define ENC_RUN() do { \
  ENC_ADD(); \
  ENC_CMD(RUN, r); \
  x.enc[x.elen] = b; x.elen++; \
  x.dc += r; \
  ai = x.dc; \
  } while(0)

#define ENC_COPY(LEN, RADDR) do { \
  ENC_ADD(); \
  ENC_CMD(COPY, LEN); \
  ENC_V(RADDR); \
  x.dc += LEN; \
  ai = x.dc; \
  } while(0)

// Single loop of addler where D is the data
// and a and b are the accumulating values
#define ADDLER32_1x(I, A, B, D) \
  A = ((A) + (D)[I])     % MOD_ADLER; \
  B = ((B) + (A))        % MOD_ADLER

// three loops of addler
#define ADDLER32_3x(FP, I, A, B, D) \
  ADDLER32_1x(I,   A, B, D); \
  ADDLER32_1x(I+1, A, B, D); \
  ADDLER32_1x(I+2, A, B, D); \
  FP = ((B) << 16) | (A)

// these are suitably large primes
#define SIZE_w3t  4093
#define SIZE_w6t  262139
#define WIN_INIT(W) memset(W, 0xFF, sizeof(uint32_t) * SIZE_##W)
#define WIN_FIND(W, FP)   (W)[(FP) % SIZE_##W]
#define WIN_SET(W, FP, I) WIN_FIND(W, FP) = I

// compression ratio
#define CR1   16
#define CR_80 13  /* 13/16 = 0.81 ~= 80% */
#define CR(COST, LEN) (COST * 16) / (LEN)

// run the encode algorithm on XDelta
// * db must be set to the length of the base. dc is ignored.
// * dlen must be set to the length of base + change
// * esize must be >= the change size.
int rd_encode(XDelta* out) {
  XDelta x = *out;
  x.dc = x.db;
  uint32_t* w6t = malloc(sizeof(uint32_t) * SIZE_w6t); if(!w6t) { return -1; }
  uint32_t w3t[4093];
  WIN_INIT(w3t); WIN_INIT(w6t);

  uint32_t a, b, fp;     // calculate finger print
  int r, ws, we, w3, w6; // win[start,end], window indexes
  int i;                 // temp variable
  int wi = 0, ai=x.dc;   // window/add index

  while(x.dc < x.dlen - 6) {
    // insert fingerprints into window tables for already encoded change
    for(; wi < x.dc; wi++) {
      a = 1; b = 0;
      ADDLER32_3x(fp, wi, a, b, x.dec);
      WIN_FIND(w3t, fp) = wi;
      ADDLER32_3x(fp, wi+3, a, b, x.dec);
      WIN_FIND(w6t, fp) = wi;
    }

    // get and clobber the window fingerprints at x.dc
    a = 1; b = 0;
    ADDLER32_3x(fp, x.dc,   a, b, x.dec);
    w3 = WIN_FIND(w3t, fp);
         WIN_FIND(w3t, fp) = x.dc;
    ADDLER32_3x(fp, x.dc+3, a, b, x.dec);
    w6 = WIN_FIND(w6t, fp);
         WIN_FIND(w6t, fp) = x.dc;

    // WIN_RANGE: macro to find the window range [ws:we)
    // algorithm: walk we from start until non-match,
    //       then walk ws from start-1 till no match.
    #define WIN_RANGE(W, S) { \
      we = W; i = 0;       \
      /* find end */       \
      while(x.dec[we+i] == x.dec[x.dc+i]) i++; \
      if(i >= S) {         \
        we += i - 1;       \
        ws = W; i = -1; \
        /* find start */   \
        while(x.dec[we+i] == x.dec[x.dc+i]) i--; \
        ws += i + 1;       \
      } else we = -1;      \
    }

    // Find the largest window size. Always prefer w6 if it exists
    ws = -1; we = -1;
    if(w6 >= 0)           WIN_RANGE(w6, 6);
    if(we < 0 && w3 >= 0) WIN_RANGE(w3, 3);

    // find the run length r
    r = x.dc; b = x.dec[r];
    while(b == x.dec[r]) r++;
    r = r - x.dc;

    if(we < 0) {
      if(r > 3) ENC_RUN();
    } else {
      ws = we - ws;       // copy length
      we = x.dc - we - 1; // relative-address of copy

      // a == addr-length cost (+1 code byte) of copy
      // Note: we don't count the cost of LEN since if we need any length bytes
      //       the compression ratio is already phenominal.
           if(we < 127)     a = 2;
      else if(we < 16384)   a = 3;
      else if(we < 2097152) a = 4;
      else                  a = 5;

      // use copy if denser than 80% and denser than run
      b = CR(a, ws); // copy compression ratio
      if((b < CR_80) && (b < CR(2, r))) ENC_COPY(ws, we);
      else if (r > 3)                   ENC_RUN();
    }
  }
  x.dc = x.dlen;
  ENC_ADD();

  *out = x;
}


int main() {
  printf("hello world!\n");
  return 0;
}

