
#include <stdlib.h>
#include <stdio.h>
#include <signal.h>
#include <execinfo.h>
#include <assert.h>

#define TRACE_DEPTH 100
#define V_SIZE      40000000

char **messages = (char**)NULL;

void doBacktrace() {
  void* trace[TRACE_DEPTH];
  ssize_t len = backtrace(trace, TRACE_DEPTH);
  messages = backtrace_symbols(trace, len);
  printf("Backtrace:\n");
  for(ssize_t i = 1; i < len; i++) {
    printf("  %p: %s\n", (void*)i, messages[i]);
  }
  free(messages);
}

void a() { doBacktrace(); }
void b() { a(); }
void c() { b(); }
void d() { c(); }
void e() { d(); }
void f() { e(); }
void g() { f(); }
void h() { g(); }
void i() { h(); }

void callFns() {
  i();
}

#define initV(V) \
  ssize_t* V = malloc(sizeof(ssize_t) * V_SIZE); \
  assert(V); \
  for(ssize_t i = 0; i < V_SIZE; i++) V[i] = i;


#define assertV(V) \
  for(ssize_t i = 0; i < V_SIZE; i++) { \
    if(i != V[i]) { \
      printf("failed at index=%p  value=%p\n", (void*)i, (void*)V[i]); \
    } \
  }

int main() {
  printf("fail start\n");

  initV(v1);
  initV(v2);
  initV(v3);
  initV(v4);

  callFns();

  assertV(v1);
  assertV(v2);
  assertV(v3);
  assertV(v4);
  printf("fail didn't fail\n");

}
