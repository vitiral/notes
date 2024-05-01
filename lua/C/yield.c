#include <stdio.h>
#include <stdlib.h>

#include <lua.h>
#include <lualib.h>
#include <lauxlib.h>

typedef lua_State LS;

int l_yielder(LS* L) {
  printf("Yielding\n");
  return lua_yield(L, 0);
}


static const struct luaL_Reg yield[] = {
  {"errno", l_errno},
  {NULL, NULL} // sentinel
};

int luaopen_ioc(LS *L) {
  luaL_newlib(L, yield);
  return 1;
}

int main() {
  printf("Yield\n");
  return 0;
}
