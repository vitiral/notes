#include <lua.h>
#include <lualib.h>
#include <lauxlib.h>

#ifndef luaL_newlib
#define luaL_newlib(L, l) (lua_newtable(L), luaL_register(L, NULL, l))
#endif

int l_mul(lua_State *L) {
  lua_Integer n1 = luaL_checkinteger(L, 1);
  lua_Integer n2 = luaL_checkinteger(L, 2);
  lua_pushinteger(L, n1 * n2);
  return 1;
}

static const struct luaL_Reg pear[] = {
  {"mul", l_mul},
  {NULL, NULL} // sentinel
};

int luaopen_pear(lua_State *L) {
  luaL_newlib(L, pear);
  return 1;
}
