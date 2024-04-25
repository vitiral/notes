#include <errno.h>
#include <locale.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>
#include <assert.h>
#include <unistd.h>
#include <fcntl.h>


#include <lua.h>
#include <lualib.h>
#include <lauxlib.h>

typedef luaL_Stream LStream;

#define tolstream() ((LStream*)luaL_checkudata(L, 1, LUA_FILEHANDLE))
static LStream* lstream_create(lua_State *L) {
  LStream *st = (LStream*)lua_newuserdata(L, sizeof(LStream));
  st->closef = NULL;
  luaL_setmetatable(L, LUA_FILEHANDLE);
  return st;
}

static int l_fclose(lua_State* L) {
  LStream* st = tolstream();
  return luaL_fileresult(L, (fclose(st->f) == 0), NULL);
}

static LStream* ls_fdopen(lua_State *L, int fd, char* mode) {
  LStream* st = lstream_pre(L);
  st->f = fdopen(fd, mode);
  if(!st->f)
    luaL_error(L, "failed to open fd %I: %s", fd, strerror(errno));
  st->closef = l_fclose;
  return st;
}

static int l_iscfunction(lua_State *L) {
  lua_pushboolean(L, lua_iscfunction(L, 1));
  return 1;
}

static int l_isclosed(lua_State *L) {
  lua_pushboolean(L, (tolstream()->f) == NULL);
  return 1;
}

static int l_pipe(lua_State *L) {
  int rw[2]; assert(0 == pipe(rw));
  ls_fdopen(L, rw[0], "r");
  ls_fdopen(L, rw[1], "w");
  return 2;
}

static int l_nonblock(lua_State *L) {
  LStream* st = tolstream();
  fcntl(fileno(st->f), F_SETFL, O_NONBLOCK);
  return 0;
}

static int l_clearerrno(lua_State *L) {
  errno = 0; return 0;
}

static int l_errno(lua_State *L) {
  lua_pushinteger(L, errno);
  return 1;
}

static const struct luaL_Reg ioc[] = {
  {"iscfunction", l_iscfunction},
  {"pipe", l_pipe},
  {"nonblock", l_nonblock},
  {"isclosed", l_isclosed},
  {"clearerrno", l_clearerrno}, {"errno", l_errno},
  {NULL, NULL} // sentinel
};

int luaopen_ioc(lua_State *L) {
  luaL_newlib(L, ioc);
  return 1;
}
