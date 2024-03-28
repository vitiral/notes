UNAME    := $(shell uname)
LUA_ARGS := $(shell pkg-config --cflags --libs lua)

ifeq ($(UNAME), Linux)
  LIBTYPE = shared
  LIBEXT  = so
else ifeq ($(UNAME), Darwin)
  LIBTYPE = dynamiclib
  LIBEXT  = dylib
endif
	
test: build
	lua test.lua
	
build: pear.c
	$(CC) -$(LIBTYPE) $+ -o pear.$(LIBEXT) $(LUA_ARGS)
