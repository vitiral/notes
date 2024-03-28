UNAME    := $(shell uname)
LUA_ARGS := $(shell pkg-config --cflags --libs lua)

CC_ARGS =
ifeq ($(UNAME), Linux)
LIBTYPE = shared
LIBEXT  = so
else ifeq ($(UNAME), Darwin)
LIBTYPE = dynamiclib
LIBEXT  = dylib
CC_ARGS = -current_version 1.0 -compatibility_version 1.0
endif
	
test: build
	lua test.lua
	
build: pear.c
	cc -$(LIBTYPE) pear.c -o pear.$(LIBEXT) $(CC_ARGS) $(LUA_ARGS)
