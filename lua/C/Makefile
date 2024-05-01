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

pear: pear.c
	gcc pear.c -c -Wall -llua5.3 -I/usr/include/lua5.3
	gcc pear.o -shared -o pear.so
	lua test.lua

ioc: ioc.c
	gcc ioc.c -c -Wall -llua5.3 -I/usr/include/lua5.3
	gcc ioc.o -shared -o ioc.so
	lua try_ioc.lua

yield: yield.c
	gcc yield.c -c -Wall -llua5.3 -I/usr/include/lua5.3
	gcc yield.o -shared -o yield.so
	lua yield.lua
