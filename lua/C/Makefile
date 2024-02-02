
test: build
	lua test.lua
	
build: pear.c
	gcc pear.c -c -Wall -llua5.3 -I/usr/include/lua5.3
	gcc pear.o -shared -o pear.so
