OBJS = tutorial.cpp \
  libs/evt2str/sdl_event_to_string.*
CC = g++
COMPILER_FLAGS = -w
LINKER_FLAGS = \
  -lSDL2 -lSDL2_image
OBJ_NAME = game  # output name

run: test
	./bin/$(OBJ_NAME)

test: build
	./bin/$(OBJ_NAME) --test

build : $(OBJS)
	mkdir -p bin/
	$(CC) $(OBJS) $(COMPILER_FLAGS) $(LINKER_FLAGS) -o bin/$(OBJ_NAME)

