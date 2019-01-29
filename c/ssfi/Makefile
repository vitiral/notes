OBJECTS = cmd.o trie.o parse.o work.o common.o topten.o
TEST_OBJECTS = test.o unity.o $(OBJECTS)
MAIN_OBJECTS = main.o $(OBJECTS)
UNITY_H = Unity/src/unity.h
UNITY = $(UNITY_H) Unity/src/unity.c Unity/src/unity_internals.h

# Compile the program.
# Note: must be first.
ssfi: $(MAIN_OBJECTS)
	make clean
	CFLAGS="-Wall -Werror -g0 -O3" make ssfi_bin \
		&& printf "\033[0;32m\n--- SUCCESS ---\n\033[0m"

run: ssfi
	mkdir -p bin/
	./bin/ssfi data/simple -f

test:
	make clean
	CFLAGS="-DTESTS_RUN -Wall -Werror -g" make tests_bin
	printf '\n\n --- RUNNING TESTS ----\n'
	valgrind                            \
		--error-exitcode=1              \
		--leak-check=full               \
		./bin/run_tests                 \
		&& printf "\033[0;32m\n--- SUCCESS ---\n\033[0m" \
		|| exit 1
	make
	python3 -m ssfi_py --test                            \
		&& printf "\033[0;32m\n--- SUCCESS ---\n\033[0m" \
		|| exit 1

	valgrind                                             \
		--error-exitcode=1                               \
		--leak-check=full                                \
		./bin/ssfi data/random-shallow -t 16             \
		&& printf "\033[0;32m\n--- SUCCESS ---\n\033[0m" \
		|| exit 1


.PHONY: fmt
fmt:
	clang-format -i *.c *.h

.PHONY: tests_bin
tests_bin: $(TEST_OBJECTS)
	mkdir -p bin/
	cc -o bin/run_tests $(TEST_OBJECTS) -lrt

.PHONY: ssfi_bin
ssfi_bin: $(MAIN_OBJECTS)
	mkdir -p bin/
	cc -o bin/ssfi $(MAIN_OBJECTS) -lrt

.PHONY: clean
clean:
	-rm -f ssfi $(MAIN_OBJECTS) $(TEST_OBJECTS)
	-rm -f *.o
	-rm -f bin/*
	-rm -f *.gch

# Note: implicitly uses `cc -c main.c $(deps)`
test.o: unity.o
main.o: cmd.h
cmd.o: cmd.h parse.h topten.h
topten.o: topten.h trie.h
parse.o: parse.h work.h
work.o: work.h
trie.o: trie.h common.h
common.o: common.h

################################################################################
# EXTERNAL DEPS
unity.o: $(UNITY)
	cc -c $(UNITY)

