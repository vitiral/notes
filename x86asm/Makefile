
# run with: make exit && ./out/exit ; echo "rc=$?"
exit: exit.S
	@as exit.S -o out/exit.o
	@ld out/exit.o -o out/exit

# run with: make largest && ./out/largest ; echo "rc=$?"
largest: largest.S
	@as largest.S -o out/largest.o
	@ld out/largest.o -o out/largest

# run with: make power && ./out/power ; echo "rc=$?"
power: power.S
	@as power.S --32 -o out/power.o
	@ld out/power.o -m elf_i386 -o out/power

# run with: make toupper && ./out/toupper ; echo "\nrc=$?"
toupper: toupper.S
	@as toupper.S --32 -o out/toupper.o
	@ld out/toupper.o -m elf_i386 -o out/toupper

