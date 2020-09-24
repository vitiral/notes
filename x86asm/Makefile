
exit: exit.S
	@as exit.S -o out/exit.o
	@ld out/exit.o -o out/exit

largest: largest.S
	@as largest.S -o out/largest.o
	@ld out/largest.o -o out/largest

power: power.S
	@as power.S --32 -o out/power.o
	@ld out/power.o -m elf_i386 -o out/power
