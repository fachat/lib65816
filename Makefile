# not the most sophisticated Makefile

# add -DWORDS_BIGENDIAN if necessary
CC = gcc -g
#CC = vc +i386-linux

all: build/lib65816.a
	$(CC) -Ilib65816 -I. main.c src/*.c -o emu65816

build/lib65816.a:
	cmake -S . -B build
	(cd build; make CCOPTS='-DDEBUG')

clean:
	rm -r build
