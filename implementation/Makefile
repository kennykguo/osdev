# Define source and header files
C_SOURCES = $(wildcard kernel/*.c drivers/*.c)
HEADERS = $(wildcard kernel/*.h drivers/*.h)

# Create object files by replacing .c with .o in C_SOURCES
OBJ = ${C_SOURCES:.c=.o}

# Specify paths for cross-compiler and GDB
CC = /usr/local/i386elfgcc/bin/i386-elf-gcc
GDB = /usr/local/i386elfgcc/bin/i386-elf-gdb

# GCC flags (including debugging symbols)
CFLAGS = -g -gdwarf-3

all: run

# Default target: create os-image.bin by concatenating boot and kernel binaries
os-image.bin: boot/boot.bin kernel.bin
	cat $^ > os-image.bin

# Create kernel.bin using the linker (binary format, no symbols)
kernel.bin: boot/kernel_entry.o ${OBJ}
	i386-elf-ld -o $@ -Ttext 0x1000 $^ --oformat binary

# Create kernel.elf for debugging (includes symbols)
kernel.elf: boot/kernel_entry.o ${OBJ}
	i386-elf-ld -g -o $@ -Ttext 0x1000 $^

# Run QEMU with the os-image.bin disk image
run: os-image.bin
	qemu-system-i386 -fda os-image.bin

# Debug target: start QEMU with GDB remote debugging
debug: os-image.bin kernel.elf
	qemu-system-i386 -s -fda os-image.bin &
	${GDB} -ex "target remote localhost:1234" -ex "symbol-file kernel.elf"

# Generic rule for compiling .c files to .o (with headers)
%.o: %.c ${HEADERS}
	${CC} ${CFLAGS} -ffreestanding -c $< -o $@

# Generic rule for assembling .asm files to .o
%.o: %.asm
	nasm $< -f elf -o $@

# Generic rule for assembling .asm files to .bin
%.bin: %.asm
	nasm $< -f bin -o $@

# Clean target: remove generated files
clean:
	rm -rf *.bin *.dis *.o os-image.bin *.elf
	rm -rf kernel/*.o boot/*.bin drivers/*.o boot/*.o
