# Directories for .asm and .c files
ASM_DIR = boot
C_DIR = kernel
OUT_DIR = .

# Tools
NASM = nasm
GCC = i386-elf-gcc
LD = i386-elf-ld
QEMU = qemu-system-i386
NDISASM = ndisasm
GDB = i386-elf-gdb

# First rule is the one executed when no parameters are fed to the Makefile
all: run

# Kernel binary depends on the kernel object files
$(OUT_DIR)/kernel.bin: $(OUT_DIR)/kernel_entry.o $(OUT_DIR)/kernel.o
	$(LD) -o $@ -Ttext 0x1000 $^ --oformat binary

# Compile the kernel entry assembly file
$(OUT_DIR)/kernel_entry.o: $(ASM_DIR)/kernel_entry.asm
	$(NASM) $< -f elf -o $@

# Compile the kernel C file (with debugging symbols)
$(OUT_DIR)/kernel.o: $(C_DIR)/kernel.c
	$(GCC) -ffreestanding -g -c $< -o $@

# Build the kernel ELF file (with debugging symbols, no standard library or runtime)
$(OUT_DIR)/kernel.elf: $(OUT_DIR)/kernel.o
	$(GCC) -ffreestanding -g -nostdlib -o $@ $^

# Rule to disassemble the kernel (useful for debugging)
$(OUT_DIR)/kernel.dis: $(OUT_DIR)/kernel.bin
	$(NDISASM) -b 32 $< > $@

# Compile the boot sector
$(OUT_DIR)/boot.bin: $(ASM_DIR)/boot.asm
	$(NASM) $< -f bin -o $@

# Create the final OS image by concatenating boot and kernel
$(OUT_DIR)/os-image.bin: $(OUT_DIR)/boot.bin $(OUT_DIR)/kernel.bin
	cat $^ > $@

# Run the OS image using QEMU
run: $(OUT_DIR)/os-image.bin
	$(QEMU) -fda $<

# Debugging with GDB and QEMU
debug: $(OUT_DIR)/os-image.bin $(OUT_DIR)/kernel.elf
	$(QEMU) -s -fda $< & \
	$(GDB) -ex "target remote localhost:1234" -ex "symbol-file $(OUT_DIR)/kernel.elf"

# Clean up build files
clean:
	rm -f $(OUT_DIR)/*.bin $(OUT_DIR)/*.o $(OUT_DIR)/*.dis $(OUT_DIR)/kernel.elf
