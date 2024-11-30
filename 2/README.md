sudo apt install qemu qemu-system-i386
qemu-system-i386 -fda build/main_floppy.img

dd if=/dev/zero of=test.img bs=512 count=2880
2880+0 records in
2880+0 records out
1474560 bytes (1.5 MB, 1.4 MiB) copied, 0.0030007 s, 491 MB/s

mkfs.fat -F 12 -n "NBOS" test.img
mkfs.fat 4.2 (2021-01-31)
mcopy -i test.img build/kernel.bin "::kernel.bin" 