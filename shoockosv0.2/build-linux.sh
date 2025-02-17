#!/bin/bash

# This script assembles the MikeOS bootloader, kernel and programs
# with NASM, and then creates floppy and CD images (on Linux)

# Only the root user can mount the floppy disk image as a virtual
# drive (loopback mounting), in order to copy across the files

# (If you need to blank the floppy image: 'mkdosfs disk_images/mikeos.flp')

mkdosfs -C bin/os.img 1440 || exit



nasm -w+orphan-labels -f bin -o source/bootload.bin source/bootload.asm || exit



cd source
nasm -w+orphan-labels -f bin -o kernel.bin kernel.asm || exit
cd ..



cd programs

for i in *.asm
do
	nasm -w+orphan-labels -f bin $i -o `basename $i .asm`.bin || exit
done

cd ..



dd status=noxfer conv=notrunc if=source/bootload.bin of=bin/os.img || exit



rm -rf tmp-loop

mkdir tmp-loop && mount -o loop -t vfat bin/os.img tmp-loop && cp source/kernel.bin tmp-loop/

cp programs/*.bin tmp-loop


umount tmp-loop || exit

rm -rf tmp-loop


rm -f bin/iso.img
mkisofs -quiet -V 'SHOCKOS' -input-charset iso8859-1 -o bin/iso.img -b os.img bin/ || exit

echo '>>> Done!'

