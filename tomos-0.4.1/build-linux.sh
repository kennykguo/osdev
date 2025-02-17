#!/bin/sh

# This script assembles the TomOS bootloader, kernel and programs
# with NASM, and then creates floppy and CD images (on Linux)

# Only the root user can mount the floppy disk image as a virtual
# drive (loopback mounting), in order to copy across the files


TOMOS_BIN='bin'
TOMOS_SRC='source'
TOMOS_PROGS='programs'
IMG_NAME='tomos.img'
ISO_NAME='tomos.iso'
KERNELNAME='tomoskrn.bin'
KERNELNAME_FAT12='TOMOSKRNBIN'
EMAIL='gavian@tlen.pl'
TOMOS_VER='0.4'

if test "`whoami`" != "root" ; then
	echo "You must be logged in as root to build (for loopback mounting)"
	echo "Enter 'su' or 'sudo bash' to switch to root"
	exit
fi

KERNELNAME_FAT12=`echo $KERNELNAME_FAT12 | tr '[a-z]' '[A-Z]'`
KERNELNAME=`echo $KERNELNAME | tr '[a-z]' '[A-Z]'`

echo ">>> Cleaning..."
    rm -rf $TOMOS_BIN/{*.[Ii][Mm][Gg],*.[bB][iI][Nn]}
    rm -rf $TOMOS_PROGS/bin/*.[Bb][Ii][Nn]
    rm -rf $TOMOS_BIN/img/*.[Ii][Mm][Gg]
    rm -rf $TOMOS_BIN/iso/*.[Ii][Ss][Oo]

echo ">>> Assembling bootloader..."

nasm -dTOMOS_VER="'$TOMOS_VER'" -dKERNELNAME="'$KERNELNAME'" -dKERNELNAME_FAT12="'$KERNELNAME_FAT12'" -w+orphan-labels -f bin -o $TOMOS_BIN/bootload.bin $TOMOS_SRC/bootload.asm || exit


echo ">>> Assembling TomOS kernel..."

cd $TOMOS_SRC
nasm -dTOMOS_VER="'$TOMOS_VER'" -dEMAIL="'$EMAIL'" -dKERNELNAME="'$KERNELNAME'" -dKERNELNAME_FAT12="'$KERNELNAME_FAT12'" -w+orphan-labels -f bin -o ../bin/$KERNELNAME os_main.asm || exit
cd - > /dev/null


echo ">>> Assembling programs..."

cd $TOMOS_PROGS

for i in *.asm
do
	echo ">>> ASM $i"
	nasm -w+orphan-labels -p ../include/syscalls.inc -f bin $i -o bin/`basename $i .asm`.bin || exit
done

cd - > /dev/null

echo  ">>> Creating floppy image..."

rm -rf $TOMOS_BIN/mikeos.img

mkdosfs -C  $TOMOS_BIN/img/$IMG_NAME 1440 1> /dev/null || exit

echo ">>> Adding bootloader to floppy image..."

dd status=noxfer conv=notrunc if=$TOMOS_BIN/bootload.bin of=$TOMOS_BIN/img/$IMG_NAME count=1 bs=512 2> /dev/null || exit

echo ">>> Copying MikeOS kernel and programs..."

rm -rf tmp-loop

mkdir tmp-loop && mount -o loop -t vfat $TOMOS_BIN/img/$IMG_NAME tmp-loop && cp $TOMOS_BIN/$KERNELNAME tmp-loop/

cp $TOMOS_PROGS/bin/*.bin tmp-loop

mkdir tmp-loop/D1
mkdir tmp-loop/D1/D12
mkdir tmp-loop/D2

echo 'TomOS' > tmp-loop/D1/TomOS.txt
echo 'MikeOS' > tmp-loop/D1/MikeOS.txt
echo 'Arch Linux' > tmp-loop/D1/D12/Arch.txt


echo ">>> Unmounting loopback floppy..."

umount tmp-loop || exit

rm -rf tmp-loop

echo ">>> Creating CD-ROM ISO image..."

mkisofs -quiet -V 'MIKEOS' -input-charset iso8859-1 -o $TOMOS_BIN/iso/$ISO_NAME -b $IMG_NAME bin/img | exit

echo ">>> Setting permisions for created files..."

chmod 664 $TOMOS_BIN/img/$IMG_NAME


echo '>>> Done!'

