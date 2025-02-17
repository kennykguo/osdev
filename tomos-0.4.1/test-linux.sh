#!/bin/sh

# This script starts the QEMU PC emulator, booting from the
# MikeOS floppy disk image

qemu -soundhw pcspk -fda bin/img/mikeos.img -serial file:serialoutput.txt

