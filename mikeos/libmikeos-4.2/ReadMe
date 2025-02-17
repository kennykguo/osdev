libmikeos, a small library to play MikeOS with Bruce's C Compiler (bcc).


Materials in this archive:

	ReadMe		This file
	build.os/	Working directory to build libmikeos.a, crt0.o, ...
	build.io/	Working directory to build libmikeio.a
	include/	Target directory to install headers
	lib/		Target directory to install libraries
	sample/		Sample codes for libmikeos
	src.os/		Source files of libmikeos.a, crt0.o, ...
	src.io/		Source files of libmikeio.a

Requirements:

	Host -
		bcc/Robert de Bath's Dev86-0.16.17 (http://www.debath.co.uk/)
		GNU Make (not BSD make)

	Target -
		386+ processor (586+ processor is recommended)

How to build:

	$ tar zxpf libmikeos-xxyyzz.tar.gz
	$ cd libmikeos/build.os
	$ make
	$ make install
	$ cd ../build.io
	$ make
	$ make install

	ar86 says "member name 'xxyyzz' truncated to 'aabbcc'", ignore.
	If you are *BSD user, use gmake instead of make.

How to write code with libmikeos:

	See sample C codes, please.

	sample/
		test00/	"Hello, World!"
		test01/	play sound
		test02/	date/time string
		test03/	string operations
		test04/	string input, show/hide cursor
		test05/	misc(1)
		test06/	move cursor
		test08/	serial port in/out
		test09/	file operation (1) load file
		test0a/	file operation (2) rename file
		test0b/	file operation (3) write file

		testfb/	I/O stuff: read MSR (for AMD processor)
		testfc/	I/O stuff: segment/MSR/TSC registers, memory, I/O
		testfd/	I/O stuff: CPUID instruction

		testfe/	bcc stuff: 16bit (int/short) operation
			(currently __idivu, __imod, __imodu)
		testff/	bcc stuff: 32bit (long) operation

License issues:

	build.os/	LGPL
	build.io/	LGPL
	include/	LGPL
	lib/		LGPL
	src.os/		LGPL
	src.io/		LGPL

		see src.os/COPYING and src.io/COPYING

	sample/		MikeOS
	ReadMe		MikeOS

		see sample/LICENSE.TXT

Hints for how to maintenance, supporting new API for future releases of MikeOS:

	1. Renew src.os/os_vector.inc

		$ cd libmikeos/src.os
		$ grep "jmp os_" /somewhere/mikeos-x.y.z/source/kernel.asm | \
		  awk '{printf("%%define %-31s %s\n",$2,$4)}' > os_vector.inc

	2. Add new "os_xxyyzz.asm"

		"os_xxyyzz.asm" must be start with

			; @@@ C interface definition

		for example,

			; @@@ unsigned int mikeos_wait_for_key(void);

	3. Rebuild and install new libmikeos.a

		See "How to build".

	4. Renew include/mikeos_api.h

		$ cd libmikeos/src.os
		$ cat os*.asm | grep @@@ | \
		  sed 's/; @@@ //' > ../include/mikeos_api.h

	5. Check your API-wrapper works correctly.

----
Thanks a lot to Mike Saunders, creating cool MikeOS and
supporting this project.

And I also say thank you to GOTO Daichi. I knew MikeOS 1.0 by his article
on MYCOM Journal (http://journal.mycom.co.jp/news/2007/09/20/005/).

SASANO Takayoshi <uaa@uaa.org.uk>
