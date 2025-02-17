/* testfd.c - check I/O function */

#include <mikeos.h>

static	unsigned char	StringNG[] = "NG ";
static	unsigned char	StringOK[] = "OK ";

#define	COM_PORT	0x3f8	/* COM1 */
#define	PCI_INDEX	0xcf8
#define	PCI_DATA	0xcfc

/* print 32bit hex value */
static	void	print_8hex(unsigned long value)
{
	mikeos_print_4hex(value >> 16);
	mikeos_print_4hex(value);
	return;
}

/* test get_ds_register, paranoia */
static	unsigned short	test_io_get_ds_register(void)
{
#asm
	cli
	push	ds

	mov	ax, #0xc000
	mov	ds, ax
	xor	ax, ax

	call	_io_get_ds_register	

	pop	ds	
	sti
#endasm
	/* return value: AX register */
}

/* test get_(segment)_register */
static	void	test_segment(void)
{
	static	unsigned char	cs[] = "CS:";
	static	unsigned char	ds[] = "DS:";
	static	unsigned char	ss[] = "SS:";

	mikeos_print_string(cs);
	mikeos_print_string(io_get_cs_register() == 0x2000 ?
			    StringOK : StringNG);
	mikeos_print_string(ds);
	mikeos_print_string(test_io_get_ds_register() == 0xc000 ?
			    StringOK : StringNG);
	mikeos_print_string(ss);
	mikeos_print_string(io_get_ss_register() == 0x0000 ?
			    StringOK : StringNG);
	mikeos_print_newline();

	return;
}	

/* test write_memory/read_memory */
static	void	test_rwmem(void)
{
	static	unsigned char	str0[] = "io_write_memory/io_read_memory: ";
	unsigned short	seg = io_get_cs_register() + 0x1000;

	mikeos_print_string(str0);

	/* little endian */
	io_write_memory32((unsigned long *)0x0000, seg, 0xffffffffUL);
	io_write_memory32((unsigned long *)0x0004, seg, 0xffffffffUL);
	io_write_memory32((unsigned long *)0x0008, seg, 0xffffffffUL);
	io_write_memory32((unsigned long *)0x000c, seg, 0xffffffffUL);

	io_write_memory8((unsigned char *)0x0000, seg, 0x00);
	io_write_memory8((unsigned char *)0x0001, seg, 0x01);
	io_write_memory8((unsigned char *)0x0002, seg, 0x02);
	io_write_memory8((unsigned char *)0x0003, seg, 0x03);
	io_write_memory16((unsigned short *)0x0004, seg, 0x0504);
	io_write_memory16((unsigned short *)0x0006, seg, 0x0706);
	io_write_memory32((unsigned long *)0x0008, seg, 0x0b0a0908UL);

	if (io_read_memory8((unsigned char *)0x0000, seg) == 0x00 &&
	    io_read_memory8((unsigned char *)0x0001, seg) == 0x01 &&
	    io_read_memory8((unsigned char *)0x0002, seg) == 0x02 &&
	    io_read_memory8((unsigned char *)0x0003, seg) == 0x03 &&
	    io_read_memory8((unsigned char *)0x0004, seg) == 0x04 &&
	    io_read_memory8((unsigned char *)0x0005, seg) == 0x05 &&
	    io_read_memory8((unsigned char *)0x0006, seg) == 0x06 &&
	    io_read_memory8((unsigned char *)0x0007, seg) == 0x07 &&
	    io_read_memory8((unsigned char *)0x0008, seg) == 0x08 &&
	    io_read_memory8((unsigned char *)0x0009, seg) == 0x09 &&
	    io_read_memory8((unsigned char *)0x000a, seg) == 0x0a &&
	    io_read_memory8((unsigned char *)0x000b, seg) == 0x0b &&
	    io_read_memory8((unsigned char *)0x000c, seg) == 0xff &&
	    io_read_memory8((unsigned char *)0x000d, seg) == 0xff &&
	    io_read_memory8((unsigned char *)0x000e, seg) == 0xff &&
	    io_read_memory8((unsigned char *)0x000f, seg) == 0xff) {
		mikeos_print_string(StringOK);
	} else {
		mikeos_print_string(StringNG);
	}

	if (io_read_memory16((unsigned short *)0x0000, seg) == 0x0100 &&
	    io_read_memory16((unsigned short *)0x0002, seg) == 0x0302 &&
	    io_read_memory16((unsigned short *)0x0004, seg) == 0x0504 &&
	    io_read_memory16((unsigned short *)0x0006, seg) == 0x0706 &&
	    io_read_memory16((unsigned short *)0x0008, seg) == 0x0908 &&
	    io_read_memory16((unsigned short *)0x000a, seg) == 0x0b0a &&
	    io_read_memory16((unsigned short *)0x000c, seg) == 0xffff &&
	    io_read_memory16((unsigned short *)0x000e, seg) == 0xffff) {
		mikeos_print_string(StringOK);
	} else {
		mikeos_print_string(StringNG);
	}

	if (io_read_memory32((unsigned long *)0x0000, seg) == 0x03020100UL &&
	    io_read_memory32((unsigned long *)0x0004, seg) == 0x07060504UL &&
	    io_read_memory32((unsigned long *)0x0008, seg) == 0x0b0a0908UL &&
	    io_read_memory32((unsigned long *)0x000c, seg) == 0xffffffffUL) {
		mikeos_print_string(StringOK);
	} else {
		mikeos_print_string(StringNG);
	}

	mikeos_print_newline();

	return;
}

/* dump 32-bit register pair */
static	void	dump32pair(unsigned long low32, unsigned long high32)
{
	static	unsigned char	colon[] = ":";

	print_8hex(high32);
	mikeos_print_string(colon);
	print_8hex(low32);
	mikeos_print_space();

	return;
}

/* test TSC/MSR */
static	void	test_tsc_msr(void)
{
	static	unsigned char	str0[] = "tsc/msr:";
	unsigned long		edx_stack, eax_stack;
	static	unsigned long	edx_static, eax_static;	

	mikeos_print_string(str0);
	mikeos_print_newline();

	/* MSR index 0x10 is TSC (AMD/Intel common) */

	/*
	 * Note:
	 * Some Intel processors treat higher-32bits as zero
	 * when setting TSC value with wrmsr instruction.
	 * (For details, see "Intel(R) 64 and IA-32 Architectures
	 * Software Developer's Manual".)
	 * So, we recommend to use AMD or other processors when
	 * you perform this test.
	 */

	eax_stack = 0x89abcdef;
	edx_stack = 0x01234567;
	dump32pair(eax_stack, edx_stack);
	io_set_msr_register(0x10UL, eax_stack, edx_stack);

	io_get_tsc_counter(&eax_stack, &edx_stack);
	dump32pair(eax_stack, edx_stack);
	io_get_msr_register(0x10UL, &eax_stack, &edx_stack);
	dump32pair(eax_stack, edx_stack);
	io_get_msr_register_amd(0x10UL, &eax_stack, &edx_stack);
	dump32pair(eax_stack, edx_stack);
	mikeos_print_newline();

	eax_static = 0x01234567;
	edx_static = 0x89abcdef;
	dump32pair(eax_static, edx_static);
	io_set_msr_register_amd(0x10UL, eax_static, edx_static);

	io_get_tsc_counter(&eax_static, &edx_static);
	dump32pair(eax_static, edx_static);
	io_get_msr_register(0x10UL, &eax_static, &edx_static);
	dump32pair(eax_static, edx_static);
	io_get_msr_register_amd(0x10UL, &eax_static, &edx_static);
	dump32pair(eax_static, edx_static);
	mikeos_print_newline();

	return;
}

/* test 8bit I/O */
static	void	test_io8bit(void)
{
	int	i;

	static	unsigned char	str0[] = "io_output_port8/io_input_port8: ";

	mikeos_print_string(str0);

	/* use scratchpad register of NS16550 UART */
	for (i = 1; i < 0x100; i <<= 1) {
		io_output_port8(COM_PORT + 7, i);
		if (io_input_port8(COM_PORT + 7) != i) break;
	}

	mikeos_print_string(i == 0x100 ? StringOK : StringNG);
	mikeos_print_newline();
}

/* test 8bit I/O - do the same thing with os_port_byte_in/out */
static	void	test_os_port_byte(void)
{
	int	i;

	static	unsigned char	str0[] = "os_port_byte_in/out: ";

	mikeos_print_string(str0);

	/* use scratchpad register of NS16550 UART */
	for (i = 1; i < 0x100; i <<= 1) {
		mikeos_port_byte_out(COM_PORT + 7, i);
		if (mikeos_port_byte_in(COM_PORT + 7) != i) break;
	}

	mikeos_print_string(i == 0x100 ? StringOK : StringNG);
	mikeos_print_newline();
}

/* test 16bit I/O */
static	void	test_io16bit(void)
{
	unsigned int	i, dll;
	unsigned char	lcr;

	static	unsigned char	str0[] = "io_output_port16/io_input_port16: ";

	mikeos_print_string(str0);

	lcr = io_input_port8(COM_PORT + 3);
	io_output_port8(COM_PORT + 3, lcr | 0x80);
	dll = io_input_port16(COM_PORT + 0);

	/* use baud-rate divisor of NS16550 UART */
	for (i = 0x8000; i > 0; i >>= 1) {
		io_output_port16(COM_PORT + 0, i);
		if (io_input_port16(COM_PORT + 0) != i) break;
	}

	io_input_port16(COM_PORT + 0, dll);
	io_output_port8(COM_PORT + 3, lcr);

	mikeos_print_string(i == 0 ? StringOK : StringNG);
	mikeos_print_newline();
}

/* test 32bit I/O */
static	void	test_io32bit(void)
{
	unsigned long	i, d;

	static	unsigned char	str0[] = "io_output_port32/io_input_port32: ";
	static	unsigned char	colon[] = ":";
	static	unsigned char	space[] = "     ";

	mikeos_print_string(str0);
	mikeos_print_newline();

	/* show vendor:device ID on PCI bus #0-#1, func#0 */
	for (i = 0; i < 64; i++) {
		io_output_port32(PCI_INDEX, 0x80000000 | (i << 11));
		d = io_input_port32(PCI_DATA);

		if (d == 0xffffffff) continue;

		mikeos_print_2hex(i);
		mikeos_print_string(colon);
		print_8hex(d);
		mikeos_print_string(space);
	}

	mikeos_print_newline();
}

int	MikeMain(void *argument)
{
	test_segment();
	test_rwmem();
	test_tsc_msr();
	test_io8bit();
	test_io16bit();
	test_io32bit();
	test_os_port_byte();

	return 0;
}
