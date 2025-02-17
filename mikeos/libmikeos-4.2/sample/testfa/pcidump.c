/* pcidump.c */

#include <mikeos.h>

typedef	unsigned long	uint32_t;

static	uint32_t	hex_decode(char *p)
{
	uint32_t	val = 0;

	while (1) {
		if (*p >= '0' && *p <= '9') {
			val = (val << 4) | (*p - '0');
		} else if (*p >= 'A' && *p <= 'F') {
			val = (val << 4) | (*p - 'A' + 10);
		} else if (*p >= 'a' && *p <= 'f') {
			val = (val << 4) | (*p - 'a' + 10);
		} else {
			break;
		}

		p++;
	}

	return val;
}

static	void	print_8hex(unsigned long value)
{
	mikeos_print_4hex(value >> 16);
	mikeos_print_4hex(value);

	return;
}

static	void	print_character(char c)
{
	static	char	buf[2];

	buf[0] = c;
	buf[1] = '\0';
	mikeos_print_string(buf);

	return;
}

static	uint32_t	read_pcireg(uint32_t reg)
{
	uint32_t	var;

	reg |= 0x80000000L;
#asm
	cli
#endasm

	io_output_port32(0xcf8, reg);
	var = io_input_port32(0xcfc);

#asm
	sti
#endasm

	return var;
}

int	MikeMain(char *argument)
{
	static char	helpmsg[] = "usage: pcidump [bus:dev:func]";
	uint32_t	i, var;

	if (argument == NULL) {
		mikeos_print_string(helpmsg);
		mikeos_print_newline();
		goto fin0;
	}

	var = hex_decode(argument) << 8;

	for (i = 0; i < 256; i += 4) {
		if (!(i % 0x20)) {
			mikeos_print_2hex(i);
			print_character(':');
			print_character(' ');
		}

		print_8hex(read_pcireg(var | i));
		print_character(' ');

		if (!((i + 4) % 0x20)) {
			mikeos_print_newline();
		}
	}

 fin0:
	return 0;
}
