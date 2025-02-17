/* rdmsramd.c - read MSR register (for AMD processor) */

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

int	MikeMain(void *argument)
{
	static	char	buf[256];
	static	char	msg[] = "MSR index (hex value 0-ffffffff) ? ";
	uint32_t	ecx, edx, eax;

	mikeos_print_string(msg);
	mikeos_input_string(buf);
	mikeos_print_newline();

	if (!mikeos_string_length(buf)) goto fin0;

	ecx = hex_decode(buf);
	print_character('[');
	print_8hex(ecx);
	print_character(']');
	print_character(' ');

	io_get_msr_register_amd(ecx, &eax, &edx);
	print_8hex(edx);
	print_character(':');
	print_8hex(eax);

	mikeos_print_newline();

 fin0:
	return 0;
}
