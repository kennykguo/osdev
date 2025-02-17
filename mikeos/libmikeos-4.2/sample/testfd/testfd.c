/* testfd.c - simply test "cpuid" */

#include <mikeos.h>

int	MikeMain(void *argument)
{
	int	i;
	static	char	str0[13];
	static	char	str1[49];
	static	char	str2[] = "(This feature is not supported)";
	unsigned long	eax;

	for (i = 0; i < sizeof(str0); i++) str0[i] = '\0';
	for (i = 0; i < sizeof(str1); i++) str1[i] = '\0';

	io_get_cpuid(0x00000000UL, &eax, &str0[0], &str0[8], &str0[4]);

	mikeos_print_string(str0);
	mikeos_print_newline();

	io_get_cpuid(0x80000000UL, &eax, NULL, NULL, NULL);

	if (eax >= 0x80000004UL) {
		io_get_cpuid(0x80000002UL,
			 &str1[ 0], &str1[ 4], &str1[ 8], &str1[12]);
		io_get_cpuid(0x80000003UL,
			 &str1[16], &str1[20], &str1[24], &str1[28]);
		io_get_cpuid(0x80000004UL,
			 &str1[32], &str1[36], &str1[40], &str1[44]);
		mikeos_print_string(str1);
	} else {
		mikeos_print_string(str2);
	}

	mikeos_print_newline();

	return 0;
}
