/* test09.c - file operation */

#include <mikeos.h>

/*
	used MikeOS API :-

	mikeos_file_selector()
	mikeos_load_file()

	mikeos_clear_screen()
	mikeos_move_cursor()
	mikeos_print_newline()
	mikeos_print_space()
	mikeos_print_string()
	mikeos_string_compare()
	mikeos_wait_for_key()
*/

/* filename and buffer for mikeos_load_file() */
static	char	FileName0[] = "TEST09.BIN";	/* found */
static	char	FileName1[] = "           ";	/* not found */
static	char	Buf[8192];			/* enough size to load */

static	char	StringOK[] = "OK";
static	char	StringNG[] = "NG";

extern	char	_edata[];

/* fill with garbage to check zero-terminated string */
static	void	fill_buffer(char *buf, int size)
{
	int	i;

	for (i = 0; i < size; i++) buf[i] = 'X';
	return;
}

/* test my binary */
static	int	verify_myself(void)
{
#define	TopAddr	0x0100			/* top address of this program */

	int	i;

	for (i = 0; i < _edata - TopAddr; i++) {
		if (_edata[i] != Buf[i]) return 0;
	}

	return 1;
}

/* test os_load_file */
static	void	test_os_load_file(void)
{
	static	char	str[] = "mikeos_load_file";

	mikeos_print_string(str);
	mikeos_print_space();

	/* file must be found */
	fill_buffer(Buf, sizeof(Buf));
	mikeos_print_string((mikeos_load_file(Buf, FileName0) < 0) ?
			    StringNG : StringOK);
	mikeos_print_space();

	/* verify loaded file - this test code */
	mikeos_print_string(verify_myself() ? StringOK : StringNG);
	mikeos_print_space();

	/* file must not be found */
	mikeos_print_string((mikeos_load_file(Buf, FileName1) < 0) ?
			    StringOK : StringNG);
	mikeos_print_newline();

	return;
}

/* test os_file_selector */
static	void	test_os_file_selector()
{
	/* mikeos_file_selector() requires the pointer to static region */

	static	char	str0[] = "Press [Esc] to continue...";
	static	char	str1[] = "Select KERNEL.BIN to continue...";
	static	char	str2[] = "KERNEL.BIN";
	int	result;

	do {
		mikeos_clear_screen();
		mikeos_print_string(str0);
		result = mikeos_file_selector(Buf);
	} while (result >= 0);

	do {
		mikeos_clear_screen();
		mikeos_print_string(str1);
		fill_buffer(Buf, sizeof(Buf));
		result = mikeos_file_selector(Buf);
	} while (result < 0 || !mikeos_string_compare(Buf, str2));

	mikeos_move_cursor(0, 23);

	return;
}


int	MikeMain(void *arg)
{
	static	char	str[] = "Press any key to continue...";

	test_os_load_file();

	mikeos_print_string(str);
	mikeos_print_newline();
	mikeos_wait_for_key();

	test_os_file_selector();

	return 0;
}
