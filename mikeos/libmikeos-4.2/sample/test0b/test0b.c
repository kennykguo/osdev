/* test0b.c - file operation (3) write file */

#include <mikeos.h>

/*
	used MikeOS API :-

	mikeos_create_file
	mikeos_file_exists
	mikeos_get_file_size
	mikeos_load_file
	mikeos_remove_file
	mikeos_write_file

	mikeos_print_4hex
	mikeos_print_newline
	mikeos_print_space
	mikeos_print_string
*/

/* filename and buffer must be on the static region */
static	char	FileName[] = "TEST0B.TMP";
static	char	Buf[8192];

static	char	StringOK[] = "OK ";
static	char	StringNG[] = "NG ";

/* fill with data */
static	void	fill_buffer(char *buf, int size, char data)
{
	int	i;

	for (i = 0; i < size; i++) buf[i] = data;
	return;
}

/* verify filled data */
static	int	verify_buffer(char *buf, int size, char data)
{
	int	i;

	for (i = 0; i < size; i++) {
		if (buf[i] != data) return -1;
	}

	return 0;
}

/* test os_write_file and os_get_file_size */
static	void	test_write_file(void)
{
	int	i, size;

	static	char	str0[] = "mikeos_write_file ";

	mikeos_print_string(str0);

	for (i = 0; i < 14; i++) {
		size = 1 << i;	/* 0-byte is not supported */
		mikeos_print_newline();
		mikeos_print_4hex(size);
		mikeos_print_space();

		/* overwrite is not supported, so test file must be removed */
		mikeos_remove_file(FileName);	/* no error check */

		/* write to file */
		fill_buffer(Buf, size, '@' + i);
		mikeos_print_string(mikeos_write_file(Buf, FileName, size)
				    == size ? StringOK : StringNG);

		/* check verify_buffer is sane */
		fill_buffer(Buf, size, 0);
		mikeos_print_string((verify_buffer(Buf, size, 0) >= 0 &&
				     verify_buffer(Buf, size, '@' + i) < 0) ?
				    StringOK : StringNG);

		/* load from file */
		mikeos_print_string(mikeos_load_file(Buf, FileName) == size ?
				    StringOK : StringNG);

		/* verify saved file */
		mikeos_print_string(verify_buffer(Buf, size, '@' + i) >= 0 ?
				    StringOK : StringNG);

		/* check file size */
		mikeos_print_string(mikeos_get_file_size(FileName) == size ?
				    StringOK : StringNG);

		/* remove test file */
		mikeos_print_string(mikeos_remove_file(FileName) >= 0 ?
				    StringOK : StringNG);

		/* no more test file... */
		mikeos_print_string(mikeos_file_exists(FileName) < 0 ?
				    StringOK : StringNG);

		mikeos_print_string(mikeos_get_file_size(FileName) < 0 ?
				    StringOK : StringNG);
	}

	mikeos_print_newline();
	return;
}

/* test os_create_file */
static	void	test_create_file(void)
{
	static	char	str0[] = "mikeos_create_file ";

	mikeos_print_string(str0);

	/* test file is already removed */
	mikeos_print_string(mikeos_remove_file(FileName) >= 0 ?
			    StringNG : StringOK);

	/* create file */
	mikeos_print_string(mikeos_create_file(FileName) >= 0 ?
			    StringOK : StringNG);

	/* check file size */
	mikeos_print_string(mikeos_get_file_size(FileName) == 0 ?
			    StringOK : StringNG);

	/* remove test file */
	mikeos_print_string(mikeos_remove_file(FileName) >= 0 ?
			    StringOK : StringNG);

	mikeos_print_newline();
	return;
}

int	MikeMain(void *arg)
{
	test_write_file();

	/* test file is removed by test_write_file() */
	test_create_file();

	return 0;
}
