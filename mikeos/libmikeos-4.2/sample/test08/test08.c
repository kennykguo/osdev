/* test08.c - serial port in/out */

#include <mikeos.h>

/*
	used MikeOS API :-

	mikeos_serial_port_enable()
	mikeos_get_via_serial()
	mikeos_send_via_serial()

	mikeos_check_for_key()
	mikeos_clear_screen()
	mikeos_move_cursor()
	mikeos_print_1hex()
	mikeos_print_2hex()
	mikeos_print_4hex()
	mikeos_print_newline()
	mikeos_print_space()
	mikeos_print_string()
*/


/* index to switch static/stack region */
static	char	Index = 0;

/* test os_get_via_serial()/os_send_via_serial() */
static	int	test_os_get_via_serial(void)
{
	static	char	c_static;
	char		c_stack;
	char		c;
	int	result;

	/*
	  mikeos_serial_port_enable configures COM1 as mode 0:-
		speed: 9600bps
		character length: 8bit
		stopbit length: 1bit
		parity: none

	  mode 1 (1200bps) is homework...
	*/
	mikeos_serial_port_enable(0);

	/* mikeos_get_via_serial() accepts both pointer to
	   static and stack region */
	if (Index & 1) {
		result = mikeos_get_via_serial(&c_static);
		c = c_static;
	} else {
		result = mikeos_get_via_serial(&c_stack);
		c = c_stack;
	}

	/* show receive status */
	mikeos_move_cursor(0, 1);
	mikeos_print_1hex(Index++ & 1);
	mikeos_print_space();
	mikeos_print_4hex(result);
	mikeos_print_space();
	mikeos_print_2hex(c);
	mikeos_print_space();

	/* if there is something error, quit immediately */
	if (result < 0) goto fin0;

	/* send echo back */
	result = mikeos_send_via_serial(c);

	/* show transmit status */
	mikeos_print_4hex(result);

 fin0:
	/* check [Esc] is not pressed */
	return mikeos_check_for_key() != 0x1b;
}

int	MikeMain(void *argument)
{
	static	char	message[] = "Press [Esc] to quit...";

	mikeos_clear_screen();
	mikeos_print_string(message);
	while (test_os_get_via_serial());
	mikeos_print_newline();

	return 0;
}
