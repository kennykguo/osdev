/* test01.c - play sound */

#include <mikeos.h>

/*
	used MikeOS API :-

	mikeos_speaker_tone()
	mikeos_speaker_off()
	mikeos_pause()

	mikeos_print_digit()
	mikeos_print_space()
	mikeos_print_newline()
*/

int	MikeMain(void *arg)
{
	unsigned char	value = 3;

	/* Use "real" PC, emulators are not recommended */

	/* count down */
	mikeos_print_digit(value--);
	mikeos_print_space();

	/* play 440Hz sound */
	mikeos_speaker_tone(2712);	/* 1193182Hz / 440Hz = 2711.777273 */

	/* wait for 500 msec */
	mikeos_pause(5);

	/* speaker off and wait */
	mikeos_speaker_off();
	mikeos_pause(5);


	/* repeat same things... */
	mikeos_print_digit(value--);	/* 2 */
	mikeos_print_space();
	mikeos_speaker_tone(2712);
	mikeos_pause(5);
	mikeos_speaker_off();
	mikeos_pause(5);

	mikeos_print_digit(value--);	/* 1 */
	mikeos_print_space();
	mikeos_speaker_tone(2712);
	mikeos_pause(5);
	mikeos_speaker_off();
	mikeos_pause(5);

	mikeos_print_digit(value--);	/* 0 */
	mikeos_print_space();
	mikeos_speaker_tone(1356);	/* 1193182Hz / 880Hz = 1355.827273 */
	mikeos_pause(5);
	mikeos_speaker_off();
	mikeos_pause(5);

	mikeos_print_newline();

	return 0;
}
