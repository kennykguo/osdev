#include "../cpu/isr.h"
#include "../cpu/timer.h"
// #include "../drivers/keyboard.h"

void main() {
    isr_install();
    clear_screen();
    asm volatile("sti");
    // init_timer(1);
    /* Comment out the timer IRQ handler to read
     * the keyboard IRQs easier */
    init_keyboard();
}