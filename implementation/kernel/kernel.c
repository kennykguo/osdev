/* This will force us to create a kernel entry function instead of jumping to kernel.c:0x00 */
void dummy_test_entrypoint() {
}

void main() {
    char* video_memory = (char*) 0xb8000;
    *video_memory = 'X';
}

// /* Enhanced Kernel Implementation for bare metal */

// /* Type definitions for bare metal environment */
// typedef unsigned char uint8_t;
// typedef unsigned short uint16_t;
// typedef unsigned int uint32_t;
// #define NULL ((void*)0)
// #define true 1
// #define false 0
// typedef uint8_t bool;

// /* Constants and Definitions */
// #define VIDEO_MEMORY ((char*)0xb8000)
// #define MAX_ROWS 25
// #define MAX_COLS 80

// /* Color codes for text display */
// #define BLACK 0x0
// #define BLUE 0x1
// #define GREEN 0x2
// #define CYAN 0x3
// #define RED 0x4
// #define MAGENTA 0x5
// #define BROWN 0x6
// #define LIGHT_GRAY 0x7
// #define DARK_GRAY 0x8
// #define LIGHT_BLUE 0x9
// #define LIGHT_GREEN 0xA
// #define LIGHT_CYAN 0xB
// #define LIGHT_RED 0xC
// #define LIGHT_MAGENTA 0xD
// #define YELLOW 0xE
// #define WHITE 0xF

// /* Text attribute byte for our default color scheme */
// #define DEFAULT_COLOR ((WHITE << 4) | BLUE)  // White background, blue text

// /* Keyboard scan codes */
// #define SC_ENTER 0x1C
// #define SC_BACKSPACE 0x0E
// #define SC_LEFT_SHIFT 0x2A
// #define SC_RIGHT_SHIFT 0x36
// #define SC_CAPS_LOCK 0x3A
// #define SC_ESCAPE 0x01

// /* System state structure */
// typedef struct {
//     int cursor_row;
//     int cursor_col;
//     char text_color;
//     char background_color;
//     bool caps_lock;
//     bool shift_pressed;
// } SystemState;

// /* Global system state */
// static SystemState sys_state = {
//     0,      // cursor_row
//     0,      // cursor_col
//     BLUE,   // text_color
//     WHITE,  // background_color
//     false,  // caps_lock
//     false   // shift_pressed
// };

// /* Function to calculate memory offset for cursor position */
// int get_offset(int row, int col) {
//     return (row * MAX_COLS + col) * 2;
// }

// /* Function to clear the screen */
// void clear_screen() {
//     char* video_memory = VIDEO_MEMORY;
//     for (int i = 0; i < MAX_ROWS * MAX_COLS * 2; i += 2) {
//         video_memory[i] = ' ';
//         video_memory[i + 1] = (sys_state.background_color << 4) | sys_state.text_color;
//     }
//     sys_state.cursor_row = 0;
//     sys_state.cursor_col = 0;
//     update_cursor(0, 0);
// }

// /* Function to scroll the screen */
// void scroll_screen() {
//     char* video_memory = VIDEO_MEMORY;
    
//     // Move each line up one position
//     for (int row = 1; row < MAX_ROWS - 1; row++) {
//         for (int col = 0; col < MAX_COLS; col++) {
//             int dest_offset = get_offset(row - 1, col);
//             int src_offset = get_offset(row, col);
//             video_memory[dest_offset] = video_memory[src_offset];
//             video_memory[dest_offset + 1] = video_memory[src_offset + 1];
//         }
//     }
    
//     // Clear the last line
//     int last_row = MAX_ROWS - 2;  // Leave space for status bar
//     for (int col = 0; col < MAX_COLS; col++) {
//         int offset = get_offset(last_row, col);
//         video_memory[offset] = ' ';
//         video_memory[offset + 1] = (sys_state.background_color << 4) | sys_state.text_color;
//     }
// }

// /* Function to update hardware cursor */
// void update_cursor(int row, int col) {
//     uint16_t pos = row * MAX_COLS + col;
    
//     outb(0x3D4, 14);
//     outb(0x3D5, (pos >> 8) & 0xFF);
//     outb(0x3D4, 15);
//     outb(0x3D5, pos & 0xFF);
// }

// /* Scan code to ASCII conversion */
// char scan_code_to_ascii(unsigned char scan_code) {
//     // Basic scan code to ASCII mapping
//     static const char ascii_low[] = {
//         0, 0, '1', '2', '3', '4', '5', '6', '7', '8', '9', '0', '-', '=', '\b',
//         '\t', 'q', 'w', 'e', 'r', 't', 'y', 'u', 'i', 'o', 'p', '[', ']', '\n',
//         0, 'a', 's', 'd', 'f', 'g', 'h', 'j', 'k', 'l', ';', '\'', '`',
//         0, '\\', 'z', 'x', 'c', 'v', 'b', 'n', 'm', ',', '.', '/', 0,
//         '*', 0, ' '
//     };
    
//     static const char ascii_high[] = {
//         0, 0, '!', '@', '#', '$', '%', '^', '&', '*', '(', ')', '_', '+', '\b',
//         '\t', 'Q', 'W', 'E', 'R', 'T', 'Y', 'U', 'I', 'O', 'P', '{', '}', '\n',
//         0, 'A', 'S', 'D', 'F', 'G', 'H', 'J', 'K', 'L', ':', '"', '~',
//         0, '|', 'Z', 'X', 'C', 'V', 'B', 'N', 'M', '<', '>', '?', 0,
//         '*', 0, ' '
//     };
    
//     if (scan_code >= sizeof(ascii_low)) {
//         return 0;
//     }
    
//     char ascii = 0;
//     if (sys_state.shift_pressed || (sys_state.caps_lock && scan_code >= 0x10 && scan_code <= 0x19)) {
//         ascii = ascii_high[scan_code];
//     } else {
//         ascii = ascii_low[scan_code];
//     }
    
//     return ascii;
// }

// /* Port I/O functions */
// void outb(uint16_t port, uint8_t value) {
//     __asm__ volatile ("outb %0, %1" : : "a"(value), "Nd"(port));
// }

// uint8_t inb(uint16_t port) {
//     uint8_t value;
//     __asm__ volatile ("inb %1, %0" : "=a"(value) : "Nd"(port));
//     return value;
// }

// /* Custom string functions */
// int strlen(const char* str) {
//     int len = 0;
//     while (str[len]) len++;
//     return len;
// }

// void int_to_string(int num, char* str) {
//     int i = 0;
//     bool is_negative = false;
    
//     if (num == 0) {
//         str[0] = '0';
//         str[1] = '\0';
//         return;
//     }
    
//     if (num < 0) {
//         is_negative = true;
//         num = -num;
//     }
    
//     while (num != 0) {
//         int rem = num % 10;
//         str[i++] = rem + '0';
//         num = num / 10;
//     }
    
//     if (is_negative) {
//         str[i++] = '-';
//     }
    
//     str[i] = '\0';
    
//     // Reverse the string
//     int start = 0;
//     int end = i - 1;
//     while (start < end) {
//         char temp = str[start];
//         str[start] = str[end];
//         str[end] = temp;
//         start++;
//         end--;
//     }
// }

// /* Check if a key is special */
// bool is_special_key(unsigned char scan_code) {
//     return (scan_code == SC_CAPS_LOCK ||
//             scan_code == SC_LEFT_SHIFT ||
//             scan_code == SC_RIGHT_SHIFT ||
//             scan_code == SC_ESCAPE ||
//             scan_code == SC_BACKSPACE ||
//             scan_code == SC_ENTER);
// }

// /* Initialize the system */
// void init_system() {
//     clear_screen();
//     set_color_scheme(sys_state.text_color, sys_state.background_color);
//     print_status_bar();
//     print_string("Enhanced Kernel v1.0\n");
//     print_string("Features:\n");
//     print_string("- Color support\n");
//     print_string("- Special key handling\n");
//     print_string("- Status bar\n");
//     print_string("- Backspace support\n");
//     print_string("\nType away...\n\n");
// }

// /* Main kernel entry point */
// void main() {
//     init_system();
    
//     while (1) {
//         unsigned char scan_code = inb(0x60);
        
//         if (scan_code != 0) {
//             if (is_special_key(scan_code)) {
//                 handle_special_key(scan_code);
//             } else {
//                 char ascii = scan_code_to_ascii(scan_code);
//                 if (ascii != 0) {
//                     print_char(ascii);
//                     print_status_bar();
//                 }
//             }
//         }
//     }
// }

// /* Print a character to the screen */
// void print_char(char c) {
//     if (c == '\b') {
//         backspace();
//         return;
//     }

//     if (sys_state.cursor_col >= MAX_COLS || c == '\n') {
//         sys_state.cursor_row++;
//         sys_state.cursor_col = 0;
        
//         if (sys_state.cursor_row >= MAX_ROWS - 1) {
//             scroll_screen();
//             sys_state.cursor_row = MAX_ROWS - 2;
//         }
        
//         if (c == '\n') return;
//     }

//     char* video_memory = VIDEO_MEMORY;
//     int offset = get_offset(sys_state.cursor_row, sys_state.cursor_col);
//     char attribute_byte = (sys_state.background_color << 4) | sys_state.text_color;
    
//     video_memory[offset] = c;
//     video_memory[offset + 1] = attribute_byte;
    
//     sys_state.cursor_col++;
//     update_cursor(sys_state.cursor_row, sys_state.cursor_col);
// }

// /* Print a string to the screen */
// void print_string(const char* str) {
//     while (*str) {
//         print_char(*str);
//         str++;
//     }
// }

// /* Handle backspace key */
// void backspace() {
//     if (sys_state.cursor_col > 0) {
//         sys_state.cursor_col--;
//     } else if (sys_state.cursor_row > 0) {
//         sys_state.cursor_row--;
//         sys_state.cursor_col = MAX_COLS - 1;
//     } else {
//         return;  // At top-left corner, nothing to backspace
//     }
    
//     int offset = get_offset(sys_state.cursor_row, sys_state.cursor_col);
//     char* video_memory = VIDEO_MEMORY;
//     video_memory[offset] = ' ';
//     video_memory[offset + 1] = (sys_state.background_color << 4) | sys_state.text_color;
    
//     update_cursor(sys_state.cursor_row, sys_state.cursor_col);
// }

// /* Print the status bar */
// void print_status_bar() {
//     int status_row = MAX_ROWS - 1;
//     char* video_memory = VIDEO_MEMORY;
//     char status_color = (BLACK << 4) | WHITE;
    
//     // Fill status bar background
//     for (int col = 0; col < MAX_COLS; col++) {
//         int offset = get_offset(status_row, col);
//         video_memory[offset] = ' ';
//         video_memory[offset + 1] = status_color;
//     }
    
//     // Create status text
//     char status[MAX_COLS];
//     char pos_str[10];
//     int_to_string(sys_state.cursor_row, pos_str);
    
//     // Print each part of the status bar
//     char* caps_status = sys_state.caps_lock ? "ON " : "OFF";
//     char* shift_status = sys_state.shift_pressed ? "ON " : "OFF";
    
//     int offset = get_offset(status_row, 0);
//     const char* caps_text = " CAPS:";
//     while (*caps_text) {
//         video_memory[offset] = *caps_text;
//         video_memory[offset + 1] = status_color;
//         offset += 2;
//         caps_text++;
//     }
    
//     while (*caps_status) {
//         video_memory[offset] = *caps_status;
//         video_memory[offset + 1] = status_color;
//         offset += 2;
//         caps_status++;
//     }
// }

// /* Set text and background colors */
// void set_color_scheme(char text_color, char background_color) {
//     sys_state.text_color = text_color;
//     sys_state.background_color = background_color;
// }

// /* Handle special keys */
// void handle_special_key(unsigned char scan_code) {
//     switch (scan_code) {
//         case SC_CAPS_LOCK:
//             sys_state.caps_lock = !sys_state.caps_lock;
//             break;
//         case SC_LEFT_SHIFT:
//         case SC_RIGHT_SHIFT:
//             sys_state.shift_pressed = true;
//             break;
//         case SC_LEFT_SHIFT + 0x80:  // Key release
//         case SC_RIGHT_SHIFT + 0x80:
//             sys_state.shift_pressed = false;
//             break;
//         case SC_ESCAPE:
//             set_color_scheme(BLUE, WHITE);
//             clear_screen();
//             init_system();
//             break;
//     }
//     print_status_bar();
// }