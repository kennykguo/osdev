#include "pprintf.h"
#include "mmalloc.h"

int main() { 
    // Test 1: Simple allocation
    int* numbers = (int*)mmalloc(5 * sizeof(int));
    if (numbers) {
        for (int i = 0; i < 5; i++) {
            numbers[i] = i * 10;
            pprintf("numbers[%d] = %d\n", i, numbers[i]);
        }
        ffree(numbers);
    }
    
    // Test 2: Multiple allocations
    char* str1 = (char*)mmalloc(20);
    char* str2 = (char*)mmalloc(20);
    
    if (str1 && str2) {
        pprintf(str1, "Hello");
        pprintf(str2, "World");
        pprintf("%s %s\n", str1, str2);
        
        ffree(str1);
        ffree(str2);
    }
    
    return 0;
}