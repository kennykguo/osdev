#include "mmalloc.h"

#define NULL ((void*)0)

static int heap_initialized = 0;  // Static variable to track initialization

// Allocate memory of given size (automatically initializes if needed)
void* mmalloc(unsigned int size) {
    static int heap_initialized = 0;
    if (size == 0) return NULL;

    if (!heap_initialized) {
        allocate_init();
        heap_initialized = 1;
    }

    return allocate(size);
}

// Free previously allocated memory
void ffree(void* ptr) {
    if (ptr != NULL) {
        deallocate(ptr);
    }
}