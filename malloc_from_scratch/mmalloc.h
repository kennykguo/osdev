#ifndef MMALLOC_H
#define MMALLOC_H

// // Declare the external assembly functions
// extern void* allocate(unsigned int);
// extern void deallocate(void*);
// extern void allocate_init(void);

// Allocate memory of given size (automatically initializes if needed)
void* mmalloc(unsigned int size);

// Free previously allocated memory
void ffree(void* ptr);


#endif
