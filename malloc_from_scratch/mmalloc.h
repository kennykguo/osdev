#ifndef MMALLOC_H
#define MMALLOC_H

// Allocate memory of given size (automatically initializes if needed)
void* mmalloc(unsigned int size);

// Free previously allocated memory
void ffree(void* ptr);


#endif
