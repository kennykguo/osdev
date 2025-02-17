CC = gcc
CFLAGS = -m32 -Wall -g -fno-pic

# Object files
OBJS = main.o pprintf.o mmalloc.o mmalloc_asm.o

# Target executable
TARGET = main

# Build the final executable
$(TARGET): $(OBJS)
	$(CC) $(CFLAGS) $(OBJS) -o $(TARGET)

# Compile main.c
main.o: main.c pprintf.h mmalloc.h
	$(CC) $(CFLAGS) -c main.c -o main.o

# Compile pprintf.s
pprintf.o: pprintf.s
	$(CC) $(CFLAGS) -c pprintf.s -o pprintf.o

# Compile mmalloc.c
mmalloc.o: mmalloc.c mmalloc.h
	$(CC) $(CFLAGS) -c mmalloc.c -o mmalloc.o

# Compile mmalloc.s
mmalloc_asm.o: mmalloc.s
	$(CC) $(CFLAGS) -c mmalloc.s -o mmalloc_asm.o

# Clean the build
clean:
	rm -f *.o $(TARGET)
