NASM = nasm
NASMFLAGS = -f as86 -O0

AR = ar86
ARFLAGS = r

# ----------------------------------------------------------------------------
TARGET = libmikeos.a

# source code directory
S = ../src.os
VPATH = $(S)

HEADER := $(S)
SRC := $(notdir $(wildcard $(TARGET:%=$(S)/*.asm)))

OBJ = $(addsuffix .o, $(basename $(SRC)))

CRT0.o = crt0.o
CRT0.s = crt0.s

LIBDIR = ../lib

# ----------------------------------------------------------------------------
.PHONY:	all clean install

all: $(TARGET) $(CRT0.o)

$(TARGET): $(OBJ)
	$(AR) $(ARFLAGS) $(TARGET) $(OBJ)

$(CRT0.o) : $(CRT0.s)
	as86 $(S)/$(CRT0.s) -o $(CRT0.o)

%.o : %.asm
	$(NASM) $(NASMFLAGS) -I $(S)/ $< -o $@

clean:
	$(RM) $(OBJ) $(TARGET) $(CRT0.o)

install:
	cp $(TARGET) $(CRT0.o) $(LIBDIR)
