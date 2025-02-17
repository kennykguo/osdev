NASM = nasm
NASMFLAGS = -f as86 -O0

AR = ar86
ARFLAGS = r

# ----------------------------------------------------------------------------
TARGET = libmikeio.a

# source code directory
S = ../src.io
VPATH = $(S)

HEADER := $(S)
SRC := $(notdir $(wildcard $(TARGET:%=$(S)/*.asm)))

OBJ = $(addsuffix .o, $(basename $(SRC)))

LIBDIR = ../lib

# ----------------------------------------------------------------------------
.PHONY:	all clean install

all: $(TARGET)

$(TARGET): $(OBJ)
	$(AR) $(ARFLAGS) $(TARGET) $(OBJ)

%.o : %.asm
	$(NASM) $(NASMFLAGS) -I $(S)/ $< -o $@

clean:
	$(RM) $(OBJ) $(TARGET)

install:
	cp $(TARGET) $(LIBDIR)
