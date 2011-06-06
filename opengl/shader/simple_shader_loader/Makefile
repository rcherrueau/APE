CC=gcc
CFLAGS=-Wall -Werror -ansi -std=c99
LDFLAGS=-lGLEW -lGLU -lGL -lglut
EXEC=shader_compiling

.PHONY: clean mrproper

all: $(EXEC)

$(EXEC): shader_compiling.o shader_samples.o main.o
	$(CC) -o $@ $^ $(LDFLAGS)

main.o: shader_compiling.h shader_samples.h

shader_samples.o:	shader_samples.h shader_compiling.h

shader_compiling.o: shader_compiling.h

%.o: %.c
	$(CC) -o $@ -c $< $(CFLAGS)

clean:
	@rm -f *.o

mrproper: clean
	@rm $(EXEC)

