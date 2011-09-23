CC=gcc
CFLAGS=-Wall -Werror -ansi -std=c99
ifeq ($(shell uname), Darwin)
	LDFLAGS += -framework OpenGL -framework GLUT -lGLEW
else
	LDFLAGS += -lGLEW -lGL -lglut -lGLU
endif
EXEC=mesh_deformers

.PHONY: clean mrproper

all: $(EXEC)

$(EXEC): utils/shader_compiling.o mesh_deformers.o main.o 
	$(CC) -o $@ $^ $(LDFLAGS)

main.o: utils/shader_compiling.h 

shader_compiling.o: utils/shader_compiling.h

%.o: %.c
	$(CC) -o $@ -c $< $(CFLAGS)

clean:
	@rm -f utils/*.o
	@rm -f *.o

mrproper: clean
	@rm $(EXEC)

