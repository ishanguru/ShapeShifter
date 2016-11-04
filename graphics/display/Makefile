
CC = g++
CFLAGS = -g -Wall
INCLUDES = -lglut -lGLU -lGL -lm

EXENAME = sshiftdisplay

$(EXENAME): main.o
	$(CC) $(CFLAGS) -o $(EXENAME) main.o $(INCLUDES)

main.o:: 

.PHONY: run
run: 
	./$(EXENAME)

.PHONY: clean
clean: 
	rm -rf *.o $(EXENAME)
