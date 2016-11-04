
CC := g++
CFLAGS := -g -Wall
INCLUDES := -lglut -lGLU -lGL -lm

CORKSUBDIRS := util file_formats math isct mesh rawmesh accel
CORKINCLUDES := ../cork/src $(addprefix -I ../cork/src/,$(CORKSUBDIRS))

INCLUDES := $(INCLUDES) $(CORKINCLUDES)



EXENAME := sshiftdisplay

$(EXENAME): main.o
	$(CC) $(CFLAGS) -o $(EXENAME) main.o $(INCLUDES)

main.o:: 
	$(CC) $(CFLAGS) -g -c main.cpp $(INCLUDES)

.PHONY: run
run: 
	./$(EXENAME)

.PHONY: clean
clean: 
	rm -rf *.o $(EXENAME)
