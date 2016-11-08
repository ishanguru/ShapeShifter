
CC := g++
CPPFLAGS := -g -Wall -std=c++11
INCLUDES := -lglut -lGLU -lGL -lm

CORKSUBDIRS := util file_formats math isct mesh rawmesh accel
CORKINCLUDES := -I ../cork/src $(addprefix -I ../cork/src/,$(CORKSUBDIRS)) -L ../cork/lib -lcork

INCLUDES := $(INCLUDES) $(CORKINCLUDES)



EXENAME := sshiftdisplay

$(EXENAME): main.o
	$(CC) $(CPPFLAGS) -o $(EXENAME) main.o $(INCLUDES)

main.o:: 
	$(CC) $(CPPFLAGS) -c main.cpp $(INCLUDES)

.PHONY: run
run: 
	./$(EXENAME)

.PHONY: clean
clean: 
	rm -rf *.o $(EXENAME)
