
#include <stdlib.h>
#include <unistd.h>
#include <string.h>
#include <stdio.h>
#include <fstream>
#include <sys/wait.h>

#include "shapeshiftertools.h"

char corkexe[] = "./cork/bin/cork";
char dispexe[] = "./display/sshiftdisplay";

// Copy shapeIn to create shapeOut
void ssCopy(char *shapeIn, char *shapeOut)
{
    std::ifstream src(shapeIn, std::ios::binary);
    std::ofstream dst(shapeOut, std::ios::binary);
    dst << src.rdbuf();
}

// Create shapeOut from shapeIn1 - shapeIn2
void ssDifference(char *shapeIn1, char *shapeIn2, char *shapeOut)
{
    pid_t pid;
    int status; 

    if ((pid = fork()) < 0) { // fork error :(
        exit(1); 
    }    
    else if (pid == 0) { // child
        execlp(corkexe, corkexe, "-diff", shapeIn1, 
                shapeIn2, shapeOut, (char *)NULL); 
        exit(127); 
    }
    else { // parent
        // Wait for child to finish since probably need this file
        if ((pid = waitpid(pid, &status, 0)) < 0) {
            fprintf(stdout, "Something went horribly wrong; difference\n"); 
            exit(1);
        }
    }
}

// Create shapeOut from the intersection of shapeIn1, shapeIn2
void ssIntersect(char *shapeIn1, char *shapeIn2, char *shapeOut)
{
    pid_t pid;
    int status; 

    if ((pid = fork()) < 0) { // fork error :(
        exit(1); 
    }    
    else if (pid == 0) { // child
        execlp(corkexe, corkexe, "-isct", shapeIn1, 
                shapeIn2, shapeOut, (char *)NULL); 
        exit(127); 
    }
    else { // parent
        // Wait for child to finish since probably need this file
        if ((pid = waitpid(pid, &status, 0)) < 0) {
            fprintf(stdout, "Something went horribly wrong; intersect\n"); 
            exit(1);
        }
    }
}

// Reflect the input shape across the plane ax + by + cz = 0
void ssReflect(char *shapeIn, float a, float b, float c)
{
    pid_t pid;
    int status; 

    char abuf[20];
    char bbuf[20];
    char cbuf[20];

    snprintf(abuf, 20, "%f", a);
    snprintf(bbuf, 20, "%f", b);
    snprintf(cbuf, 20, "%f", c);
    if ((pid = fork()) < 0) { // fork error :(
        exit(1); 
    }    
    else if (pid == 0) { // child
        execlp(corkexe, corkexe, "-reflect", shapeIn, 
                abuf, bbuf, cbuf, (char *)NULL); 
        exit(127); 
    }
    else { // parent
        // Wait for child to finish since probably need this file
        if ((pid = waitpid(pid, &status, 0)) < 0) {
            fprintf(stdout, "Something went horribly wrong with reflect\n"); 
            exit(1);
        }
    }
}

// Open a display for the shape
void ssRender(char *shapeIn)
{
    pid_t pid;
    int status; 

    if ((pid = fork()) < 0) { // fork error :(
        exit(1); 
    }    
    else if (pid == 0) { // child
        execlp(dispexe, dispexe, shapeIn, (char *)NULL); 
        exit(127); 
    }
    else { // parent
        // Wait for child to finish since probably need this file
        if ((pid = waitpid(pid, &status, 0)) < 0) {
            fprintf(stdout, "Something went horribly wrong; render\n"); 
            exit(1);
        }
    }
}

// Rotate the input shape around the x,y,z axes
void ssRotate(char *shapeIn, float x, float y, float z)
{
    pid_t pid;
    int status; 

    char xbuf[20];
    char ybuf[20];
    char zbuf[20];

    snprintf(xbuf, 20, "%f", x);
    snprintf(ybuf, 20, "%f", y);
    snprintf(zbuf, 20, "%f", z);
    if ((pid = fork()) < 0) { // fork error :(
        exit(1); 
    }    
    else if (pid == 0) { // child
        execlp(corkexe, corkexe, "-rotate", shapeIn, 
                xbuf, ybuf, zbuf, (char *)NULL); 
        exit(127); 
    }
    else { // parent
        // Wait for child to finish since probably need this file
        if ((pid = waitpid(pid, &status, 0)) < 0) {
            fprintf(stdout, "Something went horribly wrong; rotate\n"); 
            exit(1);
        }
    }
}

// Save the input shape to file 
// Clearly kind of dumb if we have file representations of shapes anyway
void ssSave(char *shapeIn, char *shapeFile)
{
    ssCopy(shapeIn, shapeFile); // Since at the moment it's the same thing
}

// Scale the input shape by x,y,z 
void ssScale(char *shapeIn, float x, float y, float z)
{
    pid_t pid;
    int status; 

    char xbuf[20];
    char ybuf[20];
    char zbuf[20];

    snprintf(xbuf, 20, "%f", x);
    snprintf(ybuf, 20, "%f", y);
    snprintf(zbuf, 20, "%f", z);
    if ((pid = fork()) < 0) { // fork error :(
        exit(1); 
    }    
    else if (pid == 0) { // child
        execlp(corkexe, corkexe, "-scale", shapeIn, 
                xbuf, ybuf, zbuf, (char *)NULL); 
        exit(127); 
    }
    else { // parent
        // Wait for child to finish since probably need this file
        if ((pid = waitpid(pid, &status, 0)) < 0) {
            fprintf(stdout, "Something went horribly wrong; scale\n"); 
            exit(1);
        }
    }
}

// Translate the input shape by x,y,z 
void ssTranslate(char *shapeIn, float x, float y, float z)
{
    pid_t pid;
    int status; 

    char xbuf[20];
    char ybuf[20];
    char zbuf[20];

    snprintf(xbuf, 20, "%f", x);
    snprintf(ybuf, 20, "%f", y);
    snprintf(zbuf, 20, "%f", z);
    if ((pid = fork()) < 0) { // fork error :(
        exit(1); 
    }    
    else if (pid == 0) { // child
        execlp(corkexe, corkexe, "-translate", shapeIn, 
                xbuf, ybuf, zbuf, (char *)NULL); 
        exit(127); 
    }
    else { // parent
        // Wait for child to finish since probably need this file
        if ((pid = waitpid(pid, &status, 0)) < 0) {
            fprintf(stdout, "Something went horribly wrong\n"); 
            exit(1);
        }
    }

}


// Create shapeOut from the union of shapeIn1, shapeIn2
void ssUnion(char *shapeIn1, char *shapeIn2, char *shapeOut)
{
    pid_t pid;
    int status; 

    if ((pid = fork()) < 0) { // fork error :(
        exit(1); 
    }    
    else if (pid == 0) { // child
        execlp(corkexe, corkexe, "-diff", shapeIn1, 
                shapeIn2, shapeOut, (char *)NULL); 
        exit(127); 
    }
    else { // parent
        // Wait for child to finish since probably need this file
        if ((pid = waitpid(pid, &status, 0)) < 0) {
            fprintf(stdout, "Something went horribly wrong; difference\n"); 
            exit(1);
        }
    }

}
