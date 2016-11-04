#include <GL/gl.h>
#include <GL/glu.h>
#include <GL/glut.h>
#include <GL/glext.h>
#include <cstdio>
#include <cmath>
#include <algorithm> // why is min/max in here??

#include "files.h"
#include "cork.h"

int windowWidth = 600; 
int windowHeight = 500; 

double winL = -100; // value at left of window 
double winR = 100; // right
double winB = -100;  // bottom
double winT = 100; // top
double winN = 0; // near
double winF = 100; 

void reshape(int w, int h)
{
    glViewport(0, 0, w, h); 
    windowHeight = std::max(w, 1); 
    windowHeight = std::max(h, 1);
    glMatrixMode(GL_PROJECTION);
    glLoadIdentity(); 
    glOrtho(winL, winR, winB, winT, winN, winF);
}

void keyboard(unsigned char key, int x, int y)
{
    if (key == 27 || key == 'q' || key == 'Q') {
        exit(0); 
    }
    // add other key functions - zoom? translate? animate?
}

// Upon mouse interaction
void mouse(int button, int state, int x, int y)
{
    // zoom, translate camera, ... 

}

// Run when not displaying... 
void idle() 
{


}

void display()
{
    glClear(GL_COLOR_BUFFER_BIT);
    glMatrixMode(GL_MODELVIEW);
    glLoadIdentity(); 

    // Draw things

    glutSwapBuffers(); 

}

void initOpenGLandGLUT(int argc, char **argv)
{
    glutInit(&argc, argv); 
    glutInitDisplayMode(GLUT_DOUBLE|GLUT_RGBA);
    glutInitWindowSize(windowWidth, windowHeight);
    
    // Position the window in the center of the screen
    int screenW = glutGet(GLUT_SCREEN_WIDTH);
    int screenH = glutGet(GLUT_SCREEN_HEIGHT);
    if (screenW > 0 && screenH > 0) {
        glutInitWindowPosition(windowWidth/2, windowHeight/2);
    }

    glutCreateWindow("Shapeshifter"); 
    glutDisplayFunc(display); 
    glutReshapeFunc(reshape);
    glutKeyboardFunc(keyboard); 
    //glutMotionFunc(motion);
    glutMouseFunc(mouse); 
    glutIdleFunc(idle); 

    glDisable(GL_LIGHTING); 
    glDisable(GL_DEPTH_TEST); 
    // Set background color to gray
    glClearColor(.7, .7, .7, 1.0);     

    reshape(windowWidth, windowHeight); 
    glutMainLoop(); 

}

void loadMesh(string fileName, CorkTriMesh *out)
{
    Files::FileMesh filemesh; 
    if(Files::readTriMesh(fileName, &filemesh) > 0) {
        fprintf(stderr, "Unable to load file from $s\n", fileName); 
        exit(1); 
    }
    file2corktrimesh(filemesh, out); 

}


int main(int argc, char **argv)
{
    if (argc != 2) {
        fprintf(stdout, "usage: sshapedisplay [shapefile.OFF]\n");
        return 0;
    }
    
    // read the thing
    CorkTriMesh in; 

    initOpenGLandGLUT(argc, argv); 
    glutMainLoop(); 
}
