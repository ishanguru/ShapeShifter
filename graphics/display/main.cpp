#define GL_GLEXT_PROTOTYPES

#include <GL/gl.h>
#include <GL/glu.h>
#include <GL/glut.h>
#include <GL/glext.h>
#include <cstdio>
#include <cmath>
#include <algorithm>

#include "files.h"
#include "cork.h"


void check_GL_Error(const char *file, int line) 
{
    GLenum err = glGetError(); 
    while (err != GL_NO_ERROR) {
        fprintf(stderr, "OpenGL Error %x at %s, line %d\n", err, file, line);
        err = glGetError();    
    }
}

// OpenGL Error checking
#define debug
#if defined(debug)
#define CHECK_GL(func) func; \
    check_GL_Error(__FILE__, __LINE__); 
#else 
#define CHECK_GL(func) func;
#endif

int windowWidth = 600; 
int windowHeight = 600; 

double winL = -10; // value at left of window 
double winR = 10; // right
double winB = -10;  // bottom
double winT = 10; // top
double winN = -1; // near
double winF = 1; 

CorkTriMesh shape; 
GLuint vbo; // vertex buffer object, stores triangle vertex info
GLuint ibo; // index buffer object, stores indices of triangles

void reshape(int w, int h)
{
    windowWidth = std::max(w, 1); 
    windowHeight = std::max(h, 1);
    CHECK_GL(glViewport(0, 0, windowHeight, windowHeight)); 
    CHECK_GL(glMatrixMode(GL_PROJECTION));
    CHECK_GL(glLoadIdentity()); 
    CHECK_GL(glOrtho(winL, winR, winB, winT, winN, winF));
}

void keyboard(unsigned char key, int x, int y)
{
    if (key == 27 || key == 'q' || key == 'Q') {
        exit(0); 
    }
    // add other key functions - zoom? translate? animate?
    // toggle axis display
}

// Upon mouse interaction
void mouse(int button, int state, int x, int y)
{
    // zoom, translate camera, ... 

}

// Run when not displaying... 
void idle() 
{
    //glutPostRedisplay();

}

void uploadMeshData()
{
    CHECK_GL(glGenBuffers(1, &vbo)); 
    CHECK_GL(glGenBuffers(1, &ibo)); 
    CHECK_GL(glBindBuffer(GL_ARRAY_BUFFER, vbo)); 
    CHECK_GL(glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, ibo));    
 
    CHECK_GL(glBufferData(GL_ARRAY_BUFFER, shape.n_vertices*3*sizeof(float), shape.vertices, GL_STATIC_DRAW));
    CHECK_GL(glBufferData(GL_ELEMENT_ARRAY_BUFFER, shape.n_triangles*3*sizeof(uint), shape.triangles, GL_STATIC_DRAW)); 
 
    CHECK_GL(glBindBuffer(GL_ARRAY_BUFFER, 0)); 
    CHECK_GL(glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, 0)); 
}

void display()
{
    glClear(GL_COLOR_BUFFER_BIT);
    glMatrixMode(GL_MODELVIEW);
    //glLoadIdentity(); 

    // Draw axes
    glBegin(GL_LINES);  
    glColor3f(1.0, 0.0, 0.0); // x 
    glVertex3f(-100.0, 0.0, 0.0); 
    glVertex3f(100.0, 0.0, 0.0); 
    glColor3f(0.0, 1.0, 0.0); // y
    glVertex3f(0.0, -100.0, 0.0); 
    glVertex3f(0.0, 100.0, 0.0); 
    glColor3f(0.0, 0.0, 1.0); // z
    glVertex3f(0.0, 0.0, -100.0); 
    glVertex3f(0.0, 0.0, 100.0); 
    glEnd(); 

    glColor3f(.2, .2, .2);

    CHECK_GL(glBindBuffer(GL_ARRAY_BUFFER, vbo)); 
    CHECK_GL(glEnableClientState(GL_VERTEX_ARRAY));
    CHECK_GL(glVertexPointer(3, GL_FLOAT, 0, 0)); 

//    CHECK_GL(glDrawArrays(GL_TRIANGLES, 0, 3*shape.n_triangles)); 

    CHECK_GL(glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, ibo));
    CHECK_GL(glDrawElements(GL_TRIANGLES, shape.n_triangles*3, GL_UNSIGNED_INT, (void*)0));

    CHECK_GL(glDisableClientState(GL_VERTEX_ARRAY));
    CHECK_GL(glBindBuffer(GL_ARRAY_BUFFER, 0)); 


/*
    glBegin(GL_TRIANGLES); 
    glColor3f(1.0, 0.0, 0.0); 
    glVertex3f(-1.0f, -1.0f, 0.0f);
    glColor3f(0.0, 1.0, 0.0);  
    glVertex3f(1.0f, -1.0f, 0.0f);
    glColor3f(0.0, 0.0, 1.0);      
    glVertex3f(0.0f, 1.0f, 0.0f); 
    glEnd(); 
  */
  


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
    CHECK_GL(glClearColor(.7, .7, .7, 1.0));     

    reshape(windowWidth, windowHeight); 

}

void loadMesh(std::string fileName, CorkTriMesh *out)
{
    Files::FileMesh fileMesh; 
    if(Files::readTriMesh(fileName, &fileMesh) > 0) {
        fprintf(stderr, "Unable to load file from %s\n", fileName.c_str()); 
        exit(1); 
    }

    // Copy data from FileMesh
    out->n_vertices = fileMesh.vertices.size(); 
    out->n_triangles = fileMesh.triangles.size(); 
    
    out->triangles = new uint[(out->n_triangles) * 3]; 
    out->vertices = new float[(out->n_vertices) * 3]; 
    for (uint i = 0; i < out->n_triangles; ++i) {
        (out->triangles)[3*i+0] = fileMesh.triangles[i].a; 
        (out->triangles)[3*i+1] = fileMesh.triangles[i].b; 
        (out->triangles)[3*i+2] = fileMesh.triangles[i].c; 
    }

    for (uint i = 0; i < out->n_vertices; ++i) {
        (out->vertices)[3*i+0] = fileMesh.vertices[i].pos.x; 
        (out->vertices)[3*i+1] = fileMesh.vertices[i].pos.y; 
        (out->vertices)[3*i+2] = fileMesh.vertices[i].pos.z; 
    }

}


int main(int argc, char **argv)
{
    if (argc != 2) {
        fprintf(stdout, "usage: sshapedisplay [shapefile.OFF]\n");
        return 0;
    }
    
    // read the thing
 
    initOpenGLandGLUT(argc, argv); 

    loadMesh(argv[1], &shape);    

    uploadMeshData(); 
   
    glutMainLoop(); 
   
}
