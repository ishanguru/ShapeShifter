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

#define PI 3.14159265

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

static void die(const char *message) 
{
    fprintf(stderr, "%s\n", message);
    exit(EXIT_FAILURE);
}

int windowWidth = 600; 
int windowHeight = 600; 

double winL = -10; // value at left of window 
double winR = 10; // right
double winB = -10;  // bottom
double winT = 10; // top
double winN = -10; // near
double winF = 10; 

double camTheta = 0.0; 
double camPhi = 90.0; 
double camR = 4.0; 
double camX, camY, camZ; 

CorkTriMesh shape; 
GLuint vbo; // vertex buffer object, stores triangle vertex info
GLuint ibo; // index buffer object, stores indices of triangles

float xTrans, yTrans, zTrans; 
void reshape(int w, int h)
{
    windowWidth = std::max(w, 1); 
    windowHeight = std::max(h, 1);
    CHECK_GL(glViewport(0, 0, windowWidth, windowHeight)); 
    CHECK_GL(glMatrixMode(GL_PROJECTION));
    CHECK_GL(glLoadIdentity()); 
    CHECK_GL(gluPerspective(80, (GLfloat)windowWidth/windowHeight, 1, 1000));
    CHECK_GL(glMatrixMode(GL_MODELVIEW));   
    glutPostRedisplay();
}

void keyboard(unsigned char key, int x, int y)
{
    double dcam = .05; 
    double theta = camTheta*PI/180.0; 
    double phi = camPhi*PI/180.0;
    
    switch (key) {
    case  27: 
    case 'q': 
    case 'Q': 
        exit(0); 
        break; 
    case '+': 
        camR = std::max(.01, camR-dcam);
        camX = camR * sin(theta) * sin(phi);
        camY = camR * cos(theta);
        camZ = camR * sin(theta) * cos(phi); 
        glutPostRedisplay();
        //fprintf(stdout, "cam: (%f, %f, %f): %f\n", camX, camY, camZ, std::sqrt(camX*camX + camY*camY + camZ*camZ));
        break; 
    case '-': 
        camR += dcam; 
        camX = camR * sin(theta) * sin(phi);
        camY = camR * cos(theta);
        camZ = camR * sin(theta) * cos(phi); 
        glutPostRedisplay();
        break; 
    default: 
        break; 
    }
   
    // add other key functions - zoom? translate? animate?
    // toggle axis display
}

void special(int key, int x, int y) 
{
    double dcam = 1; 
    switch (key) {
    // Want to not flip the damn thing when camTheta does the thing; 
    // Change up? 
    case GLUT_KEY_UP: 
        camTheta -= dcam; 
        break; 
    case GLUT_KEY_DOWN: 
        camTheta += dcam; 
        break; 
    case GLUT_KEY_LEFT: 
        camPhi -= dcam; 
        break; 
    case GLUT_KEY_RIGHT: 
        camPhi += dcam; 
        break;
    } 

    double theta = camTheta*PI/180.0; 
    double phi = camPhi*PI/180.0;
    camX = camR * sin(theta) * sin(phi);
    camY = camR * cos(theta);
    camZ = camR * sin(theta) * cos(phi); 
  
    glutPostRedisplay(); 

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

void calcNormals(float *pos, unsigned int *ind, float *norms, int n)
{
    // triangle vertex positions
    float u0, u1, u2, v0, v1, v2, w0, w1, w2; 
    float a0, a1, a2, b0, b1, b2; // edge vectors uv, uw
    float n0, n1, n2; 
    for(int i = 0; i < n; ++i) {
        u0 = pos[ind[i*3]*3];
        u1 = pos[ind[i*3]*3+1];
        u2 = pos[ind[i*3]*3+2];
        v0 = pos[ind[i*3+1]*3];
        v1 = pos[ind[i*3+1]*3+1];
        v2 = pos[ind[i*3+1]*3+2];
        w0 = pos[ind[i*3+2]*3];
        w1 = pos[ind[i*3+2]*3+1];
        w2 = pos[ind[i*3+2]*3+2];
        
        a0 = v0 - u0;
        a1 = v1 - u1;
        a2 = v2 - u2;
        b0 = w0 - u0;
        b1 = w1 - u1;
        b2 = w2 - u2;

        n0 = a1 * b2 - a2 * b1;
        n1 = a2 * b0 - a0 * b2;
        n2 = a0 * b1 - a1 * b0;
        
        float d = std::sqrt(n0*n0 + n1*n1 + n2*n2);
        n0 = n0/d;
        n1 = n1/d;
        n2 = n2/d;
        norms[i*3] = n0;
        norms[i*3+1] = n1;
        norms[i*3+2] = n2;
    }
}

void uploadMeshData()
{
    CHECK_GL(glGenBuffers(1, &vbo)); 
    CHECK_GL(glGenBuffers(1, &ibo)); 
    CHECK_GL(glBindBuffer(GL_ARRAY_BUFFER, vbo)); 
    CHECK_GL(glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, ibo));    
 
    //CHECK_GL(glBufferData(GL_ARRAY_BUFFER, shape.n_vertices*3*sizeof(float), shape.vertices, GL_STATIC_DRAW));

    // Allocate enough buffer space for positions + normals
    CHECK_GL(glBufferData(GL_ARRAY_BUFFER, shape.n_vertices*6*sizeof(float), 0, GL_STATIC_DRAW));
    // Upload position data
    CHECK_GL(glBufferSubData(GL_ARRAY_BUFFER, 0, shape.n_vertices*3*sizeof(float), shape.vertices));
    // Calc and upload normal data 
    float *norms = (float *)malloc(shape.n_triangles*3*3*sizeof(float));
    fprintf(stdout, "norms: %d, size %d tri: %d\n", norms, (shape.n_vertices*3*sizeof(float)), shape.n_triangles);
    if (!norms) {
        die("malloc failed");
    }
    fprintf(stdout, "calcin' norms\n");
    calcNormals(shape.vertices, shape.triangles, norms, shape.n_triangles);
    CHECK_GL(glBufferSubData(GL_ARRAY_BUFFER, shape.n_vertices*3*sizeof(float), shape.n_vertices*3*sizeof(float), norms));
    fprintf(stdout, "bout to free them norms\n");
    free(norms);
    fprintf(stdout, "freed them norms\n");

    CHECK_GL(glBufferData(GL_ELEMENT_ARRAY_BUFFER, shape.n_triangles*3*sizeof(uint), shape.triangles, GL_STATIC_DRAW)); 
 
    CHECK_GL(glBindBuffer(GL_ARRAY_BUFFER, 0)); 
    CHECK_GL(glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, 0)); 
}

void display()
{
    glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
    CHECK_GL(glEnable(GL_LIGHTING));
    CHECK_GL(glEnable(GL_LIGHT0));
    CHECK_GL(glEnable(GL_DEPTH_TEST));

    glMatrixMode(GL_MODELVIEW);
    glLoadIdentity();
    CHECK_GL(gluLookAt(camX, camY, camZ, 0, 0, 0, 0, 1, 0));
    //CHECK_GL(gluLookAt(camX, camY, camZ, 0, 0, 0, 0, 1, 0));


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
    CHECK_GL(glEnableClientState(GL_NORMAL_ARRAY));
    CHECK_GL(glNormalPointer(GL_FLOAT, 0, (char *)(shape.n_triangles*3)));

    //CHECK_GL(glDrawArrays(GL_TRIANGLES, 0, 3*shape.n_triangles)); 

    CHECK_GL(glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, ibo));
    CHECK_GL(glDrawElements(GL_TRIANGLES, shape.n_triangles*3, GL_UNSIGNED_INT, (void*)0));

    //CHECK_GL(glDrawElements(GL_TRIANGLES, 3, GL_UNSIGNED_INT, (void *)0)); 

    CHECK_GL(glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, 0));
    CHECK_GL(glDisableClientState(GL_VERTEX_ARRAY));
    CHECK_GL(glDisableClientState(GL_NORMAL_ARRAY));
    CHECK_GL(glBindBuffer(GL_ARRAY_BUFFER, 0)); 

    CHECK_GL(glDisable(GL_LIGHTING));
    CHECK_GL(glDisable(GL_LIGHT0));
    CHECK_GL(glDisable(GL_DEPTH_TEST));
 
    glutSwapBuffers(); 

}

void initOpenGLandGLUT(int argc, char **argv)
{
    glutInit(&argc, argv); 
    glutInitDisplayMode(GLUT_DEPTH|GLUT_DOUBLE|GLUT_RGBA);
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
    glutSpecialFunc(special);

   // Set background color to gray
    CHECK_GL(glClearColor(.7, .7, .7, 1.0));     

    // Set up lighting and material properties
    GLfloat lightPos0[] = {0.0, 10.0, 0.0, 10.0}; 
    GLfloat lightAmb0[] = {0.2, 0.2, 0.2, .0}; 
    GLfloat lightDiff0[] = {1.0, 0.0, 0.0, 1.0}; 
    GLfloat lightSpec0[] = {1.0, 0.0, 0.0, 0.0}; 
 
    CHECK_GL(glLightfv(GL_LIGHT0, GL_POSITION, lightPos0));   
    CHECK_GL(glLightfv(GL_LIGHT0, GL_AMBIENT, lightAmb0)); 
    CHECK_GL(glLightfv(GL_LIGHT0, GL_DIFFUSE, lightDiff0)); 
    CHECK_GL(glLightfv(GL_LIGHT0, GL_SPECULAR, lightSpec0));    

    CHECK_GL(glEnable(GL_COLOR_MATERIAL));
    CHECK_GL(glShadeModel (GL_FLAT));
 
    // Set camera coordinates
    double theta = camTheta*PI/180.0; 
    double phi = camPhi*PI/180.0;
    camX = camR * sin(theta) * sin(phi);
    camY = camR * cos(theta);
    camZ = camR * cos(theta) * sin(phi); 
   
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

    fprintf(stdout, "loading mesh\n");
    loadMesh(argv[1], &shape);     
    fprintf(stdout, "loaded mesh\n");
    fprintf(stdout, "uploading mesh\n");
   
    uploadMeshData(); 
    fprintf(stdout, "uploaded mesh\n");
    glutMainLoop(); 
}
