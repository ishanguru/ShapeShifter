#define GL_GLEXT_PROTOTYPES

#include <GL/gl.h>
#include <GL/glu.h>
#include <GL/glut.h>
#include <GL/glext.h>
#include <cstdio>
#include <cstring>
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
double upX = 0.0; 
double upY = 1.0; 
double upZ = 0.0; 
CorkTriMesh shape; 
GLuint vbo; // vertex buffer object, stores triangle vertex info
GLuint ibo; // index buffer object, stores indices of triangles

// Set up lighting and material properties
GLfloat lightPos0[] = {0, 10.00001, 0.000001, 10.0}; 
GLfloat lightAmb0[] = {0.1, 0.1, 0.1, 1.0}; 
GLfloat lightDiff0[] = {1.0, 1.0, 1.0, 1.0}; 
GLfloat lightSpec0[] = {1.0, 1.0, 1.0, 1.0}; 
 

float xTrans, yTrans, zTrans; 

#if defined(debug)
float *norms; 
float *fnorms; 
bool drawNorms = 0;
#endif

void reshape(int w, int h)
{
    windowWidth = std::max(w, 1); 
    windowHeight = std::max(h, 1);
    CHECK_GL(glViewport(0, 0, windowWidth, windowHeight)); 
    CHECK_GL(glMatrixMode(GL_PROJECTION));
    CHECK_GL(glLoadIdentity()); 
    CHECK_GL(gluPerspective(80, (GLfloat)windowWidth/windowHeight, .001, 1000));
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
        camX = camR * cos(theta) * cos(phi);
        camY = camR * sin(theta);
        camZ = camR * cos(theta) * sin(phi); 
        glutPostRedisplay();
        break; 
    case '-': 
        camR += dcam; 
        camX = camR * cos(theta) * cos(phi);
        camY = camR * sin(theta);
        camZ = camR * cos(theta) * sin(phi); 
        glutPostRedisplay();
        break; 
#if defined(debug)
    case 'n': 
        drawNorms = !drawNorms; 
        glutPostRedisplay();
        break; 
#endif
    default: 
        break; 
    }
   
    //fprintf(stdout, "cam: (%f, %f, %f): %f\n", camX, camY, camZ, std::sqrt(camX*camX + camY*camY + camZ*camZ));

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
    camX = camR * cos(theta) * cos(phi);
    camY = camR * sin(theta);
    camZ = camR * cos(theta) * sin(phi); 
    if(cos(theta) < 0) 
        upY = -1.0; 
    else 
        upY = 1.0; 
 
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

void calcNormals(float *pos, unsigned int *ind, float *norms, int ntri, int nv)
{
    // triangle vertex positions
    float u0, u1, u2, v0, v1, v2, w0, w1, w2; 
    float a0, a1, a2, ad, b0, b1, b2, bd, c0, c1, c2, cd; // edge vectors uv, uw
    float n0, n1, n2;
    float d = 0; 
    float au, av, aw; // angles 
    // For each triangle, calc normal, set for each vertex
    for(int i = 0; i < ntri; ++i) {
        u0 = pos[ind[i*3]*3];
        u1 = pos[ind[i*3]*3+1];
        u2 = pos[ind[i*3]*3+2];
        v0 = pos[ind[i*3+1]*3];
        v1 = pos[ind[i*3+1]*3+1];
        v2 = pos[ind[i*3+1]*3+2];
        w0 = pos[ind[i*3+2]*3];
        w1 = pos[ind[i*3+2]*3+1];
        w2 = pos[ind[i*3+2]*3+2];
        
        // uv
        a0 = v0 - u0;
        a1 = v1 - u1;
        a2 = v2 - u2;
        ad = std::sqrt(a0*a0 + a1*a1 + a2*a2);
        // uw
        b0 = w0 - u0;
        b1 = w1 - u1;
        b2 = w2 - u2;
        bd = std::sqrt(b0*b0 + b1*b1 + b2*b2);
        // vw
        c0 = w0 - v0;
        c1 = w1 - v1;
        c2 = w2 - v2; 
        cd = std::sqrt(c0*c0 + c1*c1 + c2*c2);
 
        // calc angles
        au = std::acos((a0*b0 + a1*b1 + a2*b2)/(ad*bd)); 
        if (au > PI/2.0) 
            au = PI - au; 
        av = std::acos((a0*c0 + a1*c1 + a2*c2)/(ad*cd));
        if (av > PI/2.0) 
            av = PI - av; 
        aw = std::acos((b0*c0 + b1*c1 + b2*c2)/(bd*cd));
        if (aw > PI/2.0) 
            aw = PI - aw; 
    
        // calc normals

        n0 = a1 * b2 - a2 * b1;
        n1 = a2 * b0 - a0 * b2;
        n2 = a0 * b1 - a1 * b0;
        
        // Add normal for all 3 vertices
        d = std::max(.0001f, std::sqrt(n0*n0 + n1*n1 + n2*n2));
        n0= n0/d; 
        n1 = n1/d;
        n2 = n2/d; 

#if defined(debug)
        fnorms[i*3] = n0;
        fnorms[i*3+1] = n1;
        fnorms[i*3+2] = n2; 
#endif     

        // weigh by angle
  
        norms[ind[i*3]*3] += n0*au;
        norms[ind[i*3]*3+1] += n1*au;
        norms[ind[i*3]*3+2] += n2*au;
        norms[ind[i*3+1]*3] += n0*av;
        norms[ind[i*3+1]*3+1] += n1*av;
        norms[ind[i*3+1]*3+2] += n2*av;
        norms[ind[i*3+2]*3] += n0*aw;
        norms[ind[i*3+2]*3+1] += n1*aw;
        norms[ind[i*3+2]*3+2] += n2*aw;
    }
    
    for (int i = 0; i < nv; ++i) {
        n0 = norms[i*3]; 
        n1 = norms[i*3+1];
        n2 = norms[i*3+2];
        d = std::max(.0001f, std::sqrt(n0*n0 + n1*n1 + n2*n2));
        norms[i*3] = n0/d; 
        norms[i*3+1] = n1/d;
        norms[i*3+2] = n2/d; 
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

#if defined(debug)
    norms = (float *)malloc(shape.n_vertices*3*sizeof(float));
    fnorms = (float *)malloc(shape.n_triangles*3*sizeof(float));
    if (!norms || !fnorms) {
        die("malloc failed");
    }
#else
    float *norms = (float *)malloc(shape.n_vertices*3*sizeof(float));
    if (!norms) {
        die("malloc failed");
    }
#endif

    memset(norms, 0, shape.n_vertices*3*sizeof(float));
    calcNormals(shape.vertices, shape.triangles, norms, shape.n_triangles, shape.n_vertices);
    CHECK_GL(glBufferSubData(GL_ARRAY_BUFFER, shape.n_vertices*3*sizeof(float), shape.n_vertices*3*sizeof(float), norms));
#if !defined(debug)
    free(norms);
#endif

    CHECK_GL(glBufferData(GL_ELEMENT_ARRAY_BUFFER, shape.n_triangles*3*sizeof(uint), shape.triangles, GL_STATIC_DRAW)); 
 
    CHECK_GL(glBindBuffer(GL_ARRAY_BUFFER, 0)); 
    CHECK_GL(glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, 0)); 
}

void display()
{
    glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
    CHECK_GL(glLightfv(GL_LIGHT0, GL_POSITION, lightPos0));   
    CHECK_GL(glEnable(GL_DEPTH_TEST));
    CHECK_GL(glEnable(GL_NORMALIZE));

    glMatrixMode(GL_MODELVIEW);
    glLoadIdentity();

    CHECK_GL(gluLookAt(camX, camY, camZ, 0, 0, 0, upX, upY, upZ));
     
    // Draw axes
    glLineWidth(1.0);
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
    
    glColor3f(.7, .7, .7);
    CHECK_GL(glEnable(GL_LIGHTING));
    CHECK_GL(glEnable(GL_LIGHT0));
    glEnable(GL_COLOR_MATERIAL);
    //glColorMaterial(GL_FRONT_AND_BACK, GL_AMBIENT_AND_DIFFUSE);
    //CHECK_GL(glFrontFace(GL_CW)); 
   
    CHECK_GL(glBindBuffer(GL_ARRAY_BUFFER, vbo)); 
    CHECK_GL(glEnableClientState(GL_VERTEX_ARRAY));
    CHECK_GL(glVertexPointer(3, GL_FLOAT, 0, 0)); 
    CHECK_GL(glEnableClientState(GL_NORMAL_ARRAY));
    CHECK_GL(glNormalPointer(GL_FLOAT, 0, (char *)((intptr_t)(shape.n_vertices*3))));


    CHECK_GL(glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, ibo));
    CHECK_GL(glDrawElements(GL_TRIANGLES, shape.n_triangles*3, GL_UNSIGNED_INT, (void*)0));

#if defined(debug)
    // draw normals yo
    if(drawNorms) {
        glLineWidth(3.0);
        float px, py, pz; 
        glBegin(GL_LINES);
        glColor3f(1.0, 1.0, 1.0);
        for (int i = 0; i < (int)shape.n_vertices; ++i) {
            px = shape.vertices[i*3]; 
            py = shape.vertices[i*3+1];
            pz = shape.vertices[i*3+2];
            glVertex3f(px, py, pz);
            glVertex3f(px+norms[i*3], py+norms[i*3+1], pz+norms[i*3+2]);    
        }
        glEnd(); 
/*
    // draw face normals
    glColor3f(0.0, 1.0, 1.0);
    glBegin(GL_LINES); 
    for (int i = 0; i < (int)shape.n_triangles; ++i) {
        px = shape.vertices[shape.triangles[i*3]*3];
        py = shape.vertices[shape.triangles[i*3]*3+1];
        pz = shape.vertices[shape.triangles[i*3]*3+2];
        glVertex3f(px, py, pz);
        glVertex3f(px+fnorms[i*3], py+fnorms[i*3+1], pz+fnorms[i*3+2]);
    }
    glEnd();
*/
    }
#endif

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
    CHECK_GL(glClearColor(.1, .1, .1, 1.0));     

    CHECK_GL(glLightfv(GL_LIGHT0, GL_POSITION, lightPos0));   
    CHECK_GL(glLightfv(GL_LIGHT0, GL_AMBIENT, lightAmb0)); 
    CHECK_GL(glLightfv(GL_LIGHT0, GL_DIFFUSE, lightDiff0)); 
    CHECK_GL(glLightfv(GL_LIGHT0, GL_SPECULAR, lightSpec0));    
    CHECK_GL(glLightModelf(GL_LIGHT_MODEL_LOCAL_VIEWER, 1));
    CHECK_GL(glShadeModel(GL_SMOOTH));

    // Set camera coordinates
    double theta = camTheta*PI/180.0; 
    double phi = camPhi*PI/180.0;
    camX = camR * cos(theta) * cos(phi);
    camY = camR * sin(theta);
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

    loadMesh(argv[1], &shape);     
   
    uploadMeshData(); 
    glutMainLoop(); 
}
