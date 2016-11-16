
/*
    This is the C++ interface for Shapeshifter. 
    At the moment most of these functions work by calling an executable to 
    operate on shapes stored as ~.off files.

    Perhaps it might be reasonable to later on modify this to pass around 
    pointers to mesh info; constantly reading/writing to disk will be 
    slow af. But for now this will suffice.   
    Most inputs are strings for the filenames of the input/output shapes.
*/

// Copy shapeIn to create shapeOut
void ssCopy(char *shapeIn, char *shapeOut);

// Create shapeOut from shapeIn1 - shapeIn2
void ssDifference(char *shapeIn1, char *shapeIn2, char *shapeOut);

// Create shapeOut from the intersection of shapeIn1, shapeIn2
void ssIntersect(char *shapeIn1, char *shapeIn2, char *shapeOut);

// Reflect the input shape across the plane ax + by + cz = 0
void ssReflect(char *shapeIn, float a, float b, float c);

// Open a display for the shape
void ssRender(char *shapeIn);

// Rotate the input shape around the x,y,z axes
void ssRotate(char *shapeIn, float x, float y, float z);

// Save the input shape to file 
// Clearly kind of dumb if we have file representations of shapes anyway
void ssSave(char *shapeIn, char *shapeFile);

// Scale the input shape by x,y,z 
void ssScale(char *shapeIn, float x, float y, float z);

// Translate the input shape by x,y,z 
void ssTranslate(char *shapeIn, float x, float y, float z);

// Create shapeOut from the union of shapeIn1, shapeIn2
void ssUnion(char *shapeIn1, char *shapeIn2, char *shapeOut);
