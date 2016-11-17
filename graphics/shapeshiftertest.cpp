#include "shapeshiftertools.h"

int main(int argc, char **argv) 
{
    char tetra1[] = "./cork/samples/tetra.off";
    char tetra2[] = "./cork/samples/tetra2.off";
    char tetra3[] = "./cork/samples/tetra3.off";
    char tetra4[] = "./cork/samples/tetra4.off";   

    ssCopy(tetra1, tetra2); 
    ssCopy(tetra1, tetra3); 

    ssTranslate(tetra2, 0.f, 2.5f, 0.f); 
    ssScale(tetra3, 2.0f, 2.0f, 2.0f); 
    ssUnion(tetra2, tetra3, tetra4); 

    ssRender(tetra4);

}
