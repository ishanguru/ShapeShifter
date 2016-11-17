#include "shapeshiftertools.h"

int main(int argc, char **argv) 
{
    char tetra[] = "./cork/samples/tetra.off";
    ssTranslate(tetra, 2.0f, 0.0f, 0.0f);
    ssCopy(tetra, "./cork/samples/tetra2.off");
}
