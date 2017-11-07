#include "gc.h"

typedef int** VAR;

VAR env;

typedef struct
{
    VAR ct;
    envNode* prev;
} envNode;

typedef struct {
    LABELPT func;
    envNode* ctx;
} closure;

VAR* ENV(int step) {
    int i = 0;
    envNode* pt = (envNode*) env;
    while (i < step) {
        pt = pt -> prev;
        i ++;
    }
    return &(pt->ct);
}

void ADDENV(int number) {

}

#define GETCTX(r) (((closure*)(r))->ctx)
#define GOTOLABEL(r) (((LABELPT)(r))())
#define SAVECTX(r, e) (((closure*)((r)))->ctx = e) 

void LABEL0();

void ENTRY() {
    // Prepare the GC
    // start LABEL0
    LABEL0();

}

void ADD();
void MULT();
void INV();
void NEG();
void CAR();
void CDR();
void PAIR();
void ZEROP();
void SYS();