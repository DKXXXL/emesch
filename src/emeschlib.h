#include "gc.h"

typedef struct {
    enum obtype ty;
    int** ct;
} VAR;

VAR env;

enum obtype{_closure, _pair, _envNode, _literal};

static GCHandler GCINFO;

#define alloc(ty, size) memAlloc(&GCINFO, (ty), (size))

#define MEMPOOLSIZE 1024000

#define APPLY() env = GETCTX(reg0);GOTOLABEL(reg0);

typedef struct
{
    VAR ct;
    VAR prev;
} envNode;

typedef struct {
    LABELPT func;
    VAR ctx;
} closure;



typedef struct {
    VAR fst;
    VAR snd;
} pair;

VAR* ENV(int step) {
    int i = 0;
    envNode* pt = (envNode*) (env.ct);
    while (i < step) {
        pt = pt -> prev.ct;
        i ++;
    }
    return &(pt->ct);
}

void ADDENV(int number) {
    int i = 0;
    while (i < number) {
        envNode* newenvNode = alloc(_envNode, sizeof(envNode));
        newenvNode ->prev = env;
        (env.ct) = newenvNode;
        env.ty = _envNode;
        i ++;
    }
}

VAR CLOSURE(LABELPT f) {
    VAR ret;

    ret.ty = _closure;
    closure* cls = alloc(_closure, sizeof(closure));
    cls->func = f;
    cls->ctx = env;
    ret.ct = cls;
    return ret;
}



#define GETCTX(r) (((closure*)((r).ct))->ctx)
#define GOTOLABEL(r) (((LABELPT)((r).ct))())
#define SAVECTX(r, e) (((closure*)(((r).ct)))->ctx = e) 

void LABEL0();

void ENTRY();

void ADD();
void MULT();
void INV();
void NEG();
void CAR();
void CDR();
void PAIR();
void ZEROP();
void SYS();