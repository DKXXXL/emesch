#include "emeschlib.h"
#include "stdio.h"
#include "stdlib.h"

extern VAR reg[];



void gothrough(Marker marker, Marked marked) {
    _gothrough(env);
    for(int i = 0; i < sizeof(reg) / sizeof(VAR); i ++) {
        _gothrough(reg[i]);
    }
}

void _gothrough(VAR v, Marker marker, Marked marked) {
    if(!marked(v.ct)) {
        marker(v.ct);
        if(v.ty == _closure) {
            closure* pt = v.ct;
            _gothrough(pt->ctx, marker, marked);
        }else if(v.ty == _pair) {
            pair* pt = v.ct;
            _gothrough(pt->fst, marker, marked);
            _gothrough(pt->snd, marker, marked);
        } else if(v.ty == _envNode) {
            envNode* pt = v.ct;
            _gothrough(pt->ct, marker, marked);
            _gothrough(pt->prev, marker, marked);
        }
    }
}

void ENTRY() {
    void* pool1 = malloc(MEMPOOLSIZE);
    void* pool2 = NULL // malloc(MEMPOOLSIZE);
    Mempool mp1, mp2;
    mp1.size = MEMPOOLSIZE;
    mp2.size = 0;
    mp1.pt = pool1;
    mp2.pt = pool2;
    GCINFO = GCInit(mp1, mp2, gothrough);
    LABEL0();
}



void ADD()
{
    double a = (double)(reg[1].ct);
    double b = (double)(reg[2].ct);
    a = a + b;
    reg[0] = reg[3];
    reg[1].ct = (int**)a;
    APPLY();

}
void MULT(){
    double a = (double)(reg[1].ct);
    double b = (double)(reg[2].ct);
    a = a * b;
    reg[0] = reg[3];
    reg[1].ct = (int**)a;
    APPLY();

}
void INV(){
    double a = (double)(reg[1].ct);
    
    a = 1 / a;
    reg[0] = reg[2];
    reg[1].ct = (int**)a;
    APPLY();

}
void NEG(){
    double a = (double)(reg[1].ct);
    
    a = - a;
    reg[0] = reg[2];
    reg[1].ct = (int**)a;
    APPLY();

}
void CAR(){
    double a = (double)(reg[1].ct);
    
    a = 1 / a;
    reg[0] = reg[2];
    reg[1].ct = (int**)a;
    APPLY();

}
void CDR();
void PAIR();
void ZEROP();
void SYS();