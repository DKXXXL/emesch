#include "emeschlib.h"
#include "stdio.h"
#include "stdlib.h"

extern VAR reg[];

#define isliteral(v) ((v).ty == _lNum || (v).ty == _lBool || (v).ty == _lQuote || (v).ty == _lString)

void gothrough(Marker marker, Marked marked) {
    _gothrough(env);
    for(int i = 0; i < sizeof(reg) / sizeof(VAR); i ++) {
        _gothrough(reg[i]);
    }
}

void _gothrough(VAR v, Marker marker, Marked marked) {
    if(!isliteral(v) && !marked(v.ct.pt)) {
        marker(v.ct.pt);
        if(v.ty == _closure) {
            closure* pt = v.ct.pt;
            _gothrough(pt->ctx, marker, marked);
        }else if(v.ty == _pair) {
            pair* pt = v.ct.pt;
            _gothrough(pt->fst, marker, marked);
            _gothrough(pt->snd, marker, marked);
        } else if(v.ty == _envNode) {
            envNode* pt = v.ct.pt;
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


#define ASSERT(n,x) if(!(n)){printf(x);exit(-1);}
void ADD()
{
    ASSERT(reg[1].ty == _lNum || reg[1].ty == _lBool, "Not Addable.");
    ASSERT(reg[2].ty == _lNum || reg[2].ty == _lBool, "Not Addable.");    
    ASSERT(reg[2].ty == reg[1].ty , "Not Addable.");
    double a = (double)(reg[1].ct.dat);
    double b = (double)(reg[2].ct.dat);
    a = a + b;
    reg[0] = reg[3];
    reg[1].ct = (int**)a;
    APPLY();

}
void MULT(){
    ASSERT(reg[1].ty == _lNum || reg[1].ty == _lBool, "Not multipliable.");
    ASSERT(reg[2].ty == _lNum || reg[2].ty == _lBool, "Not multipliable.");    
    ASSERT(reg[2].ty == reg[1].ty , "Not multipliable.");
    double a = (double)(reg[1].ct.dat);
    double b = (double)(reg[2].ct.dat);
    a = a * b;
    reg[0] = reg[3];
    reg[1].ct = (int**)a;
    APPLY();

}
void INV(){
    ASSERT(reg[1].ty == _lNum, "Not invertible.");
    double a = (double)(reg[1].ct.dat);
    ASSERT(a!=0, "Divide 0!");
    a = 1 / a;
    reg[0] = reg[2];
    reg[1].ct = (int**)a;
    APPLY();

}
void NEG(){
    ASSERT(reg[1].ty == _lNum || reg[1].ty == _lBool, "Not Negatable.");
    double a = (double)(reg[1].ct.dat);
    a = (reg[1].ty == _lBool? 1 - a : -a);
    reg[0] = reg[2];
    reg[1].ct = (int**)a;
    APPLY();

}
void CAR(){
    ASSERT(reg[1].ty == _pair, "Not a Pair.");
    reg[0] = reg[2];
    reg[1] = ((pair*)(reg[1].ct.pt))->first;
    APPLY();
}
void CDR() {
    ASSERT(reg[1].ty == _pair, "Not a Pair.");
    reg[0] = reg[2];
    reg[1] = ((pair*)(reg[1].ct.pt))->second;
    APPLY();
}
void PAIR() {
    VAR ret;
    ret.ty = _pair;
    pair* retpair = alloc(_pair, sizeof(pair));
    retpair->first = reg[1];
    retpair->second = reg[2];
    ret.ct.pt = retpair;
    reg[0] = reg[3];
    reg[1] = ret;
    APPLY();
    
}
void ZEROP() {
    ASSERT(reg[1].ty == _lNum, "Not a number.");
    double a = (double)(reg[1].ct.dat);
    double b = (a == 0)? 1 : 0;
    VAR ret;
    ret.ty = _lBool;
    ret.ct.dat = b;
    reg[0] = reg[2];
    reg[1] = ret;
    APPLY();
}


void SYS() {
    // Only String and Num now
    if(reg[2].ty == _lNum) {
        printf("%d", reg[2].ct.dat);
    } else if(reg[2].ty == _lString) {
        printf("%s", reg[2].ct.pt);
    }
    reg[0] = reg[3];
    reg[1].ty = _lBool;
    reg[1].ct.dat = 1;
    APPLY();
}