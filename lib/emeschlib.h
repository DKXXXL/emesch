#include "gc.h"

enum obtype{_closure, _pair, _envNode, _lNum, _lBool, _lQuote, _lString};

typedef union {
    void* pt;
    double dat;
} Data;

typedef struct {
    enum obtype ty;
    Data ct;
} VAR;

VAR env;


static GCHandler GCINFO;

#define alloc(ty, size) memAlloc(&GCINFO, (ty), (size))

#define MEMPOOLSIZE 1024000

#define APPLY() env = GETCTX(reg[0]);GOTOLABEL(reg[0]);

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





#define GETCTX(r) (((closure*)((r).ct.pt))->ctx)
#define GOTOLABEL(r) (((LABELPT)((((closure*)((r).ct.pt))->func)))())
#define JUMPLABEL(l) ((l)())
#define SAVECTX(r, e) (((closure*)(((r).ct.pt)))->ctx = e) 

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

#define SNUMBER(i) (VAR){_lNum, {.dat = (double)(i)}}
#define SBOOL(i) (VAR){_lBool, {.dat = (double)(i)}}
#define SQuote(i) (VAR){_lQuote, {.pt = (i)}}
#define SString(i) (VAR){_lString, {.pt = (i)}}

#define COND(i,j,k) if((i).ct.dat){j}else{k}

VAR* ENV(int step);
void ADDENV(int number);
VAR CLOSURE(LABELPT f);

static const int REGNUM;
