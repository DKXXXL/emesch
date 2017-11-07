typedef void (*LABELPT)();
typedef void (*Marker)(void*);
typedef void (*Gothrougher)(Marker);

typedef struct {
    int size;
    void* pt;
} Mempool;

enum MemState{idle = 0, inUse = 1, markedIdle = 2, markedUse = 3};

#define INUSE(mnodep) ((mnodep)->st == inUse)
#define INIDLE(mnodep) ((mnodep)->st == idle)
#define MARKED(mnodep) ((mnodep)->st == markedIdle || (mnodep)->st == markedUse)

typedef struct {
    enum MemState st;
    MemNode* next;
    int size;
} MemNode;

typedef struct {
    int usesize;
    int idlesize;
    void* mempoolInUse;
    void* mempoolIdle;
    MemNode* first;
    Gothrougher gothrough;
} GCHandler;


GCHandler GCInit(Mempool memory, Gothrougher gothrough);

void* GCAlloc(GCHandler* handle, int size);