#include "gc.h"
#include <stdlib.h>


MemNode* _GCAlloc(MemNode* pt, int size);
void markAndSweep(GCHandler* handler);
void copyAllObjects(MemNode* oldarea, MemNode* newarea);

GCHandler GCInit(Mempool memUse, Mempool memIdle, Gothrougher gothrough)
{
    GCHandler ret;
    ret.usesize = memUse.size;
    ret.idlesize = memIdle.size;
    ret.mempoolInUse = memUse.pt;
    ret.mempoolIdle = memIdle.pt;
    MemNode* firstnodept = (MemNode*)(memUse.pt);
    firstnodept->st = idle;
    firstnodept->next = NULL;
    firstnodept->size = memUse.size - sizeof(MemNode);
    ret.first = firstnodept;
    ret.gothrough = gothrough;
    return ret;
}

void* GCAlloc(GCHandler* handle, int size){
    MemNode* ret = _GCAlloc(handle->first, size);
    if(ret == NULL) {
        // start gc
        markAndSweep(handle);
        ret = _GCAlloc(handle->first, size);
        if(ret == NULL) {
            exit(-1);
        } 
    }
    return (void*)(((char*)ret) + sizeof(MemNode));

}

MemNode* _GCAlloc(MemNode* pt, int size) {
    if(pt == NULL) {
        return NULL;
    }

    if(INIDLE(pt) && pt->size >= size) {
        int remainSize = pt->size - size;
        if(remainSize > 2*sizeof(MemNode)) {
            MemNode* newpt = (MemNode*)(((char*)pt) + sizeof(MemNode) + size);
            newpt -> st = idle;
            newpt -> next = pt -> next;
            pt -> next = newpt;
            newpt -> size = pt-> size - size - sizeof(MemNode);
            pt->size = size;
            pt->st = inUse;
        } 
        return pt;

    } else {
        return _GCAlloc(pt->next, size);
    }
}

void marker(void* mem) {
    char* pt = mem;
    MemNode* node = (MemNode*)(pt - sizeof(MemNode));
    node->st = markedUse;
}

int marked(void* mem) {
    char* pt = mem;
    MemNode* node = (MemNode*)(pt - sizeof(MemNode));
    return (node->st == markedUse);
}

void markAndSweep(GCHandler* handler) {
    MemNode* pt = handler->first;
    while(pt != NULL) {
        if(MARKED(pt)) {
            pt->st = inUse;
        } else if (INUSE(pt)) {
            pt -> st = idle;
        }
    }
}

void copyGC(GCHandler* handler) {
    handler->gothrough(marker, marked);
    MemNode* pt = handler->first;
    MemNode* newfirst;
    MemNode* newlast;
    while(pt!=NULL && ! (MARKED(pt))) {
        pt = pt -> next;
    }
    newfirst = pt;
    newlast = pt;
    while(pt!=NULL) {
        if(MARKED(pt)) {
            newlast -> next = pt;
            newlast = pt;
        }
        pt = pt -> next;
    }
    // Initialize idle mempool
    int interchangesize = handler->usesize;
    handler->usesize = handler ->idlesize;
    handler->idlesize = interchangesize;

    void* interchangemem = handler -> mempoolIdle;
    handler -> mempoolIdle = handler -> mempoolInUse;
    handler -> mempoolInUse = interchangemem;

    MemNode* first = handler -> mempoolInUse;
    first -> st = idle;
    first -> next = NULL;
    first -> size = handler -> usesize - sizeof(MemNode);

    copyAllObjects(newfirst, first);

    handler -> first = first;

    //Reconnection of the  pointer pointing

}

void mcpy(char* from, char* to, int size) {
    int i = 0;
    while(i < size) {
        *to = *from;
        to ++; from ++;
    }
}

void copyAllObjects(MemNode* oldarea, MemNode* newarea) {
    MemNode* pt = oldarea;
    while(pt != NULL) {
        MemNode* newregion = _GCAlloc(newarea, pt->size);
        char* copyfrom = ((char*)pt) + sizeof(MemNode);
        char* copyto = ((char*)newregion) + sizeof(MemNode);
        mcpy(copyfrom, copyto, pt->size);
        pt = pt -> next;
    }
}
