#include <stdlib.h>
#include <stdio.h>
#define POOLSIZE 4096
#define ACTUALPOOLSIZE POOLSIZE*sizeof(ptlong) 

enum bool {False,True};

enum __type 
  {
    CInt,
    CString,
    CQuote,
    CBool,
    CLambda,
    CPair,
    _RStack
  };

struct __CPair
{
  struct __obji* a;
  struct __obji* b;
}

typedef struct __ptlong
{
  void* __c;
}ptlong;

typedef struct ___content
{}__content;

typedef struct ___obji
{
  __type _t;
  long _size;  // Unit: sizeof(char)
  ptlong* _copied;
  __content _content;

}__obji;

void gc();

static char __mempool[ACTUALPOOLSIZE];
static char __newmempool[ACTUALPOOLSIZE];
static long pt_m = 0;

#define ASSIGNNECEP(obj) sizeof(obj) + sizeof(__obji) + pt_m
#define EXITREPORT(s) perror(s),abort() 

#define _ASSIGN21(_register,obj,type)	      \
  if(ASSIGNNECEP(obj) > ACTUALPOOLSIZE)	      \
    {\
      __gc();\
      if(ASSIGNNECEP(obj) > ACTUALPOOLSIZE)\
	{EXITREPORT("Out of memory.");}\
    }\
  *_register = (void*)&__mempool[pt_m];					\
  ((__obji*)&(__mempool[pt_m]))->_t = type;				\
  ((__obji*)&(__mempool[pt_m]))->_copied = 0;				\
  ((__obji*)&(__mempool[pt_m]))->_size = sizeof(obj);			\
  //  ((__obji*)&(__mempool[pt_m]))->content = (void*)&(__mempool[pt_m+sizeof(__obji)]); \
  memcpy(&(((__obji*)&(__mempool[pt_m]))->content),obj,sizeof(obj));	\
  pt_m = ASSIGNNECEP(obj);


#define _ASSIGN22(_register,obj,type)	      \
  if(ASSIGNNECEP(obj) > ACTUALPOOLSIZE)	      \
    {\
      __gc();\
      if(ASSIGNNECEP(obj) > ACTUALPOOLSIZE)\
	{EXITREPORT("Out of memory.");}\
    }\
  *_register = (void*)&__mempool[pt_m];					\
  ((__obji*)&(__mempool[pt_m]))->_t = type;				\
  ((__obji*)&(__mempool[pt_m]))->_copied = 0;				\
  ((__obji*)&(__mempool[pt_m]))->_size = sizeof(obj);			\
  //((__obji*)&(__mempool[pt_m]))->content = (void*)&(__mempool[pt_m+sizeof(__obji)]); \
  memcpy(&(((__obji*)&(__mempool[pt_m]))->content),&obj,sizeof(obj));	\
  pt_m = ASSIGNNECEP(obj);						
  


#define ASSIGN21(_register,obj) _ASSIGN22(_register,obj,CInt)
#define ASSIGN22(_register,obj) _ASSIGN21(_register,obj,CString)
#define ASSIGN23(_register,obj) _ASSIGN22(_register,obj,CBool)
#define ASSIGN24(_register,obj) _ASSIGN22(_register,obj,CLambda)
#define ASSIGN25(_register,obj) *_register = obj

#define ASSIGN20(_register,obj) Object_type_wrong

#define ptlongtopt(ptl) ptl.__c
////
////All Register (except Ret) store the address of __obji which has content of the information
////

#define __getobjiaddr(reg) (*((__obji**)reg))

#define lambdavaraddr(reg,offset)		\
  (((ptlong*)(&(__getobjiaddr(reg)->content)))+1+offset)
// +1 is because there is a function entrance
// It would be silly to not use magic number here

#define VARCATCH(_register,x,y,varname,struname)			\
  ((struname *) (__getobjiaddr(_register))) -> varname.__c = lambdavaraddr(Env,y)


//VARCATCH means the function which deal with the situation that the catched var is not referred in
//the function in which VARCATCH is excuted
//VARCATCHREF means the function which deals with the situation that the catched var is referred
//in the function in which VARCATCHREF is excuted
//SETVAR,SETVARREF is also mean dealing with REFered situation or not.


#define VARCATCHREF(_register,x,y,varname,strucname)		\
  ((struname *) (__getobjiaddr(_register))) -> varname = *(lambdavaraddr(Env,y))
  
#define SETVAR(x,y,_register)			\
  (*(lambdavaraddr(Env,y))).__c = (ptlong*)__getobjiaddr(_register)

#define SETVARREF(x,y,_register)		\
  (**(ptlong**)(lambdavaraddr(Env,y))).__c = (ptlong*)__getobjiaddr(_register)

#define GETVAR(x,y,_register)			\
  *_register = *(lambdavaraddr(Env,y))

#define GETVARREF(x,y,_register)		\
  *_register = **(ptlong**)(lambdavaraddr(Env,y))

#define CALL(_reg)				\
  ((void(*)())&(__getobjiaddr(_reg)->content))()


#define stacksize(from) __stacksize__(from)
#define __stacksize__(from) ((long)(from - b##from))*sizeof(ptlong*)

#define SAVE(from,to) __SAVE__(from,to)
#define __SAVE__(from, to)					\
  if(stacksize(from) + sizeof(__obji) + pt_m > ACTUALPOOLSIZE)	\
    {__gc();							\
      if(stacksize(from) + sizeof(__obji) + pt_m > ACTUALPOOLSIZE)	\
	{EXITREPORT("Out of Memory.");}}\
  *to = (void*)&__mempool[pt_m];					\
  ((__obji*)&(__mempool[pt_m]))->_t = _Rstack;				\
  ((__obji*)&(__mempool[pt_m]))->_copied = 0;				\
  ((__obji*)&(__mempool[pt_m]))->_size = stacksize(from);			\
  //  ((__obji*)&(__mempool[pt_m]))->content = (void*)&(__mempool[pt_m+sizeof(__obji)]); \
  memcpy((void*)&(((__obji*)&(__mempool[pt_m]))->content), b##from ,stacksize(from)); \
  pt_m = stacksize(from) + sizeof(__obji) + pt_m;
  

#define LOAD(to,from) __LOAD__(to,from)
#define __LOAD__(to, from)						\
  memcpy(b##to, &((*(__obji**)from)->content) ,(*(__obji**)from)->size); \
  to = b##to + (*(__obji**)from)->size;

void* newObj(long size)
  {
    if(size + pt_m + sizeof(__obji) > ACTUALPOOLSIZE)		
      {						
	__gc();
	if(size + pt_m + sizeof(__obji) > ACTUALPOOLSIZE)
	  {EXITREPORT("Out of memory.");}
      }
    //*_register = (void*)&__mempool[pt_m];					
    ((__obji*)&(__mempool[pt_m]))->_t = type;				
    ((__obji*)&(__mempool[pt_m]))->_copied = 0;				
    ((__obji*)&(__mempool[pt_m]))->_size = size;			
    //((__obji*)&(__mempool[pt_m]))->content = (void*)&(__mempool[pt_m+sizeof(__obji)]); \
    //memcpy(&(((__obji*)&(__mempool[pt_m]))->content),&obj,size);	
    void* ret = &(__mempool[pt_m]);
    pt_m = ASSIGNNECEP(obj);
    return ret;
  }
#define MAX_REGISTER_NUMBER 16

const static ptlong* ___registers[MAX_REGISTER_NUMBER] = {0};
static ptlong** __registerp = ___registers;

#define __copyobj(objiaddr,to)		\
  memcpy(to,objiaddr,objiaddr->size + sizeof(*objiaddr))

static long new_pt;

void __dealwith(const ptlong* regbase,ptlong* regp)
{
  while((regp - regbase) >= 0)
    {
      if(__getobjiaddr(regp)-> _copied == 0)
	{
	      __copyobj(__getobjiaddr(regp),&(__newmempool[new_pt]));
	      __getobjiaddr(regp) -> _copied =  &(__newmempool[new_pt]);
	      new_pt += __getobjiaddr(regp)->size + sizeof(*__getobjiaddr);
	  switch(__getobjiaddr(regp)->_t)
	    {
	    case CLambda:
	      __dealwith(((ptlong*)(__getobjiaddr(regp)->content))+1,
			 ((ptlong*)(__getobjiaddr(regp)->content))+((__getobjiaddr(regp)->size)/sizeof(ptlong)));
	      break;
	    case _RStack:
	      __dealwith(((ptlong*)(__getobjiaddr(regp)->content)),
			 ((ptlong*)(__getobjiaddr(regp)->content))+((__getobjiaddr(regp)->size)/sizeof(ptlong)));
	      break;
	      
	    case CPair :
	      __dealwith(((ptlong*)(__getobjiaddr(regp)->content)),
			 ((ptlong*)(__getobjiaddr(regp)->content))+1);
	      break;

	      ////These magic number are because the corresponding struct
	    default:
	      break;

	    }
	  
	}
      regp--;
    }
}

void __relink(const ptlong* regbase,ptlong* regp)
{
  while((regp - regbase) >= 0)
    {
      
      while(__getobjiaddr(regp)->_copied != 0)
	{
	  __getobjiaddr(regp) = __getobjiaddr(regp)->_copied;
	}

      switch(__getobjiaddr(regp)->_t)
	{
	case CLambda:
	  __relink(((ptlong*)(__getobjiaddr(regp)->content))+1,
		   ((ptlong*)(__getobjiaddr(regp)->content))+((__getobjiaddr(regp)->size)/sizeof(ptlong)));
	  break;
	case _RStack:
	  __relink(((ptlong*)(__getobjiaddr(regp)->content)),
		   ((ptlong*)(__getobjiaddr(regp)->content))+((__getobjiaddr(regp)->size)/sizeof(ptlong)));
	  break;
	  
	case CPair :
	      __relink(((ptlong*)(__getobjiaddr(regp)->content)),
		       ((ptlong*)(__getobjiaddr(regp)->content))+1);
	      break;
	default:
	  break;
	  ////These magic number are because the corresponding struct
	}
	  
   
      regp--;
    }
  
}

void __gc()
{
  //Default : all registers exists
  new_pt = 0;
  __dealwith(bVal,Val);
  __dealwith(bEnv,Env);
  __dealwith(bArgl,Argl);
  __dealwith(bExp,Exp);
  __relink(bVal,Val);
  __relink(bEnv,Env);
  __relink(bArgl,Argl);
  __relink(bExp,Exp);
  pt_m = new_pt;
  char* a = __mempool;
  __mempool = __newmempool;
  __newmempool = a;

}

#include "emeschlib.h"
