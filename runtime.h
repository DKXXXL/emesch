#include <stdlib.h>
#include <stdio.h>
#define POOLSIZE 4096
#define ACTUALPOOLSIZE POOLSIZE*sizeof(ptlong) 

enum bool {False,True};

enum __type 
  {
    Cint,
    CString,
    CQuote,
    CBool,
    CLambda,
    _RStack
  };



struct ptlong
{
  void* __c;
};

struct __obji
{
  __type _t;
  long _size;  // Unit: sizeof(char)
  void* _content;

};

static char __mempool[ACTUALPOOLSIZE];
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
  ((__obji*)&(__mempool[pt_m]))->_size = sizeof(obj);			\
  ((__obji*)&(__mempool[pt_m]))->content = (void*)&(__mempool[pt_m+sizeof(__obji)]); \
  memcpy(&(__mempool[pt_m + sizeof(__obji)]),obj,sizeof(obj));		\
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
  ((__obji*)&(__mempool[pt_m]))->_size = sizeof(obj);			\
  ((__obji*)&(__mempool[pt_m]))->content = (void*)&(__mempool[pt_m+sizeof(__obji)]); \
  memcpy(&(__mempool[pt_m + sizeof(__obji)]), &obj ,sizeof(obj));		\
  pt_m = ASSIGNNECEP(obj);\
  
  
#define ASSIGN21(_register,obj) _ASSIGN22(_register,obj,CInt)
#define ASSIGN22(_register,obj) _ASSIGN21(_register,obj,CString)
#define ASSIGN23(_register,obj) _ASSIGN22(_register,obj,CBool)
#define ASSIGN24(_register,obj) _ASSIGN22(_register,obj,CLambda)
#define ASSIGN20(_register,obj) Object_type_wrong

#define ptlongtopt(ptl) ptl.__c


#define lambdavaraddr(reg,offset)  ((*((ptlong**)reg))+1+reg)
// +1 is because there is a function entrance
// It would be silly to not use magic number here

#define VARCATCH(_register,x,y,varname,struname)			\
  (struname *) _register -> varname.__c = lambdavaraddr(Env,y)

//VARCATCH means the function which deal with the situation that the catched var is not referred in
//the function in which VARCATCH is excuted
//VARCATCHREF means the function which deals with the situation that the catched var is referred
//in the function in which VARCATCHREF is excuted
//SETVAR,SETVARREF is also mean dealing with REFered situation or not.


#define VARCATCHREF(_register,x,y,varname,strucname)		\
  (struname *) _register -> varname = *(lambdavaraddr(Env,y))
  
#define SETVAR(x,y,_register)			\
  *(lambdavaraddr(Env,y)) = _register

#define SETVARREF(x,y,_register)		\
  **(ptlong**)(lambdavaraddr(Env,y)) = _register

#define GETVAR(x,y,_register)			\
  _register = *(lambdavaraddr(Env,y))

#define GETVARREF(x,y,_register)		\
  _register = **(ptlong**)(lambdavaraddr(Env,y))


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
  ((__obji*)&(__mempool[pt_m]))->_size = stacksize(from);			\
  ((__obji*)&(__mempool[pt_m]))->content = (void*)&(__mempool[pt_m+sizeof(__obji)]); \
  memcpy(&(__mempool[pt_m + sizeof(__obji)]), b##from ,sizeof(obj));		\
  stacksize(from) + sizeof(__obji) + pt_m;
  

#define LOAD(to,from) __LOAD__(to,from)
#define __LOAD__(to, from)						\
  memcpy(b##to, (*(__obji**)from)->content ,(*(__obji**)from)->size);	\
  to = b##to + (*(__obji**)from)->size;

void __gc()
{
  
}
