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
  _register = (void*)&__mempool[pt_m];					\
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
  _register = (void*)&__mempool[pt_m];					\
  ((__obji*)&(__mempool[pt_m]))->_t = type;				\
  ((__obji*)&(__mempool[pt_m]))->_size = sizeof(obj);			\
  ((__obji*)&(__mempool[pt_m]))->content = (void*)&(__mempool[pt_m+sizeof(__obji)]); \
  memcpy(&(__mempool[pt_m + sizeof(__obji)]), &obj ,sizeof(obj));		\
  pt_m = ASSIGNNECEP(obj);\
  
  
#define ASSIGN21(_register,obj) _ASSIGN22(_register,obj,CInt)
#define ASSIGN22(_register,obj) _ASSIGN21(_register,obj,CString)
#define ASSIGN23(_register,obj) _ASSIGN22(_register,obj,CBool)
#define ASSIGN24(_register,obj) _ASSIGN22(_register,obj,CLambda)
#define ASSIGN25(_register,obj) Object_type_wrong




void __gc()
{
  
}
