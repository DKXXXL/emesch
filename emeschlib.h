//Argl must exists.
//Val must exists.
void __FUNC__ADD()
{
  __obji* a = *(__obji**)(Argl--);
  __obji* b = *(__obji**)(Argl--);
#define ASSERT_TYPE(addr,_type) if(addr -> type != _type){EXITREPORT("ADD: Wrong type.");}
  ASSERT_TYPE(a,CInt);
  ASSERT_TYPE(b,CInt);
  
}



#define DECLARE_FUNC(x) __DECLARE__FUNC(x)
#define __DECLARE__FUNC(FUNCNAME) \
  struct __STRUCT__##FUNCNAME()	  \
  {							\
    const static ptlong* func = __FUNC__FUNCNAME;	\
  }__INSTA__FUNCNAME;				




