//Argl must exists.
//Val must exists.
#define STANDARD_NUMERIAL_TYPE typeof(1)
STANDARD_NUMERIAL_TYPE ____________a;
#define STANDARD_NUMERIAL_POINTER_TYPE typeof(&____________a)

void __FUNC__ADD()
{
  __obji* a = *(__obji**)(Argl--);
  __obji* b = *(__obji**)(Argl--);
#define ASSERT_TYPE(addr,_type) if(addr -> type != _type){EXITREPORT("Wrong type.");}
  ASSERT_TYPE(a,CInt);
  ASSERT_TYPE(b,CInt);
  __obji* ret = newObj(sizeof(int));
  ret->_t = CInt;
  ret -> _copied = 0;  
  *(((STANDARD_NUMERIAL_POINTER_TYPE))(&(ret -> content))) =
    *((STANDARD_NUMERIAL_POINTER_TYPE)(&(a->content))) + *((STANDARD_NUMERIAL_POINTER_TYPE)(&(b->content)));
  ptlong _ret = {ret};
  
  ASSIGN25(Val,_ret);

}



void __FUNC__MINUS()
{
  __obji* a = *(__obji**)(Argl--);
  __obji* b = *(__obji**)(Argl--);
  //#define ASSERT_TYPE(addr,_type) if(addr -> type != _type){EXITREPORT("MINUS: Wrong type.");}
  ASSERT_TYPE(a,CInt);
  ASSERT_TYPE(b,CInt);
  __obji* ret = newObj(sizeof(int));
  ret->_t = CInt;
  ret -> _copied = 0;
  *(((STANDARD_NUMERIAL_POINTER_TYPE))(&(ret -> content))) =
    *((STANDARD_NUMERIAL_POINTER_TYPE)(&(a->content))) - *((STANDARD_NUMERIAL_POINTER_TYPE)(&(b->content)));
  ptlong _ret = {ret};
  
  ASSIGN25(Val,_ret);
  
}
   
 
 void __FUNC__MUTIPLY()
 {
  __obji* a = *(__obji**)(Argl--);
  __obji* b = *(__obji**)(Argl--);
  //#define ASSERT_TYPE(addr,_type) if(addr -> type != _type){EXITREPORT("MUTIPLY: Wrong type.");}
  ASSERT_TYPE(a,CInt);
  ASSERT_TYPE(b,CInt);
  __obji* ret = newObj(sizeof(int));
  ret->_t = CInt;
  ret ->_copied = 0;
  *(((STANDARD_NUMERIAL_POINTER_TYPE))(&(ret -> content))) =
    *((STANDARD_NUMERIAL_POINTER_TYPE)(&(a->content))) * *((STANDARD_NUMERIAL_POINTER_TYPE)(&(b->content)));
  ptlong _ret = {ret};
  
  ASSIGN25(Val,_ret);
 }


void __FUNC__DIVIDE()
{
  __obji* a = *(__obji**)(Argl--);
  __obji* b = *(__obji**)(Argl--);
  //#define ASSERT_TYPE(addr,_type) if(addr -> type != _type){EXITREPORT("MUTIPLY: Wrong type.");}
  ASSERT_TYPE(a,CInt);
  ASSERT_TYPE(b,CInt);
  __obji* ret = newObj(sizeof(int));
  ret->_t = CInt;
  ret -> _copied = 0;
  *(((STANDARD_NUMERIAL_POINTER_TYPE))(&(ret -> content))) =
    *((STANDARD_NUMERIAL_POINTER_TYPE)(&(a->content))) / *((STANDARD_NUMERIAL_POINTER_TYPE)(&(b->content)));
  ptlong _ret = {ret};
  
  ASSIGN25(Val,_ret);
}
 
 

void __FUNC__CONS()
{
  __obji* a = *(__obji**)(Argl--);
  __obji* b = *(__obji**)(Argl--);

  __obji* ret = newObj(sizeof(ptlong)*2);
  ret->_t = CPair;
  ret->_copied = 0;
  *(ptlong*)&(ret->_content).__c = a;
  *(((ptlong*)&(ret->_content))+1).__c = b;
  ptlong _ret = {ret};
  ASSIGN25(Val,_ret);
}

void __FUNC__CAR()
{
  __obji* a = *(__obji**)(Argl--);
  ASSERT_TYPE(a,CPair);
  ptlong _ret ={a->_content};
  ASSIGN25(Val,_ret);
}


void __FUNC__CAR()
{
  __obji* a = *(__obji**)(Argl--);
  ASSERT_TYPE(a,CPair);
  ptlong _ret =*(((ptlong*)&(a->_content))+1);
  ASSIGN25(Val,_ret);
}


#define DECLARE_FUNC(x) __DECLARE__FUNC(x)
#define __DECLARE__FUNC(FUNCNAME) \
  struct __STRUCT__##FUNCNAME()	  \
  {							\
    const static ptlong* func = __FUNC__FUNCNAME;	\
  }__INSTA__FUNCNAME;				



DECLARE_FUNC(ADD);
DECALRE_FUNC(MINUS);
DECLARE_FUNC(MUTIPLY);
DECLARE_FUNC(DIVIDE);
DECLARE_FUNC(CONS);
DECLARE_FUNC(CAR);
DECLARE_FUNC(CDR);
