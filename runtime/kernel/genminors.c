#include <stdio.h>
#include <stdlib.h>

#define __ILU_PROCESSING_GENMINORS__
#include <iluxport.h>

int main (int argc, char**argv) {
  char *major;

#undef ILU_M1NOR
#undef ILU_MINOR
#undef ILU_MINOR_LAST
#define ILU_M1NOR(n,c)		ILU_MINOR(n,c)
#define ILU_MINOR(n,c)		ILU_GEN_MINOR(ilu_##n,c)
#define ILU_GEN_MINOR(nam,cmt)	printf("@item\n@ftindex %s (minor code of %s)\n @constant{%s}(%lu, 0x%lx) --- %s.\n",\
                                       #nam, major, #nam, nam, nam, cmt); \
                                fflush(stdout);
#define ILU_MINOR_LAST(n)	;

#define ILU_GEN_MAJOR(prev,cur,next)\
  printf("@node %s, %s, %s, %s\n"\
         "@section Minor Codes for System Exception %s\n"\
         "@ftindex %s\n@itemize @bullet\n\n",\
         #cur, #next, #prev,\
         "ILU-Specific CORBA System Exception Minor Codes",\
         #cur, #cur);\
  major = #cur;\
  ILU_##cur##_MINORS\
  printf("@end itemize\n\n");

  printf("@c $Id: genminors.c,v 1.5 1999/07/17 03:18:37 janssen Exp $\n");
  ILU_GEN_MAJOR(Intro,		BAD_PARAM,	IMP_LIMIT);
  ILU_GEN_MAJOR(BAD_PARAM,	IMP_LIMIT,	COMM_FAILURE);
  ILU_GEN_MAJOR(IMP_LIMIT,	COMM_FAILURE,	INV_OBJREF);
  ILU_GEN_MAJOR(COMM_FAILURE,	INV_OBJREF,	INTERNAL);
  ILU_GEN_MAJOR(INV_OBJREF,	INTERNAL,	MARSHAL);
  ILU_GEN_MAJOR(INTERNAL,	MARSHAL,	BAD_TYPECODE);
  ILU_GEN_MAJOR(MARSHAL,	BAD_TYPECODE,	BAD_OPERATION);
  ILU_GEN_MAJOR(BAD_TYPECODE,	BAD_OPERATION,	NO_RESOURCES);
  ILU_GEN_MAJOR(BAD_OPERATION,	NO_RESOURCES,	TRANSIENT);
  ILU_GEN_MAJOR(NO_RESOURCES,	TRANSIENT,	NO_MEMORY);
  return 0;
}
