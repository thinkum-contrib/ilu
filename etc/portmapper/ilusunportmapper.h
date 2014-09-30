#include <iluxport.h>

ILU_RUNTIME_PUBLIC ilu_boolean
  ilu_portmapper_register (ILU_C_Object *, ILU_C_ENVIRONMENT *);

ILU_RUNTIME_PUBLIC ILU_C_Object *
  ilu_portmapper_bind (char *,		/* hostname, RETAIN */
		       ilu_Class,	/* object's type */
		       ILU_C_ENVIRONMENT *);
