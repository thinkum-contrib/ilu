#ifndef _ILU_PYTHON_PACKAGE_H
#define _ILU_PYTHON_PACKAGE_H

typedef enum { idlModule, idlInterface } idltype;

typedef struct _namespace {

  idltype	type;
  char *	name;
  struct _namespace *parent;
  union {
    struct {
      char *	brand;
      char *	id;
      boolean	has_submodules;
    } module;
    struct {
      Type	ifc;
    } interface;
  } d;
  Interface	ifc;
  list	subspaces;
  list	types;
  list	exceptions;
  list	constants;

} *namespace;

extern namespace		idlsort (Interface);	/* returns list of namespace */
extern void			printSort (namespace);
extern Interface		namespace_interface(namespace);
extern list			namespace_imports(namespace);	/* returns list of Imported */
extern char *			getSimpleNamespaceName (namespace);

#endif /* def _ILU_PYTHON_PACKAGE_H */
