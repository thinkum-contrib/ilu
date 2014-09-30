#include "Python.h"

extern char * testImpl__initialize (char *);

static PyMethodDef strlenGlue_methods[] = {
	{NULL,	      	NULL}
};

void
  initstrlenGlue()
{
  PyObject *m, *d, *de;
  char * sbh;

  m = Py_InitModule("strlenGlue", strlenGlue_methods);
  d = PyModule_GetDict(m);
  sbh = testImpl__initialize(NULL);
  if ((sbh == NULL) ||
      ((de = PyString_FromString(sbh)) == NULL) ||
      PyDict_SetItemString(d,"strlen_sbh",de))
    Py_FatalError("can't initialize strlenGlue module");
}
