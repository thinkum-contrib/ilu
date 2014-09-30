#include "strlen.h"
#include "Python.h"

extern void initiluPr(void);

static char *
  do_python_stuff (char *sid, char *moduleName, char *fnName)
{
  PyObject *mod, *dict, *fn, *psbh;
  char *sbh = NULL;
  char *javapython_verbose = getenv("JAVAPYTHON_VERBOSE");

  /* initialize Python interpreter */
  if (javapython_verbose) fprintf(stderr, "Initing Python\n");
  Py_Initialize();
  if (javapython_verbose) fprintf(stderr, "Initing ILU\n");
  initiluPr();
  if (javapython_verbose) fprintf(stderr, "adding . to PYTHONPATH\n");
  PyRun_SimpleString("import sys ; sys.path.append('.')");
  /* load the Python implementation */
  if (javapython_verbose) fprintf(stderr, "importing module %s\n", moduleName);
  mod = PyImport_ImportModule(moduleName);
  if (javapython_verbose) fprintf(stderr, "module is %p\n", mod);
  if (mod == NULL)
    return NULL;
  /* find the init function */
  if (javapython_verbose) fprintf(stderr, "getting dict\n");
  dict = PyModule_GetDict(mod);	/* docs say `this function never fails' */
  if (javapython_verbose) fprintf(stderr, "getting fn %s\n", fnName);
  fn = PyDict_GetItemString(dict, fnName);
  if (fn == NULL)
    return NULL;
  /* call the init function */
  if (javapython_verbose) fprintf(stderr, "calling fn %s\n", fnName);
  psbh = PyObject_CallFunction(fn, "s", sid);
  if (psbh == NULL || !PyString_Check(psbh))
    return NULL;
  sbh = PyString_AsString(psbh);
  Py_DECREF(psbh);
  if (javapython_verbose) fprintf(stderr, "returning string %s\n", sbh);
  return sbh;
}

#ifdef ILUJAVA_JNI

#include <string.h>	/* for strlen */
#include <memory.h>	/* for memcpy */
#include <malloc.h>	/* for malloc and free */

jstring JNICALL Java_strlen_initImpl
  (JNIEnv *env, jclass obj, jstring jsid)
{
  jstring jsbh;
  char *sid;
  const char *sbh;
  jsize len;

  if (jsid == 0)
    sid = NULL;
  else {
    sbh = (*env)->GetStringUTFChars(env, jsid, 0);
    len = strlen(sbh);
    sid = malloc(len + 1);
    memcpy(sid, sbh, len);
    sid[len] = 0;
    (*env)->ReleaseStringUTFChars(env, jsid, sbh);
  }

  sbh = do_python_stuff (sid, "testImpl", "init");

  /* return the value */
  if (sid != NULL)
    free(sid);
  jsbh = (*env)->NewStringUTF(env, sbh);
  return jsbh;
}

#else

extern struct Hjava_lang_String *
  strlen_initImpl (struct Hstrlen * obj,
		       struct Hjava_lang_String * jsid)
{
  struct Hjava_lang_String * jsbh;
  char * sid;
  char * sbh;

  if (jsid == 0)
    sid = NULL;
  else
    sid = allocCString(jsid);

  sbh = do_python_stuff(sid, "testImpl", "init");

  if (sid != NULL)
    free(sid);
  jsbh = makeJavaString (sbh, strlen(sbh));
  return jsbh;
}

#endif
