#include "strlen.h"

extern char * testImpl__initialize(char *);

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
  sbh = testImpl__initialize(sid);
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
  sbh = testImpl__initialize(sid);
  if (sid != NULL)
    free(sid);
  jsbh = makeJavaString (sbh, strlen(sbh));
  return jsbh;
}

#endif
