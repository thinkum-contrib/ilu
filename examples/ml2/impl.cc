#include <stdio.h>

#include "test.hh"

class test_Strlen_impl : public virtual test_T_Strlen {

public:
  test_Strlen_impl(char *instanceHandle, iluServer *server);

  virtual char * ILUGetInstanceHandle();
  virtual iluServer * ILUGetServer();

  virtual ilu_Cardinal len (testStatus *status, ilu_CString s);

private:
  char *ourInstanceHandle;
  iluServer *ourServer;
};

test_Strlen_impl::test_Strlen_impl(char *instanceHandle, iluServer *server)
{
  this->ourInstanceHandle = instanceHandle;
  this->ourServer = server;
}

char * test_Strlen_impl::ILUGetInstanceHandle()
{
  return this->ourInstanceHandle;
}

iluServer * test_Strlen_impl::ILUGetServer()
{
  return this->ourServer;
}

ilu_Cardinal test_Strlen_impl::len (testStatus *_status, ilu_CString str)
{
  _status->returnCode = testReply_Success;
  return strlen(str);
}

extern void test__InitializeServer(void);

extern "C" {

  ilu_string testImpl__initialize (ilu_string sid)
    {
      iluServer *s;
      test_Strlen_impl *i;

      ilu::InitializeRuntime();

      s = new iluServer (sid, NULL);
      s->AddPort(NULL, NULL, ilu_TRUE);

      /* we'll explicitly initialize the test namespace */
      test__Initialize();
      test__InitializeServer();

      /* now create an instance of test.Strlen, and return its SBH */
      i = new test_Strlen_impl(NULL, s);
      return i->ILUStringBindingHandle();
      return NULL;
    }
}
