#include <stdio.h>

#include "test-cpptrue.hpp"

class test_Strlen_impl : public virtual test(Strlen) {

public:
  test_Strlen_impl (char *pc_instance_handle = NULL,
		    iluServer& r_an_ilu_server = iluServer::iluGetDefaultServer(),
		    CORBA(Boolean) b_within_object_table = ILUCPP_FALSE) :
	iluObject(iluGetILUClassRecord(), pc_instance_handle, r_an_ilu_server, b_within_object_table) {}

  virtual ~test_Strlen_impl() {
    iluDeactivate();
  }

  virtual CORBA(ULong) len (const iluShortCharacter * s);
};

CORBA(ULong)
     test_Strlen_impl::len (const iluShortCharacter * str)
{
  return (CORBA(ULong)) strlen(str);
}

extern "C" {

  ilu_string testImpl__initialize (ilu_string sid)
    {
      iluServer *s;
      test_Strlen_impl *i;

      ILU_INIT_test_SERVER_ONLY();
      iluCppRuntime::iluInitialize();

      s = new iluServer (sid);

      /* now create an instance of test.Strlen, and return its SBH */
      i = new test_Strlen_impl((iluCString) NULL, *s);
      return i->iluObjectToString();
    }
}
