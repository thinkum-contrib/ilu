#include <stdio.h>
#include <stdlib.h>

#include "Svc1.h"

int main(int argc, char**argv)
{
  int i, n=100;
  char *id;
  Svc1_O theO;
  CORBA_Environment lenv;
  if (argc<2 || argc>3)
    goto usage;
  id = argv[1];
  if (argc>2)
    n = atoi(argv[2]);
  Svc1__Initialize();
  theO = (Svc1_O) ILU_C_LookupObject("indir-test-svc1", "it", Svc1_O__MSType);
  if (!theO) {
    fprintf(stderr, "Unable to lookup object!\n");
    exit(1);
  }
  printf("Calling %s %d times.\n",
	 ILU_C_SBHOfObject(theO), n);
  for (i=0; i<n; i++) {
    char buf[100];
    sprintf(buf, "%s[%d]", id, i);
    Svc1_O_m1(theO, buf, &lenv);
    if (!ILU_C_SUCCESSFUL(&lenv)) {
      fprintf(stderr, "m1(%s) failed!\n", buf);
      exit(1);
    }
  }
  return 0;
 usage:
  fprintf(stderr, "Usage: %s <id:string> [<iterations:int>]\n", argv[0]);
  exit(1);
}
