
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <iluchdrs.h>

int main(int ac, char **av) {

  FILE *file;
  file = fopen("test1.configvals", "w");
  fputs("Relevant configuration values\n", file);
#if (defined(ILU_OS_THREADED))  
  fputs("ILU_OS_THREADED\n",file);
#endif
#if (defined(ILU_BINDING_HOST)) 
  fprintf(file, "ILU_BINDING_HOST %s\n", ILU_BINDING_HOST);
#endif
#if (defined(ILU_BINDING_REALM))
  fprintf(file, "ILU_BINDING_REALM %s\n", ILU_BINDING_REALM);
#endif
#if (defined(ILU_BINDING_PORT))
  fprintf(file, "ILU_BINDING_PORT %d\n", ILU_BINDING_PORT);
#endif

  fclose(file);

  return 0;
}
 
