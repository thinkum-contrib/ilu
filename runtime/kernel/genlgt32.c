#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "iluxport.h"

int main() {
  long unsigned u, up = 1;
  float v, l = floor(pow(2.0, 32.0));
  unsigned i, j=1;
  FILE *hfile, *tfile;
  hfile = fopen("ilulgt32.h", "w");
  if (!hfile) {
    fprintf(stderr, "Unable to open output file ilulgt32.h!\n");
    exit(1);
  }
  tfile = fopen("no_memory_minors.txt", "w");
  if (!tfile) {
    fprintf(stderr, "Unable to open output file no_memory_minors.txt!\n");
    exit(1);
  }
  fprintf(hfile, "/* %s%s */\n", "$I", "d: $");
  fprintf(hfile, " 0,\n 1,\n");
  fprintf(tfile, "%s%s\n", "$I", "d: $");
  fprintf(tfile, "    minor code       --- size range\n");
  fprintf(tfile, "decimal(hexadecimal) ---  decimal\n");
  fprintf(tfile, "-----------------------------------\n");
  fprintf(tfile, "%010lu(%08lx) --- 0.\n",
	  ILU_VMCID_BASE+0, ILU_VMCID_BASE+0);
  for (i=32; ; i++) {
    v = floor(pow(2.0, i/32.0));
    if (v>l)
      break;
    u = v;
    if (u > up) {
      ++j;
      fprintf(hfile, " %uU,\n", u);
      fprintf(tfile, "%010lu(%08lx) --- [%lu .. %lu].\n",
	      j+ILU_VMCID_BASE-1, j+ILU_VMCID_BASE-1, up, u-1);
      up = u;
    }
  }
  fprintf(tfile, "%010lu(%08lx) --- [%lu .. %lu].\n",
	  j+ILU_VMCID_BASE, j+ILU_VMCID_BASE, u, 0xFFFFFFFF );
  if (fclose(hfile) != 0) {
    fprintf(stderr, "Failure closing output file ilulgt32.h!\n");
    exit (1);
  }
  if (fclose(tfile) != 0) {
    fprintf(stderr, "Failure closing output file no_memory_minors.txt!\n");
    exit (1);
  }
  return 0;
}
