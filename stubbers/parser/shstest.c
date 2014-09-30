#include <stdio.h>
#include <string.h>

#include "shs.h"

int main (int argc, char**argv) {
  SHS_CTX ctx;
  int i;
  unsigned char hash[20];
  
  SHSInit(&ctx);
  for (i=1; i<argc; i++) {
    unsigned char *arg = (unsigned char*) argv[i];
    int len = strlen(argv[i]);
    SHSUpdate(&ctx, arg, len);
  }
  SHSFinal(hash, &ctx);
  for (i=0; i<20; i++)
    printf("%02x", hash[i]);
  printf("\n");
  return 0;
}
