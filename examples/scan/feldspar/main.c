#include <stdio.h>
#include <stdlib.h>
#include <CL/cl.h>
#include <math.h>
#include <time.h>
#include "feldspar_c99.h"
#include "scan.h"
#define MAX_SOURCE_SIZE (0x100000)


void outputMeasure(char *to, clock_t time, int size) {
  FILE *fp = fopen(to, "a");
  if(fp != NULL) {
    fprintf(fp, "%li %f %i\n", time, ((double)time)/CLOCKS_PER_SEC, size);
  }
  fclose(fp);
}

int main (int argc, char *argv[]) {
  const int size = atoi(argv[1]);

  struct array *a = NULL;
  a = initArray(a, sizeof(int), size); 

  int* buf = (int*) a->buffer;
  for(int i = 0; i < size; i++) {
    buf[i] = size-i;
  }


  struct array *res = NULL;
  res = initArray(res, sizeof(int), size);
  clock_t t;
  t = clock();
  f0(base, a, &res);
  t = clock() - t;

  outputMeasure("scanFeldspar.log",t, size);
  return 0;
}

