#include <stdio.h>
#include <stdlib.h>
#include <CL/cl.h>
#include <math.h>
#include <time.h>
#include "feldspar_c99.h"
#include "dotProd.h"
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
  struct array *b = NULL;
  b = initArray(b, sizeof(int), size); 

  int* buf1 = (int*) a->buffer;
  int* buf2 = (int*) b->buffer;
  for(int i = 0; i < size; i++) {
    buf1[i] = i % 4;
    buf2[i] = i % 4;
  }

  struct array *res = NULL;
  res = initArray(res, sizeof(int), size);
  clock_t t;
  t = clock();
  f0(a, b, &res);
  t = clock() - t;

  outputMeasure("dotFeldspar.log",t, size);
  return 0;
}

