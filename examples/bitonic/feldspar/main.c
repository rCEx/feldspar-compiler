#include <stdio.h>
#include <stdlib.h>
#include <CL/cl.h>
#include <math.h>
#include <time.h>
#include "feldspar_c99.h"
#include "bitonic.h"
#define MAX_SOURCE_SIZE (0x100000)


void outputMeasure(char *to, long time, int size) {
  FILE *fp = fopen(to, "a");
  if(fp != NULL) {
    fprintf(fp, "%li %i\n", time, size);
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

  struct timespec timer1;
  struct timespec timer2;
  clock_gettime(CLOCK_PROCESS_CPUTIME_ID, &timer1);
  f0(log2(size), a, &res);
  clock_gettime(CLOCK_PROCESS_CPUTIME_ID, &timer2);
  long nanos = timer2.tv_nsec - timer1.tv_nsec;

  outputMeasure("bitonicFeldspar.log",nanos, size);

  return 0;
}
