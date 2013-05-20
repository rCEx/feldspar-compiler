#include <stdio.h>
#include <stdlib.h>
#include <CL/cl.h>
#include <math.h>
#include <time.h>
#include "feldspar_c99.h"
#include "dotProd.h"

//#include <sys/time.h>
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
  struct timespec timer1;
  struct timespec timer2;
  clock_gettime(CLOCK_PROCESS_CPUTIME_ID, &timer1);
 // clock_t t;
 // t = clock();
  f0(a, b, &res);
  //t = clock() - t;
  clock_gettime(CLOCK_PROCESS_CPUTIME_ID, &timer2);
  long nanos = timer2.tv_nsec - timer1.tv_nsec;

  outputMeasure("dotFeldspar.log",nanos, size);
  printf("%i\n", at(int, res,0));

  return 0;
}

