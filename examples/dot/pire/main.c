#include <stdio.h>
#include <stdlib.h>
#ifdef __APPLE__
#include <OpenCL/opencl.h>
#else
#include <CL/cl.h>
#endif
#include <math.h>
#include <time.h>
#include <string.h>
#include "feldspar_c99.h"

#include "fun.c"


void outputMeasure(char *to, double time, int size) {
  FILE *fp = fopen(to, "a");
  if(fp != NULL) {
    fprintf(fp, "%lf %i\n", time, size);
  }
  fclose(fp);
}


int main (int argc, char *argv[]) {
  const int arrSize = atoi(argv[1]);

  int *a = (int*) malloc(sizeof(int)*arrSize);
  int *b = (int*) malloc(sizeof(int)*arrSize);
  int i;
  for(i = 0; i < arrSize; i++) {
    a[i] = i%4;
    b[i] = i%4;
  }
  int * volatile res = malloc(arrSize*sizeof(int));

  init();


  /// Test
  double t1,t2;
  int const iter = 10;
  printf("Running test of size %d (%d iterations)\n", arrSize, iter);
  printf("Before: ");
  for (int i=9; i<min(arrSize,10); i++)
  {
    printf("%d ", a[i]);
    printf("%d ", b[i]);
  }
  printf("\n");

  for (int i=-2; i<iter; i++) // Negative i is warmup
  {
    if (i == 0)
      t1 = getRealTime();
    f0(a, arrSize, b, arrSize, &res);
  }
  t2 = getRealTime();
  double nanos = (t2 - t1) * 1.0e9 / iter;

  outputMeasure("dotPIRE.log",nanos, arrSize);

  printf("After:  ");
  for (int i=0; i<min(arrSize,10); i++)
  {
    printf("%d ", res[i]);
  }
  printf("\nhead:%d\n",res[0]);

  /// Teardown
//  teardown();

  free(a);
  free(b);
  free(res);
  return 0;
}



