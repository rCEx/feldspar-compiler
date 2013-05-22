#include <stdio.h>
#include <stdlib.h>
#ifdef __APPLE__
#include <OpenCL/opencl.h>
#else
#include <CL/cl.h>
#endif
#include <math.h>
#include <time.h>
#include "feldspar_c99.h"
#include "feldspar_array.h"
#include "bitonic.h"
#define MAX_SOURCE_SIZE (0x100000)

#ifdef __APPLE__
#include <sys/time.h>
double getRealTime() {
  struct timeval tv;
  gettimeofday(&tv,0);
  return (double)tv.tv_sec+1.0e-6*(double)tv.tv_usec;
}
#else
double getRealTime() {
  struct timespec timer;
  clock_gettime(CLOCK_PROCESS_CPUTIME_ID, &timer);
  return (double)timer.tv_sec+1.0e-9*(double)timer.tv_nsec;
}
#endif

void outputMeasure(char *to, double time, int size) {
  FILE *fp = fopen(to, "a");
  if(fp != NULL) {
    fprintf(fp, "%lf %i\n", time, size);
  }
  fclose(fp);
}



int main (int argc, char *argv[]) {
  const int arrSize = atoi(argv[1]);
  const int size = (int) log2(arrSize); 
  
  struct array *a = NULL;
  a = initArray(a, sizeof(uint32_t), arrSize); 

  for(int i = 0; i < arrSize; i++) {
    at(uint32_t,a,i) = i%4;
  }

  struct array *res = NULL;
  res = initArray(res, sizeof(uint32_t), arrSize);

  double t1,t2;
  int const iter = 10;
  printf("Running test of size %d (%d iterations)\n", arrSize, iter);
  printf("Before: ");
  for (int i=0; i<min(arrSize,10); i++)
  {
    printf("%d ", at(uint32_t,a,i));
  }
  printf("\n");

  for (int i=-2; i<iter; i++) // Negative i is warmup
  {
    if (i == 0)
      t1 = getRealTime();
    f0(size, a, &res);
  }
  t2 = getRealTime();
  double nanos = (t2 - t1) * 1.0e9 / iter;

  outputMeasure("bitonicFeldspar.log",nanos, arrSize);

  printf("After:  ");
  for (int i=0; i<min(arrSize,10); i++)
  {
    printf("%d ", at(uint32_t,res,i));
  }
  printf("\n");

  freeArray(a);
  freeArray(res);

  return 0;
}
