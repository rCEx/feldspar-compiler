#include <stdio.h>
#include <stdlib.h>
#include <CL/cl.h>
#include <math.h>
#include <time.h>
#include "feldspar_c99.h"
#include "bitonic.h"
#define MAX_SOURCE_SIZE (0x100000)

int main() {
  
 int base = 2;
 int size = 4;
  
 struct array *a = NULL;
 a = initArray(a, sizeof(int), size); 

  int* buf = (int*) a->buffer;
  for(int i = 0; i < size; i++) {
    buf[i] = size-i;
  }
  //a[0] = 13;
  //a[1] = 2;
  //a[2] = 1;
  //a[3] = 1;
  //a[4] = 1;
  //a[5] = 1;
  //a[6] = 3;
  //a[7] = 3;
  //a[8] = 0;
  //a[9] = 2;
  //a[10] = 2;
  //a[11] = 1;
  //a[12] = 11;
  //a[13] = 6;
  //a[14] = 7;
  //a[15] = 8;
  struct array *res = NULL;
  res = initArray(res, sizeof(int), size);
  f0(base, a, &res);
  
  buf = (int*) res->buffer;
  for(int i = 0; i < size; i++) {
    printf("%i\n", buf[i]);
  }
  


  return 0;
}
