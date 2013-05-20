#include "dotProd.h"
#include "feldspar_c99.h"
#include "feldspar_array.h"
#include "feldspar_future.h"
#include "ivar.h"
#include "taskpool.h"
#include <stdint.h>
#include <string.h>
#include <math.h>
#include <stdbool.h>
#include <complex.h>


void f0(struct array * v0, struct array * v1, struct array * * out)
{
  int v11;
  int len0;
  int v12;
  int v13;
  int len1;
  struct array * v9 = NULL;
  
  v11 = min(getLength(v0), getLength(v1));
  len0 = ((32 - bitScan_fun_int32_t(v11)) - 2);
  *out = initArray(*out, sizeof(int), v11);
  for (int v7 = 0; v7 < v11; v7 += 1)
  {
    at(int,*out,v7) = (at(int,v0,v7) * at(int,v1,v7));
  }
  for (int v8 = 0; v8 < len0; v8 += 1)
  {
    v12 = pow_fun_int32_t(2, (v8 + 1));
    v13 = pow_fun_int32_t(2, v8);
    len1 = getLength(*out);
    v9 = initArray(v9, sizeof(int), len1);
    for (int v10 = 0; v10 < len1; v10 += 1)
    {
      if (((v10 % v12) == 0))
      {
        at(int,v9,v10) = (at(int,*out,v10) + at(int,*out,(v10 + v13)));
      }
      else
      {
        at(int,v9,v10) = 0;
      }
    }
    *out = initArray(*out, sizeof(int), getLength(v9));
    copyArray(*out, v9);
  }
  freeArray(v9);
}
