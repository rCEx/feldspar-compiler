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
  uint32_t v11;
  uint32_t len0;
  uint32_t v12;
  uint32_t v13;
  uint32_t len1;
  struct array * v9 = NULL;
  
  v11 = min(getLength(v0), getLength(v1));
  len0 = ((32 - bitScan_fun_uint32_t(v11)) - 1);
  *out = initArray(*out, sizeof(uint64_t), v11);
  for (uint32_t v7 = 0; v7 < v11; v7 += 1)
  {
    at(uint64_t,*out,v7) = (at(uint64_t,v0,v7) * at(uint64_t,v1,v7));
  }
  for (uint32_t v8 = 0; v8 < len0; v8 += 1)
  {
    v12 = pow_fun_uint32_t(2, (v8 + 1));
    v13 = pow_fun_uint32_t(2, v8);
    len1 = getLength(*out);
    v9 = initArray(v9, sizeof(uint64_t), len1);
    for (uint32_t v10 = 0; v10 < len1; v10 += 1)
    {
      if (((v10 % v12) == 0))
      {
        at(uint64_t,v9,v10) = (at(uint64_t,*out,v10) + at(uint64_t,*out,(v10 + v13)));
      }
      else
      {
        at(uint64_t,v9,v10) = 0;
      }
    }
    *out = initArray(*out, sizeof(uint64_t), getLength(v9));
    copyArray(*out, v9);
  }
  freeArray(v9);
}
