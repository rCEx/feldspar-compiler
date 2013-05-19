#include "scan.h"
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


void f0(struct array * v0, struct array * * out)
{
  uint32_t len0;
  uint32_t len1;
  uint32_t v10;
  uint32_t v11;
  struct array * v8 = NULL;
  
  len0 = (32 - bitScan_fun_uint32_t(getLength(v0)));
  *out = initArray(*out, sizeof(uint32_t), getLength(v0));
  copyArray(*out, v0);
  for (uint32_t v7 = 0; v7 < len0; v7 += 1)
  {
    len1 = getLength(*out);
    v8 = initArray(v8, sizeof(uint32_t), len1);
    for (uint32_t v9 = 0; v9 < len1; v9 += 1)
    {
      v10 = (v9 >> v7);
      v11 = at(uint32_t,*out,v9);
      if (testBit_fun_uint32_t(v10, 0))
      {
        at(uint32_t,v8,v9) = (at(uint32_t,*out,((v10 << v7) - 1)) + v11);
      }
      else
      {
        at(uint32_t,v8,v9) = v11;
      }
    }
    *out = initArray(*out, sizeof(uint32_t), getLength(v8));
    copyArray(*out, v8);
  }
  freeArray(v8);
}
