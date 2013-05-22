#include "bitonic.h"
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


void f0(uint32_t v0, struct array * v1, struct array * * out)
{
  uint32_t v55;
  uint32_t v56;
  struct array * v45 = NULL;
  uint32_t len0;
  uint32_t v57;
  uint32_t v58;
  uint32_t v59;
  uint32_t v60;
  uint32_t len1;
  uint32_t v61;
  uint32_t v62;
  struct array * v53 = NULL;
  struct array * v31 = NULL;
  
  *out = initArray(*out, sizeof(uint32_t), getLength(v1));
  copyArray(*out, v1);
  for (uint32_t v30 = 0; v30 < v0; v30 += 1)
  {
    v55 = ~((4294967295 << (v30 + 1)));
    v56 = (v30 + 1);
    len0 = getLength(*out);
    v45 = initArray(v45, sizeof(uint32_t), len0);
    for (uint32_t v44 = 0; v44 < len0; v44 += 1)
    {
      v57 = at(uint32_t,*out,v44);
      v58 = at(uint32_t,*out,(v44 ^ v55));
      if (testBit_fun_uint32_t(v44, v30))
      {
        at(uint32_t,v45,v44) = max(v57, v58);
      }
      else
      {
        at(uint32_t,v45,v44) = min(v57, v58);
      }
    }
    v31 = initArray(v31, sizeof(uint32_t), getLength(v45));
    copyArray(v31, v45);
    for (uint32_t v52 = 0; v52 < v30; v52 += 1)
    {
      v59 = (v56 - (v52 + 2));
      v60 = (1 << v59);
      len1 = getLength(v31);
      v53 = initArray(v53, sizeof(uint32_t), len1);
      for (uint32_t v54 = 0; v54 < len1; v54 += 1)
      {
        v61 = at(uint32_t,v31,v54);
        v62 = at(uint32_t,v31,(v54 ^ v60));
        if (testBit_fun_uint32_t(v54, v59))
        {
          at(uint32_t,v53,v54) = max(v61, v62);
        }
        else
        {
          at(uint32_t,v53,v54) = min(v61, v62);
        }
      }
      v31 = initArray(v31, sizeof(uint32_t), getLength(v53));
      copyArray(v31, v53);
    }
    *out = initArray(*out, sizeof(uint32_t), getLength(v31));
    copyArray(*out, v31);
  }
  freeArray(v45);
  freeArray(v53);
  freeArray(v31);
}
