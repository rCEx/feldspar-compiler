int testBit_fun_int32_t( int x, int i )
{
    return (x & 1 << i) != 0;
}


__kernel void k10( __global int* mem4, __global int* mem7, __global int* mem9, __global int* o ) {
  int tid = get_global_id(0);
  int localSize = get_local_size(0);
  int globalSize = get_global_size(0);
  if(tid < localSize) {
    for(int ix = 0; ix < globalSize/localSize; ix++) {
      int mem11;
      mem11 = mem4[(tid + (localSize * ix))];
      int mem12;
      mem12 = mem4[((tid + (localSize * ix)) ^ (*mem7))];
      mem9[(tid + (localSize * ix))] = testBit_fun_int32_t((tid + (localSize * ix)),(*o)) ? max(mem11,mem12) : min(mem11,mem12);
    }
  }
}
__kernel void k17( __global int* mem5, __global int* mem16, __global int* mem13, __global int* mem15 ) {
  int tid = get_global_id(0);
  int localSize = get_local_size(0);
  int globalSize = get_global_size(0);
  if(tid < localSize) {
    for(int ix = 0; ix < globalSize/localSize; ix++) {
      int mem18;
      mem18 = mem5[(tid + (localSize * ix))];
      int mem19;
      mem19 = mem5[((tid + (localSize * ix)) ^ (*mem16))];
      mem13[(tid + (localSize * ix))] = testBit_fun_int32_t((tid + (localSize * ix)),(*mem15)) ? max(mem18,mem19) : min(mem18,mem19);
    }
  }
}
