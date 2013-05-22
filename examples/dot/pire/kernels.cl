int testBit_fun_int32_t( int x, int i ) {
  return (x & 1 << i) != 0;
}
__kernel void k6( __global int* mem5, __global int* arg1, __global int* arg2 ) {
  int tid = get_global_id(0);
  int localSize = get_local_size(0);
  int globalSize = get_global_size(0);
  if(tid < localSize) {
    for(int ix = 0; ix < globalSize/localSize; ix++) {
      mem5[(tid + (localSize * ix))] = (arg1[(tid + (localSize * ix))] * arg2[(tid + (localSize * ix))]);
    }
  }
}
__kernel void k11( __global int* mem5, __global int* mem7, int mem9, int mem10 ) {
  int tid = get_global_id(0);
  int localSize = get_local_size(0);
  int globalSize = get_global_size(0);
  if(tid < localSize) {
    for(int ix = 0; ix < globalSize/localSize; ix++) {
      int mem12;
      mem12 = mem5[(tid + (localSize * ix))];
      mem7[(tid + (localSize * ix))] = (((tid + (localSize * ix)) % mem9) == 0) ? (mem12 + mem5[((tid + (localSize * ix)) + mem10)]) : mem12;
    }
  }
}
