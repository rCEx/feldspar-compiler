int testBit_fun_int32_t( int x, int i ) {
  return (x & 1 << i) != 0;
}
__kernel void k8( __global int* mem7, __global int* mem2, __global int* mem4 ) {
  int tid = get_global_id(0);
  mem7[tid] = (mem2[tid] * mem4[tid]);
}
__kernel void k12( __global int* mem7, int mem10, int mem11 ) {
  int tid = get_global_id(0);
  int mem13;
  mem13 = mem7[tid];
  mem7[tid] = ((tid % mem10) == 0) ? (mem13 + mem7[(tid + mem11)]) : mem13;
}
