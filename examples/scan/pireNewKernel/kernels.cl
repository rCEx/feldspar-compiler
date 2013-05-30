int testBit_fun_int32_t( int x, int i ) {
  return (x & 1 << i) != 0;
}
__kernel void k6( int n, __global int* mem3, __global int* mem4 ) {
  int tid = get_global_id(0);
  int localSize = get_local_size(0);
  int globalSize = get_global_size(0);
      int mem7;
      mem7 = tid >> n;
      int mem8;
      mem8 = mem3[tid];
      mem4[(tid)] = testBit_fun_int32_t(mem7,0) ? (mem3[((mem7 << n) - 1)] + mem8) : mem8;
}
