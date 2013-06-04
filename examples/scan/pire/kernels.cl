int testBit_fun_int32_t( int x, int i ) {
  return (x & 1 << i) != 0;
}
__kernel void k6( int n, __global int* mem4 ) {
  int tid = get_global_id(0);
  int mem7;
  mem7 = (tid >> n);
  int mem8;
  mem8 = mem4[tid];
  mem4[tid] = testBit_fun_int32_t(mem7,0) ? (mem4[((mem7 << n) - 1)] + mem8) : mem8;
}
