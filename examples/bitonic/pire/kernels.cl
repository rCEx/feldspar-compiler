int testBit_fun_int32_t( int x, int i ) {
  return (x & 1 << i) != 0;
}
__kernel void k10( __global int* mem5, int mem7, __global int* mem9, int o ) {
  int tid = get_global_id(0);
  int mem11;
  mem11 = mem5[tid];
  int mem12;
  mem12 = mem5[(tid ^ mem7)];
  mem9[tid] = testBit_fun_int32_t(tid,o) ? max(mem11,mem12) : min(mem11,mem12);
}
__kernel void k16( __global int* mem5, int mem15, int mem14 ) {
  int tid = get_global_id(0);
  int mem17;
  mem17 = mem5[tid];
  int mem18;
  mem18 = mem5[(tid ^ mem15)];
  mem5[tid] = testBit_fun_int32_t(tid,mem14) ? max(mem17,mem18) : min(mem17,mem18);
}
