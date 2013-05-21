#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#ifdef __APPLE__
#include <OpenCL/opencl.h>
#else
#include <CL/cl.h>
#endif


#include <math.h>
#include <time.h>
#include "feldspar_c99.h"
#define MAX_SOURCE_SIZE (0x100000)

#ifdef __APPLE__
#include <sys/time.h>
double getRealTime() {
  struct timeval tv;
  gettimeofday(&tv,0);
  return (double)tv.tv_sec+1.0e-6*(double)tv.tv_usec;
}
#else
double getRealTime() {
  struct timespec timer;
  clock_gettime(CLOCK_PROCESS_CPUTIME_ID, &timer);
  return (double)timer.tv_sec+1.0e-9*(double)timer.tv_nsec;
}
#endif

static cl_program program;

static cl_kernel k6, k11;

static cl_context context;
static cl_command_queue command_queue;




void init()
{
  FILE *fp = NULL;
  char* source_str;
  fp = fopen( "kernels.cl" , "r");
  source_str = (char*) malloc(MAX_SOURCE_SIZE);
  size_t source_size = fread( source_str, 1, MAX_SOURCE_SIZE, fp);
  fclose( fp );

  program = clCreateProgramWithSource(context, 1, (const char **)&source_str, (const size_t *)&source_size, NULL);
  clBuildProgram(program, 1, NULL /*&device_id*/, NULL, NULL, NULL);
  
  k6 = clCreateKernel(program, "k6", NULL);
  k11 = clCreateKernel(program, "k11", NULL);

  free(source_str);
}

void teardown()
{
  clReleaseKernel(k6);
  clReleaseKernel(k11);
}



void f0(int* arg1, int arg1c, int* arg2, int arg2c, int** out4) {
  int mem3;
  mem3 = min(arg1c,arg2c);
  int mem5c;
  mem5c = mem3;
  int* mem5 = (int*) malloc(sizeof(int) * mem5c);
  clBuildProgram(program, 1, NULL /*&device_id */, NULL, NULL, NULL);
  cl_mem mem5_obj = clCreateBuffer(context, CL_MEM_READ_WRITE, mem3*sizeof(int), NULL, NULL);
  clSetKernelArg(k6, 0, sizeof(cl_mem), (void *)&mem5_obj);
  clEnqueueWriteBuffer(command_queue, mem5_obj, CL_TRUE, 0, mem3*sizeof(int), mem5, 0, NULL, NULL);
  cl_mem arg1_obj = clCreateBuffer(context, CL_MEM_READ_WRITE, mem3*sizeof(int), NULL, NULL);
  clSetKernelArg(k6, 1, sizeof(cl_mem), (void *)&arg1_obj);
  clEnqueueWriteBuffer(command_queue, arg1_obj, CL_TRUE, 0, mem3*sizeof(int), arg1, 0, NULL, NULL);
  cl_mem arg2_obj = clCreateBuffer(context, CL_MEM_READ_WRITE, mem3*sizeof(int), NULL, NULL);
  clSetKernelArg(k6, 2, sizeof(cl_mem), (void *)&arg2_obj);
  clEnqueueWriteBuffer(command_queue, arg2_obj, CL_TRUE, 0, mem3*sizeof(int), arg2, 0, NULL, NULL);
  size_t global_item_size = mem3;
  size_t local_item_size = min(arg1c,1024);
  clEnqueueNDRangeKernel(command_queue, k6, 1, NULL, &global_item_size, &local_item_size, 0, NULL, NULL);
  clEnqueueReadBuffer(command_queue, mem5_obj, CL_TRUE, 0, mem3*sizeof(int), mem5, 0, NULL, NULL);


  int mem7c;
  for(int q = 0; q < ((32 - bitScan_fun_int32_t(mem3)-2) - 1); q++) {
    int mem9;
    mem9 = pow(2,(q + 1));
    int mem10;
    mem10 = pow(2,q);
    mem7c = mem5c;
    int* mem7 = (int*) malloc(sizeof(int) * mem7c);
    clBuildProgram(program, 1, NULL /* &device_id */, NULL, NULL, NULL);
    cl_mem mem7_obj = clCreateBuffer(context, CL_MEM_READ_WRITE, mem5c*sizeof(int), NULL, NULL);
    clSetKernelArg(k11, 0, sizeof(cl_mem), (void *)&mem7_obj);
    clEnqueueWriteBuffer(command_queue, mem7_obj, CL_TRUE, 0, mem5c*sizeof(int), mem7, 0, NULL, NULL);
    cl_mem mem9_obj = clCreateBuffer(context, CL_MEM_READ_WRITE, sizeof(int), NULL, NULL);
    clSetKernelArg(k11, 1, sizeof(cl_mem), (void *)&mem9_obj);
    clEnqueueWriteBuffer(command_queue, mem9_obj, CL_TRUE, 0, sizeof(int), &mem9, 0, NULL, NULL);
    mem5_obj = clCreateBuffer(context, CL_MEM_READ_WRITE, mem5c*sizeof(int), NULL, NULL);
    clSetKernelArg(k11, 2, sizeof(cl_mem), (void *)&mem5_obj);
    clEnqueueWriteBuffer(command_queue, mem5_obj, CL_TRUE, 0, mem5c*sizeof(int), mem5, 0, NULL, NULL);
    cl_mem mem10_obj = clCreateBuffer(context, CL_MEM_READ_WRITE, sizeof(int), NULL, NULL);
    clSetKernelArg(k11, 3, sizeof(cl_mem), (void *)&mem10_obj);
    clEnqueueWriteBuffer(command_queue, mem10_obj, CL_TRUE, 0, sizeof(int), &mem10, 0, NULL, NULL);
    global_item_size = mem5c;
    local_item_size = min(arg1c,1024);
    clEnqueueNDRangeKernel(command_queue, k11, 1, NULL, &global_item_size, &local_item_size, 0, NULL, NULL);
    clEnqueueReadBuffer(command_queue, mem7_obj, CL_TRUE, 0, mem5c*sizeof(int), mem7, 0, NULL, NULL);


    //mem5 = mem7;
    memcpy(mem5,mem7,mem5c);
    free(mem7);
  }
  (*out4) = mem5;
}



void outputMeasure(char *to, double time, int size) {
  FILE *fp = fopen(to, "a");
  if(fp != NULL) {
    fprintf(fp, "%lf %i\n", time, size);
  }
  fclose(fp);
}



int main (int argc, char *argv[]) {
  const int size = atoi(argv[1]);
  int *a = (int*) malloc(sizeof(int)*size);
  int *b = (int*) malloc(sizeof(int)*size);
  int i;
  for(i = 0; i < size; i++) {
    a[i] = i%4;
    b[i] = i%4;
  }
  int *res = 0;

  /// Setup
  cl_platform_id platform_id = NULL;
  cl_device_id device_id = NULL;
  cl_uint ret_num_devices;
  cl_uint ret_num_platforms;
  clGetPlatformIDs(1, &platform_id, &ret_num_platforms);
  clGetDeviceIDs(platform_id, CL_DEVICE_TYPE_DEFAULT, 1, &device_id, &ret_num_devices);
  context = clCreateContext(NULL, 1, &device_id, NULL, NULL, NULL);
  command_queue = clCreateCommandQueue(context, device_id, 0, NULL);

  init();


  /// Test
  double t1,t2;
  int const iter = 10;
  printf("Running test of size %d (%d iterations)\n", size, iter);
  for (int i=-2; i<iter; i++) // Negative i is warmup
  {
    if (i == 0)
      t1 = getRealTime();

      f0(a,size,b,size, &res);
      printf("%i\n",res[0]);
      free(res);
  }
  t2 = getRealTime();
  double nanos = (t2 - t1) * 1.0e9 / iter;

  outputMeasure("dotPIRE.log",nanos, size);

  /// Teardown
  teardown();

  free(a);
  free(b);
  free(res);
  return 0;
}
