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

#include <assert.h>

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

//////////////////////////
// Initialization
//

static cl_platform_id platform_id = NULL;
static cl_device_id device_id = NULL;
static cl_program program;

static cl_kernel k10, k17;

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
  clBuildProgram(program, 1, &device_id, NULL, NULL, NULL);
  
  k10 = clCreateKernel(program, "k10", NULL);
  k17 = clCreateKernel(program, "k17", NULL);

  free(source_str);
}

void teardown()
{
  clReleaseKernel(k10);
  clReleaseKernel(k17);
}

void f0(int arg1, int* arg2, int arg2c, int** out3) {
  int sz = arg2c*sizeof(int);

  // Allocate memory
  cl_mem mem4_obj  = clCreateBuffer(context, CL_MEM_READ_WRITE, sz, NULL, NULL);
  cl_mem mem5_obj  = clCreateBuffer(context, CL_MEM_READ_WRITE, sz, NULL, NULL);

  // Initialization
  clEnqueueWriteBuffer(command_queue, mem4_obj, CL_TRUE, 0, sz, arg2, 0, NULL, NULL);

  // Create schedule by enqueueing all steps
  for(int o = 0; o < arg1; o++) {
    int mem7;
    mem7 = (~(-1 << (o + 1)));
    int mem8;
    mem8 = (o + 1);
    size_t global_item_size = arg2c;
    size_t local_item_size = min(arg2c,1024);
    cl_int err = CL_SUCCESS;
    err |= clSetKernelArg(k10, 0, sizeof(cl_mem), &mem4_obj);
    err |= clSetKernelArg(k10, 1, sizeof(int), &mem7);
    err |= clSetKernelArg(k10, 2, sizeof(cl_mem), &mem5_obj);
    err |= clSetKernelArg(k10, 3, sizeof(int), &o);
    if (err != CL_SUCCESS)
      printf("error k10: %d\n", err);
    clEnqueueNDRangeKernel(command_queue, k10, 1, NULL, &global_item_size, &local_item_size, 0, NULL, NULL);

    for(int w = 0; w < o; w++) {
      int mem15;
      mem15 = (mem8 - (w + 2));
      int mem16;
      mem16 = (1 << mem15);
      global_item_size = arg2c;
      local_item_size = min(arg2c,1024);
      cl_int err = CL_SUCCESS;
      err |= clSetKernelArg(k17, 0, sizeof(cl_mem), &mem5_obj);
      err |= clSetKernelArg(k17, 1, sizeof(int), &mem16);
      err |= clSetKernelArg(k17, 2, sizeof(cl_mem), &mem4_obj);
      err |= clSetKernelArg(k17, 3, sizeof(int), &mem15);
      if (err != CL_SUCCESS)
        printf("error k17: %d\n", err);
      clEnqueueNDRangeKernel(command_queue, k17, 1, NULL, &global_item_size, &local_item_size, 0, NULL, NULL);
      clEnqueueCopyBuffer(command_queue, mem4_obj, mem5_obj, 0, 0, sz, 0, NULL, NULL);
    }
    clEnqueueCopyBuffer(command_queue, mem5_obj, mem4_obj, 0, 0, sz, 0, NULL, NULL);
  }
  *out3 = realloc(*out3, sz);
  clEnqueueReadBuffer(command_queue, mem4_obj, CL_TRUE, 0, sz, *out3, 0, NULL, NULL);
  clFinish(command_queue);

  clReleaseMemObject(mem4_obj);
  clReleaseMemObject(mem5_obj);
}



void outputMeasure(char *to, double time, int size) {
  FILE *fp = fopen(to, "a");
  if(fp != NULL) {
    fprintf(fp, "%lf %i\n", time, size);
  }
  fclose(fp);
}




int main (int argc, char *argv[]) {
  const int arrSize = atoi(argv[1]);
  const int size = (int) log2(arrSize); //pow(2,size);

  int *a = (int*) malloc(sizeof(int)*arrSize);
  int i;
  for(i = 0; i < arrSize; i++) {
    a[i] = i%4;
  }
  int * volatile res = NULL;

  /// Setup
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
  printf("Running test of size %d (%d iterations)\n", arrSize, iter);
  printf("Before: ");
  for (int i=0; i<min(arrSize,10); i++)
  {
    printf("%d ", a[i]);
  }
  printf("\n");

  for (int i=-2; i<iter; i++) // Negative i is warmup
  {
    if (i == 0)
      t1 = getRealTime();
    f0(size, a, arrSize, &res);
  }
  t2 = getRealTime();
  double nanos = (t2 - t1) * 1.0e9 / iter;

  outputMeasure("bitonicPIRE.log",nanos, arrSize);

  printf("After:  ");
  for (int i=0; i<min(arrSize,10); i++)
  {
    printf("%d ", res[i]);
  }
  printf("\n");

  /// Teardown
  teardown();

  free(a);
  free(res);
  return 0;
}


