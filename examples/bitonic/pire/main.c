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

//////////////////////////
// Initialization
//

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
  clBuildProgram(program, 1, NULL /*&device_id*/, NULL, NULL, NULL);
  
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
  int mem4c;
  mem4c = arg2c;
  int* mem4 = (int*) malloc(sizeof(int) * mem4c);
  memcpy(mem4,arg2,sizeof(int)*mem4c);
  int mem5c;
  for(int o = 0; o < arg1; o++) {
    int mem7;
    mem7 = (~(-1 << (o + 1)));
    int mem8;
    mem8 = (o + 1);
    int mem9c;
    mem9c = mem4c;
    int* mem9 = (int*) malloc(sizeof(int) * mem9c);
    cl_mem mem4_obj = clCreateBuffer(context, CL_MEM_READ_WRITE, mem4c*sizeof(int), NULL, NULL);
    clSetKernelArg(k10, 0, sizeof(cl_mem), (void *)&mem4_obj);
    clEnqueueWriteBuffer(command_queue, mem4_obj, CL_TRUE, 0, mem4c*sizeof(int), mem4, 0, NULL, NULL);
    cl_mem mem7_obj = clCreateBuffer(context, CL_MEM_READ_WRITE, sizeof(int), NULL, NULL);
    clSetKernelArg(k10, 1, sizeof(cl_mem), (void *)&mem7_obj);
    clEnqueueWriteBuffer(command_queue, mem7_obj, CL_TRUE, 0, sizeof(int), &mem7, 0, NULL, NULL);
    cl_mem mem9_obj = clCreateBuffer(context, CL_MEM_READ_WRITE, mem4c*sizeof(int), NULL, NULL);
    clSetKernelArg(k10, 2, sizeof(cl_mem), (void *)&mem9_obj);
    clEnqueueWriteBuffer(command_queue, mem9_obj, CL_TRUE, 0, mem4c*sizeof(int), mem9, 0, NULL, NULL);
    cl_mem o_obj = clCreateBuffer(context, CL_MEM_READ_WRITE, sizeof(int), NULL, NULL);
    clSetKernelArg(k10, 3, sizeof(cl_mem), (void *)&o_obj);
    clEnqueueWriteBuffer(command_queue, o_obj, CL_TRUE, 0, sizeof(int), &o, 0, NULL, NULL);
    size_t global_item_size = mem4c;
    size_t local_item_size = min(arg2c,1024);
    clEnqueueNDRangeKernel(command_queue, k10, 1, NULL, &global_item_size, &local_item_size, 0, NULL, NULL);
    clEnqueueReadBuffer(command_queue, mem9_obj, CL_TRUE, 0, mem4c*sizeof(int), mem9, 0, NULL, NULL);


    mem5c = mem9c;
    int* mem5 = (int*) malloc(sizeof(int) * mem5c);
    memcpy(mem5,mem9,sizeof(int)*mem5c);
    free(mem9);
    int mem13c;
    for(int w = 0; w < o; w++) {
      int mem15;
      mem15 = (mem8 - (w + 2));
      int mem16;
      mem16 = (1 << mem15);
      mem13c = mem5c;
      int* mem13 = (int*) malloc(sizeof(int) * mem13c);
      cl_mem mem5_obj = clCreateBuffer(context, CL_MEM_READ_WRITE, mem5c*sizeof(int), NULL, NULL);
      clSetKernelArg(k17, 0, sizeof(cl_mem), (void *)&mem5_obj);
      clEnqueueWriteBuffer(command_queue, mem5_obj, CL_TRUE, 0, mem5c*sizeof(int), mem5, 0, NULL, NULL);
      cl_mem mem16_obj = clCreateBuffer(context, CL_MEM_READ_WRITE, sizeof(int), NULL, NULL);
      clSetKernelArg(k17, 1, sizeof(cl_mem), (void *)&mem16_obj);
      clEnqueueWriteBuffer(command_queue, mem16_obj, CL_TRUE, 0, sizeof(int), &mem16, 0, NULL, NULL);
      cl_mem mem13_obj = clCreateBuffer(context, CL_MEM_READ_WRITE, mem5c*sizeof(int), NULL, NULL);
      clSetKernelArg(k17, 2, sizeof(cl_mem), (void *)&mem13_obj);
      clEnqueueWriteBuffer(command_queue, mem13_obj, CL_TRUE, 0, mem5c*sizeof(int), mem13, 0, NULL, NULL);
      cl_mem mem15_obj = clCreateBuffer(context, CL_MEM_READ_WRITE, sizeof(int), NULL, NULL);
      clSetKernelArg(k17, 3, sizeof(cl_mem), (void *)&mem15_obj);
      clEnqueueWriteBuffer(command_queue, mem15_obj, CL_TRUE, 0, sizeof(int), &mem15, 0, NULL, NULL);
      global_item_size = mem5c;
      local_item_size = min(arg2c,1024);
      clEnqueueNDRangeKernel(command_queue, k17, 1, NULL, &global_item_size, &local_item_size, 0, NULL, NULL);
      clEnqueueReadBuffer(command_queue, mem13_obj, CL_TRUE, 0, mem5c*sizeof(int), mem13, 0, NULL, NULL);


      memcpy(mem5,mem13,sizeof(int)*mem5c);
      free(mem13);
    }
    memcpy(mem4,mem5,sizeof(int)*mem4c);
    free(mem5);
  }
  free(*out3);
  (*out3) = mem4;
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
  int *res = NULL;

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
  printf("Running test of size %d (%d iterations)\n", arrSize, iter);
  for (int i=-2; i<iter; i++) // Negative i is warmup
  {
    if (i == 0)
      t1 = getRealTime();
    f0(size, a, arrSize, &res);
  }
  t2 = getRealTime();
  double nanos = (t2 - t1) * 1.0e9 / iter;

  outputMeasure("bitonicPIRE.log",nanos, arrSize);

  /// Teardown
  teardown();

  free(a);
  free(res);
  return 0;
}


