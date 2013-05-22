#include <stdio.h>
#include <stdlib.h>
#ifdef __APPLE__
#include <OpenCL/opencl.h>
#else
#include <CL/cl.h>
#endif
#include <math.h>
#include <time.h>
#include <string.h>
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
static cl_device_id device_id = NULL;
static cl_command_queue command_queue;
static cl_context context;
static char* source_str;
static size_t source_size;
static cl_kernel k10;
static cl_kernel k17;
void init() {

  FILE *fp = NULL;
  fp = fopen( "kernels.cl" , "r");
  source_str = (char*) malloc(MAX_SOURCE_SIZE);
  source_size = fread( source_str, 1, MAX_SOURCE_SIZE, fp);
  fclose( fp );
  cl_platform_id platform_id = NULL;
  cl_uint ret_num_devices;
  cl_uint ret_num_platforms;
  clGetPlatformIDs(1, &platform_id, &ret_num_platforms);
  clGetDeviceIDs(platform_id, CL_DEVICE_TYPE_DEFAULT, 1, &device_id, &ret_num_devices);
  context = clCreateContext(NULL, 1, &device_id, NULL, NULL, NULL);
  command_queue = clCreateCommandQueue(context, device_id, 0, NULL);
  program = clCreateProgramWithSource(context, 1, (const char **)&source_str, (const size_t *)&source_size, NULL);
  clBuildProgram(program, 1, &device_id, NULL, NULL, NULL);
  k10 = clCreateKernel(program, "k10", NULL);
  k17 = clCreateKernel(program, "k17", NULL);

}
void f0(int arg1, int* arg2, int arg2c, int** out3) {
  int mem4c;
  mem4c = arg2c;
  cl_mem mem4 = clCreateBuffer(context,CL_MEM_READ_WRITE,(mem4c * sizeof(int)),NULL,NULL);
  clEnqueueWriteBuffer(command_queue,mem4,CL_TRUE,0,(mem4c * sizeof(int)),arg2,0,NULL,NULL);
  int mem5c;
  for(int o = 0; o < arg1; o++) {
    int mem7;
    mem7 = (~(4294967295 << (o + 1)));
    int mem8;
    mem8 = (o + 1);
    int mem9c;
    mem9c = mem4c;
    cl_mem mem9 = clCreateBuffer(context,CL_MEM_READ_WRITE,(mem9c * sizeof(int)),NULL,NULL);
    clSetKernelArg(k10, 0, sizeof(cl_mem), &mem4);
    clSetKernelArg(k10, 1, sizeof(int), &mem7);
    clSetKernelArg(k10, 2, sizeof(cl_mem), &mem9);
    clSetKernelArg(k10, 3, sizeof(int), &o);
    size_t global_item_size = mem4c;
    size_t local_item_size = 1024;
    clEnqueueNDRangeKernel(command_queue, k10, 1, NULL, &global_item_size, &local_item_size, 0, NULL, NULL);
    mem5c = mem9c;
    cl_mem mem5 = clCreateBuffer(context,CL_MEM_READ_WRITE,(mem5c * sizeof(int)),NULL,NULL);
    clEnqueueCopyBuffer(command_queue,mem9,mem5,0,0,(mem5c * sizeof(int)),0,NULL,NULL);
    int mem13c;
    for(int w = 0; w < o; w++) {
      int mem15;
      mem15 = (mem8 - (w + 2));
      int mem16;
      mem16 = (1 << mem15);
      mem13c = mem5c;
      cl_mem mem13 = clCreateBuffer(context,CL_MEM_READ_WRITE,(mem13c * sizeof(int)),NULL,NULL);
      clSetKernelArg(k17, 0, sizeof(cl_mem), &mem5);
      clSetKernelArg(k17, 1, sizeof(int), &mem16);
      clSetKernelArg(k17, 2, sizeof(cl_mem), &mem13);
      clSetKernelArg(k17, 3, sizeof(int), &mem15);
      global_item_size = mem5c;
      local_item_size = 1024;
      clEnqueueNDRangeKernel(command_queue, k17, 1, NULL, &global_item_size, &local_item_size, 0, NULL, NULL);
      clEnqueueCopyBuffer(command_queue,mem13,mem5,0,0,(mem13c * sizeof(int)),0,NULL,NULL);
      clReleaseMemObject(mem13);
    }
    clReleaseMemObject(mem9);
    clEnqueueCopyBuffer(command_queue,mem5,mem4,0,0,(mem5c * sizeof(int)),0,NULL,NULL);
    clReleaseMemObject(mem5);
  }
  clEnqueueReadBuffer(command_queue,mem4,CL_TRUE,0,(mem4c * sizeof(int)),(*out3),0,NULL,NULL);
  clReleaseMemObject(mem4);
}
