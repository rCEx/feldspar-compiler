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
static cl_kernel k6;
static cl_kernel k11;
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
  k6 = clCreateKernel(program, "k6", NULL);
  k11 = clCreateKernel(program, "k11", NULL);

}
void f0(int* arg1, int arg1c, int* arg2, int arg2c, int** out4) {
  int mem3;
  mem3 = min(arg1c,arg2c);
  int mem5c;
  mem5c = mem3;
  cl_mem mem5 = clCreateBuffer(context,CL_MEM_READ_WRITE,(mem5c * sizeof(int)),NULL,NULL);
  clSetKernelArg(k6, 0, sizeof(cl_mem), &mem5);
  clSetKernelArg(k6, 1, sizeof(cl_mem), &arg1);
  clSetKernelArg(k6, 2, sizeof(cl_mem), &arg2);
  size_t global_item_size = mem3;
  size_t local_item_size = 1024;
  clEnqueueNDRangeKernel(command_queue, k6, 1, NULL, &global_item_size, &local_item_size, 0, NULL, NULL);
  int mem7c;
  for(int q = 0; q < (31 - bitScan_fun_int32_t(mem3)); q++) {
    int mem9;
    mem9 = pow(2,(q + 1));
    int mem10;
    mem10 = pow(2,q);
    mem7c = mem5c;
    cl_mem mem7 = clCreateBuffer(context,CL_MEM_READ_WRITE,(mem7c * sizeof(int)),NULL,NULL);
    clSetKernelArg(k11, 0, sizeof(cl_mem), &mem5);
    clSetKernelArg(k11, 1, sizeof(cl_mem), &mem7);
    clSetKernelArg(k11, 2, sizeof(int), &mem9);
    clSetKernelArg(k11, 3, sizeof(int), &mem10);
    global_item_size = mem5c;
    local_item_size = 1024;
    clEnqueueNDRangeKernel(command_queue, k11, 1, NULL, &global_item_size, &local_item_size, 0, NULL, NULL);
    clEnqueueCopyBuffer(command_queue,mem7,mem5,0,0,(mem7c * sizeof(int)),0,NULL,NULL);
    clReleaseMemObject(mem7);
  }
  clEnqueueReadBuffer(command_queue,mem5,CL_TRUE,0,(mem5c * sizeof(int)),(*out4),0,NULL,NULL);
  clReleaseMemObject(mem5);
}
