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
static cl_kernel k8;
static cl_kernel k12;
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
  k8 = clCreateKernel(program, "k8", NULL);
  k12 = clCreateKernel(program, "k12", NULL);

}
void f0(int* arg1, int arg1c, int* arg3, int arg3c, int** out6) {
  int mem2c;
  mem2c = arg1c;
  cl_mem mem2 = clCreateBuffer(context,CL_MEM_READ_WRITE,(mem2c * sizeof(int)),NULL,NULL);
  clEnqueueWriteBuffer(command_queue,mem2,CL_TRUE,0,(mem2c * sizeof(int)),arg1,0,NULL,NULL);
  int mem4c;
  mem4c = arg3c;
  cl_mem mem4 = clCreateBuffer(context,CL_MEM_READ_WRITE,(mem4c * sizeof(int)),NULL,NULL);
  clEnqueueWriteBuffer(command_queue,mem4,CL_TRUE,0,(mem4c * sizeof(int)),arg3,0,NULL,NULL);
  int mem5;
  mem5 = min(mem2c,mem4c);
  int mem7c;
  mem7c = mem5;
  cl_mem mem7 = clCreateBuffer(context,CL_MEM_READ_WRITE,(mem7c * sizeof(int)),NULL,NULL);
  clSetKernelArg(k8, 0, sizeof(cl_mem), &mem7);
  clSetKernelArg(k8, 1, sizeof(cl_mem), &mem2);
  clSetKernelArg(k8, 2, sizeof(cl_mem), &mem4);
  size_t global_item_size = mem5;
  size_t local_item_size = 1024;
  clEnqueueNDRangeKernel(command_queue, k8, 1, NULL, &global_item_size, &local_item_size, 0, NULL, NULL);
  for(int r = 0; r < (31 - bitScan_fun_int32_t(mem5) - 1); r++) {
    int mem10;
    mem10 = pow(2,(r + 1));
    int mem11;
    mem11 = pow(2,r);
    clSetKernelArg(k12, 0, sizeof(cl_mem), &mem7);
    clSetKernelArg(k12, 1, sizeof(int), &mem10);
    clSetKernelArg(k12, 2, sizeof(int), &mem11);
    global_item_size = mem7c;
    local_item_size = 1024;
    clEnqueueNDRangeKernel(command_queue, k12, 1, NULL, &global_item_size, &local_item_size, 0, NULL, NULL);
  }
  clEnqueueReadBuffer(command_queue,mem7,CL_TRUE,0,(mem7c * sizeof(int)),(*out6),0,NULL,NULL);
  clReleaseMemObject(mem7);
  clReleaseMemObject(mem4);
  clReleaseMemObject(mem2);
}
