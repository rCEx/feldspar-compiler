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

}
void f0(int* arg1, int arg1c, int** out3) {
  int mem2c;
  mem2c = arg1c;
  cl_mem mem2 = clCreateBuffer(context,CL_MEM_READ_WRITE,(mem2c * sizeof(int)),NULL,NULL);
  clEnqueueWriteBuffer(command_queue,mem2,CL_TRUE,0,(mem2c * sizeof(int)),arg1,0,NULL,NULL);
  int mem4c;
  mem4c = mem2c;
  cl_mem mem4 = clCreateBuffer(context,CL_MEM_READ_WRITE,(mem4c * sizeof(int)),NULL,NULL);
  clEnqueueCopyBuffer(command_queue,mem2,mem4,0,0,(mem4c * sizeof(int)),0,NULL,NULL);
  for(int n = 0; n < (31 - bitScan_fun_int32_t(mem2c)); n++) {
    clSetKernelArg(k6, 0, sizeof(int), &n);
    clSetKernelArg(k6, 1, sizeof(cl_mem), &mem4);
    size_t global_item_size = mem4c;
    size_t local_item_size = 1024;
    clEnqueueNDRangeKernel(command_queue, k6, 1, NULL, &global_item_size, &local_item_size, 0, NULL, NULL);
  }
  clEnqueueReadBuffer(command_queue,mem4,CL_TRUE,0,(mem4c * sizeof(int)),(*out3),0,NULL,NULL);
  clReleaseMemObject(mem4);
  clReleaseMemObject(mem2);
}
