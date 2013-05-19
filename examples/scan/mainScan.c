#include <stdio.h>
#include <stdlib.h>
#include <CL/cl.h>
#include <math.h>
#include <time.h>
#include "feldspar_c99.h"
#define MAX_SOURCE_SIZE (0x100000)


void f0(int* arg1, int arg1c, int** out2) {
  FILE *fp = NULL;
  char* source_str;
  fp = fopen( "kernels.cl" , "r");
  source_str = (char*) malloc(MAX_SOURCE_SIZE);
  size_t source_size = fread( source_str, 1, MAX_SOURCE_SIZE, fp);
  fclose( fp );
  cl_platform_id platform_id = NULL;
  cl_device_id device_id = NULL;
  cl_uint ret_num_devices;
  cl_uint ret_num_platforms;
  clGetPlatformIDs(1, &platform_id, &ret_num_platforms);
  clGetDeviceIDs(platform_id, CL_DEVICE_TYPE_DEFAULT, 1, &device_id, &ret_num_devices);
  cl_context context = clCreateContext(NULL, 1, &device_id, NULL, NULL, NULL);
  cl_command_queue command_queue = clCreateCommandQueue(context, device_id, 0, NULL);
  


  int mem3c;
  mem3c = arg1c;
  int* mem3 = (int*) malloc(sizeof(int) * mem3c);
  mem3 = arg1;
  int mem4c;
  for(int n = 0; n < (32 - bitScan_fun_int32_t(arg1c)); n++) {
    mem4c = mem3c;
    int* mem4 = (int*) malloc(sizeof(int) * mem4c);
    cl_program program = clCreateProgramWithSource(context, 1, (const char **)&source_str, (const size_t *)&source_size, NULL);
    clBuildProgram(program, 1, &device_id, NULL, NULL, NULL);
    cl_kernel kernel = clCreateKernel(program, "k6", NULL);
    cl_mem n_obj = clCreateBuffer(context, CL_MEM_READ_WRITE, sizeof(int), NULL, NULL);
    clSetKernelArg(kernel, 0, sizeof(cl_mem), (void *)&n_obj);
    clEnqueueWriteBuffer(command_queue, n_obj, CL_TRUE, 0, sizeof(int), &n, 0, NULL, NULL);
    cl_mem mem3_obj = clCreateBuffer(context, CL_MEM_READ_WRITE, mem3c*sizeof(int), NULL, NULL);
    clSetKernelArg(kernel, 1, sizeof(cl_mem), (void *)&mem3_obj);
    clEnqueueWriteBuffer(command_queue, mem3_obj, CL_TRUE, 0, mem3c*sizeof(int), mem3, 0, NULL, NULL);
    cl_mem mem4_obj = clCreateBuffer(context, CL_MEM_READ_WRITE, mem3c*sizeof(int), NULL, NULL);
    clSetKernelArg(kernel, 2, sizeof(cl_mem), (void *)&mem4_obj);
    clEnqueueWriteBuffer(command_queue, mem4_obj, CL_TRUE, 0, mem3c*sizeof(int), mem4, 0, NULL, NULL);
    size_t global_item_size = mem3c;
    size_t local_item_size = 1024;
    clEnqueueNDRangeKernel(command_queue, kernel, 1, NULL, &global_item_size, &local_item_size, 0, NULL, NULL);
    clEnqueueReadBuffer(command_queue, mem4_obj, CL_TRUE, 0, mem3c*sizeof(int), mem4, 0, NULL, NULL);


    mem3 = mem4;
  }
  (*out2) = mem3;
}


void outputMeasure(char *to, clock_t time, int size) {
  FILE *fp = fopen(to, "a");
  if(fp != NULL) {
    fprintf(fp, "%li %f %i\n", time, ((double)time)/CLOCKS_PER_SEC, size);
  }
  fclose(fp);
}

int main (int argc, char *argv[]) {
  const int size = atoi(argv[1]);
  int *a = (int*) malloc(sizeof(int)*size);
  int i;
  for(i = 0; i < size; i++) {
    a[i] = i%4;
  }
  int *res;
  clock_t t;
  t = clock();
  f0(a, size, &res);
  t = clock() - t;
  outputMeasure("scanPIRE.log",t, size);
  printf("%i : res \n", res[size-1]);
  return 0;
}
