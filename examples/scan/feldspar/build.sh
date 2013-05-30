#!/bin/sh

gcc -std=c99 -O3 -I /usr/local/cuda/include -lOpenCL -I ../../../C_new/  ../../../C_new/feldspar_c99.c main.c scan.c -D_POSIX_C_SOURCE=199309 -lm -lrt
