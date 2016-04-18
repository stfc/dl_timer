#!/bin/bash
# Environment vars set-up in order to build dl_timer with the Intel compiler
export F90=ifort
export F90FLAGS="-g -C -warn all"
export CC=icc
#export MPIF90=mpiifort
#export OMPFLAGS="-openmp"

