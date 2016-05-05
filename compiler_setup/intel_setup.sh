#!/bin/bash
# Environment vars set-up in order to build dl_timer with the Intel compiler
export F90=ifort
export F90FLAGS="-fast"
export LDFLAGS="-fast"
export CC=icc
#export MPIF90=mpiifort
#export OMPFLAGS="-openmp"

