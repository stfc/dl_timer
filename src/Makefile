# Modules that are independent of whether we're using MPI
COMMON_MODULES = intel_timer_mod.o

SM_MODULES = parallel_stub.o
DM_MODULES = parallel_mpi.o

# Build non-mpi version by default
all: sm_build

.PHONY: sm_build
sm_build:
	${MAKE} MODULES="${COMMON_MODULES} ${SM_MODULES}" LIB_NAME=${LIB_NAME} FC=${F90} lib

.PHONY: dm_build
dm_build:
	${MAKE} MODULES="${COMMON_MODULES} ${DM_MODULES}" LIB_NAME=${LIB_NAME} FC=${MPIF90} lib

lib: ${MODULES} dl_timer.o
	${AR} ${ARFLAGS} ${LIB_NAME} ${MODULES} dl_timer.o

.PHONY: clean
clean:
	rm -f *.o *.mod *~

.PHONY: allclean
allclean: clean
	rm -f *.a

# We have to build the timing module with OpenMP enabled in
# order to use the OpenMP timing routine
dl_timer.o: ${MODULES} dl_timer.f90
	$(FC) $(F90FLAGS) $(OMPFLAGS) -c dl_timer.f90

# Have to compile the MPI part of the library using a Fortran
# compiler with the necessary incl paths etc.
parallel_mpi.o: parallel_mpi.f90
	$(FC) $(F90FLAGS) $(OMPFLAGS) -c $<

%.o: %.f90
	$(F90) $(F90FLAGS) -c $<

%.o: %.F90
	$(F90) $(F90FLAGS) -c $<
