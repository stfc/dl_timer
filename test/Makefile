# Makefile for dl_timer tests
INC_DIR = ../src

# Tests not requiring MPI
TEST_EXES = timer_test.exe timer_test_no_repeat.exe \
            timer_test_granularity.exe timer_register_test.exe \
	    timer_test_omp.exe timer_test_omp_no_repeats.exe

# Tests requiring MPI
DM_TEST_EXES = timer_test_mpi.exe

LINK = ${F90} ${OMPFLAGS} ${LDFLAGS}

%.exe: %.o
	${LINK} $< ../dl_timer_lib.a -o $@

timer_test_mpi.exe: ../dl_timer_lib.a timer_test_mpi.o
	${MPIF90} ${OMPFLAGS} ${LDFLAGS} timer_test_mpi.o ../dl_timer_lib.a -o $@

.PHONY: sm_test
sm_test:  ../dl_timer_lib.a ${TEST_EXES}
	./timer_test.exe
	./timer_register_test.exe
	./timer_test_no_repeat.exe
	./timer_test_omp.exe
	./timer_test_omp_no_repeats.exe
	./timer_test_granularity.exe

.PHONY: dm_test
dm_test: ${DM_TEST_EXES}
	mpirun -np 2 ./timer_test_mpi.exe

.PHONY: clean
clean:
	rm -f *.o *~

.PHONY: allclean
allclean: clean
	rm -f *.exe
	rm -f times_*_t0*.dat

# This test case must be compiled with MPI include path
timer_test_mpi.o: timer_test_mpi.f90
	$(MPIF90) $(F90FLAGS) -I$(INC_DIR) -c $<

# These test cases must be compiled with OpenMP enabled
timer_test_omp.o: timer_test_omp.f90
	$(F90) $(F90FLAGS) $(OMPFLAGS) -I$(INC_DIR) -c $<

timer_test_omp_no_repeats.o: timer_test_omp_no_repeats.f90
	$(F90) $(F90FLAGS) $(OMPFLAGS) -I$(INC_DIR) -c $<

%.o: %.f90
	$(F90) $(F90FLAGS) -I${INC_DIR} -c $<

%.o: %.F90
	$(F90) $(F90FLAGS) -I${INC_DIR} -c $<
