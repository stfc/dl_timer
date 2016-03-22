# Makefile for dl_timer tests
INC_DIR = ../src

TEST_EXES = timer_test.exe timer_test_no_repeat.exe \
            timer_test_omp.exe timer_test_mpi.exe

timer_test.exe: ../dl_timer_lib.a timer_test.o
	${F90} ${OMPFLAGS} timer_test.o ../dl_timer_lib.a -o $@

timer_test_no_repeat.exe: ../dl_timer_lib.a timer_test_no_repeat.o
	${F90} ${OMPFLAGS} timer_test_no_repeat.o ../dl_timer_lib.a -o $@

timer_test_omp.exe: ../dl_timer_lib.a timer_test_omp.o
	${F90} ${OMPFLAGS} timer_test_omp.o ../dl_timer_lib.a -o $@

timer_test_mpi.exe: ../dl_timer_lib.a timer_test_mpi.o
	${MPIF90} ${OMPFLAGS} timer_test_mpi.o ../dl_timer_lib.a -o $@

.PHONY: test
test: ${TEST_EXES}
	./timer_test.exe
	./timer_test_no_repeat.exe
	./timer_test_omp.exe
	mpirun -np 2 ./timer_test_mpi.exe

.PHONY: clean
clean:
	rm -f *.o *~

.PHONY: allclean
allclean: clean
	rm -f *.exe

# This test case must be compiled with MPI include path
timer_test_mpi.o: timer_test_mpi.f90
	$(MPIF90) $(F90FLAGS) -I$(INC_DIR) -c $<

# This test case must be compiled with OpenMP enabled
timer_test_omp.o: timer_test_omp.f90
	$(F90) $(F90FLAGS) $(OMPFLAGS) -I$(INC_DIR) -c $<

%.o: %.f90
	$(F90) $(F90FLAGS) -I${INC_DIR} -c $<

%.o: %.F90
	$(F90) $(F90FLAGS) -I${INC_DIR} -c $<
