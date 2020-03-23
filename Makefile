# Location of the source files
SRC_DIR = src
# Location of the test code
TEST_DIR = test

# The directory 'test' does actually exist but this target does not
# create or update it - therefore mark it as phony. By default we do the
# non-MPI tests.
.PHONY: sm_lib sm_test dm_lib dm_test test testclean clean allclean

all:
	@echo "Possible make targets are:"
	@echo "   Build: sm_lib (OpenMP support), dm_lib (MPI support)"
	@echo "   Test: sm_test, dm_test"

# The name of the library/archive file we will build
sm_lib:	DLT_LIB=libdl_timer_omp.a
dm_lib:	DLT_LIB=libdl_timer_mpi.a

# Library with shared memory (OpenMP) support
sm_lib: ${SRC_DIR}/*.?90
	${MAKE} --directory=${SRC_DIR} LIB_NAME=${DLT_LIB} sm_build
	mv ${SRC_DIR}/${DLT_LIB} .

# Library with distributed memory (MPI) support
dm_lib: ${SRC_DIR}/*.?90
	${MAKE} --directory=${SRC_DIR} LIB_NAME=${DLT_LIB} dm_build
	mv ${SRC_DIR}/${DLT_LIB} .

test: sm_test

sm_test: DLT_LIB=libdl_timer_omp.a
sm_test: sm_lib
	${MAKE} --directory=test LIB_NAME=${DLT_LIB} sm_test

dm_test: DLT_LIB=libdl_timer_mpi.a
dm_test: dm_lib
	${MAKE} --directory=test LIB_NAME=${DLT_LIB} dm_test

testclean:
	${MAKE} --directory=${TEST_DIR} clean

clean: testclean
	${MAKE} --directory=${SRC_DIR} clean

allclean: clean
	${MAKE} --directory=${SRC_DIR} allclean
	${MAKE} --directory=${TEST_DIR} allclean
	rm -f *.a *~
