# The name of the library/archive file we will build
DLT_LIB = dl_timer_lib.a
# Location of the source files
SRC_DIR = src
# Location of the test code
TEST_DIR = test

# Build the non-mpi version of the library by default
all: sm_lib

.PHONY: sm_lib
sm_lib: ${SRC_DIR}/*.?90
	${MAKE} --directory=${SRC_DIR} LIB_NAME=${DLT_LIB} sm_build
	mv ${SRC_DIR}/${DLT_LIB} .

.PHONY: dm_lib
dm_lib: ${SRC_DIR}/*.?90
	${MAKE} --directory=${SRC_DIR} LIB_NAME=${DLT_LIB} dm_build
	mv ${SRC_DIR}/${DLT_LIB} .

# The directory 'test' does actually exist but this target does not
# create or update it - therefore mark it as phony. By default we do the
# non-MPI tests.
.PHONY: test
test: sm_test

.PHONY: sm_test
sm_test:
	${MAKE} ${DLT_LIB}
	${MAKE} --directory=test sm_test

.PHONY: dm_test
dm_test:
	${MAKE} ${DLT_LIB}
	${MAKE} --directory=test dm_test

.PHONY: clean
clean:
	${MAKE} --directory=${SRC_DIR} clean
	${MAKE} --directory=${TEST_DIR} clean

.PHONY: allclean
allclean:
	${MAKE} --directory=${SRC_DIR} allclean
	${MAKE} --directory=${TEST_DIR} allclean
	rm -f ${DLT_LIB} *~
