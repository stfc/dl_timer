# The name of the library/archive file we will build
DLT_LIB = dl_timer_lib.a
# Location of the source files
SRC_DIR = src
# Location of the test code
TEST_DIR = test

${DLT_LIB}: ${SRC_DIR}/*.?90
	${MAKE} --directory=${SRC_DIR} LIB_NAME=${DLT_LIB}
	mv ${SRC_DIR}/${DLT_LIB} .

# The directory 'test' does actually exist but this target does not
# create or update it - therefore mark it as phony.
.PHONY: test
test:
	${MAKE} ${DLT_LIB}
	${MAKE} --directory=test test

.PHONY: clean
clean:
	${MAKE} --directory=${SRC_DIR} clean
	${MAKE} --directory=${TEST_DIR} clean

.PHONY: allclean
allclean:
	${MAKE} --directory=${SRC_DIR} allclean
	${MAKE} --directory=${TEST_DIR} allclean
	rm -f ${DLT_LIB} *~
