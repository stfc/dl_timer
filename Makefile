# The name of the library/archive file we will build
DLT_LIB = dl_timer_lib.a
# Location of the source files
SRC_DIR = src
# Location of the test code
TEST_DIR = test

${DLT_LIB}:
	make --directory=${SRC_DIR} LIB_NAME=${DLT_LIB}
	mv ${SRC_DIR}/${DLT_LIB} .

test: ${DLT_LIB}
	make --directory=test test

clean:
	make --directory=${SRC_DIR} clean
	make --directory=${TEST_DIR} clean

allclean:
	make --directory=${SRC_DIR} allclean
	make --directory=${TEST_DIR} allclean
	rm -f ${DLT_LIB} *~
