# Makefile for dl_timer tests
INC_DIR = ../src

timer_test.exe: ../dl_timer_lib.a timer_test.o
	${F90} ${OMPFLAGS} timer_test.o ../dl_timer_lib.a -o $@

timer_test_no_repeat.exe: ../dl_timer_lib.a timer_test_no_repeat.o
	${F90} ${OMPFLAGS} timer_test_no_repeat.o ../dl_timer_lib.a -o $@

test: timer_test.exe timer_test_no_repeat.exe
	./timer_test.exe
	./timer_test_no_repeat.exe

clean:
	rm -f *.o *~

allclean: clean
	rm -f *.exe


%.o: %.f90
	$(F90) $(F90FLAGS) -I${INC_DIR} -c $<

%.o: %.F90
	$(F90) $(F90FLAGS) -I${INC_DIR} -c $<
