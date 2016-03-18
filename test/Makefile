INC_DIR = ../src

timer_test.exe: ../dl_timer_lib.a timer_test.o
	${F90} ${OMPFLAGS} timer_test.o ../dl_timer_lib.a -o $@

test: timer_test.exe
	./timer_test.exe

clean:
	rm -f *.o *~

allclean: clean
	rm -f *.exe


%.o: %.f90
	$(F90) $(F90FLAGS) -I${INC_DIR} -c $<

%.o: %.F90
	$(F90) $(F90FLAGS) -I${INC_DIR} -c $<
