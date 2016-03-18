MODULES = timing_mod.o \
          intel_timer_mod.o

API_LIB = ${LIB_NAME}

all: ${API_LIB}

${API_LIB}: ${MODULES}
	${AR} ${ARFLAGS} ${API_LIB} ${MODULES}

clean:
	rm -f *.o *.mod *~

allclean:
	rm -f *.a

# We have to build the timing module with OpenMP enabled in
# order to use the OpenMP timing routine
timing_mod.o: intel_timer_mod.o
	$(F90) $(F90FLAGS) $(OMPFLAGS) -c timing_mod.f90

%.o: %.f90
	$(F90) $(F90FLAGS) -c $<

%.o: %.F90
	$(F90) $(F90FLAGS) -c $<
