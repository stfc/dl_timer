# README #

This is the README for the dl_timer library. The package aims to provide
a simple, lightweight and portable timing API.

By default the library configured to use the OpenMP timing routine (as that
has consistently good resolution) so you'll need to at least link
against OpenMP. You can change it to use the intrinsic Fortran timer
or the Intel RDTSC processor counter.

##  Building ##

The Makefiles pick up the compiler to use etc. from the following
environment variables:

* F90      - the command with which to invoke the Fortran compiler
* F90FLAGS - flags to pass to the compiler, e.g. -g
* OMPFLAGS - the flag(s) required to enable OpenMP with the chosen compiler

e.g. to build with Gnu Fortran I use:

    export F90=gfortran
    export F90FLAGS=-g
    export OMPFLAGS=-fopenmp

## Examples ##

There are examples of the usage of dl_timer in the test directory.
In short though it is used like so:

    use dl_timer
    integer :: itimer0

    ! Initialise timing system
    call timer_init()

    call timer_start('Time-stepping', itimer0 )

    ... do some stuff

    ! Stop the timer for the time-stepping section
    call timer_stop(itimer0)

    call timer_report()