!> Provides an interface to the Intel-specifc getticks routine which
!! queries the Time-Stamp Counter (rdtsc) register.
MODULE intel_timer_mod
   USE iso_c_binding
   IMPLICIT none

   PUBLIC

   ! Need 64-bit integers when using the Intel counter for timing
   INTEGER, PARAMETER :: int64 = SELECTED_INT_KIND(14)

CONTAINS

  ! Dummy implementation so that we have something to link to when
  ! not using the Intel compiler
  FUNCTION getticks()
    INTEGER (C_INT64_T) :: getticks
    write (*,*) 'TIMING: ERROR: stub version of getticks() called!'
    getticks = 1
  END FUNCTION getticks

END MODULE intel_timer_mod
