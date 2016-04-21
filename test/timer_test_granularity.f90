!> Simple program to test the granularity/overhead of the timing API
PROGRAM timer_test_granularity
  use dl_timer

  integer, parameter :: r_def = KIND(1.0d0)

  integer :: time0
  integer :: istep
  integer, parameter :: nstep = 10000

  !--------------------------------------------------------------
  ! Initialisation

  call timer_init()

  !--------------------------------------------------------------
  ! Time-stepping

  do istep = 1, nstep
     call timer_start('Time-step', time0)
     call timer_stop(time0)
  end do

  !---------------------------------------------------------------
  ! Finalise

  call timer_report()

END PROGRAM timer_test_granularity
