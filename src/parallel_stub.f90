module dl_timer_parallel
  !> Module containing empty stubs of routines that, in an MPI
  !! build, would make calls to MPI routines. This allows us
  !! to compile without requiring MPI to be installed.
  !! When performing an MPI build we use parallel_mpi.f90
  !! instead.
  implicit none

contains

  function is_parallel()
    logical :: is_parallel
    is_parallel = .FALSE.
    return
  end function is_parallel

  function get_rank()
    integer :: get_rank
    get_rank = 0
    return
  end function get_rank

end module dl_timer_parallel
