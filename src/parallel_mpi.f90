module dl_timer_parallel
  use mpi
  !> Module containing all routines that involve calls to MPI routines.
  implicit none

contains

  function is_parallel()
    logical :: is_parallel
    is_parallel = .TRUE.
    return
  end function is_parallel

  function get_rank()
    integer :: get_rank
    integer :: ierr
    call MPI_COMM_RANK(MPI_COMM_WORLD, get_rank, ierr)
    return
  end function get_rank

end module dl_timer_parallel
