module dl_timer_parallel
  use mpi
  !> Module containing all routines that involve calls to MPI routines.
  implicit none

  ! This kind parameter definition is repeated from dl_timer.f90. We
  ! could fix this by having a global types module.
  integer, parameter :: wp = SELECTED_REAL_KIND(12,307)

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

  subroutine calc_dm_timer_stats(nThreads, ntimers, &
                                 times, max_times, min_times, sum_times)
    integer,                                  intent(in) :: nThreads, ntimers
    !> The total time spent in each region by each thread on this process
    real(wp), dimension(ntimers,nThreads),    intent(in) :: times
    real(wp), dimension(2,ntimers,nThreads), intent(out) :: max_times, min_times
    real(wp), dimension(ntimers,nThreads),   intent(out) :: sum_times
    ! Locals
    real(wp), allocatable, dimension(:,:,:) :: times_ranks
    integer :: ierr
    integer :: jt, itimer

    ! We must pack the timing data into an array suitable for the
    ! reduction operations
    do jt = 1, nThreads, 1
       do itimer = 1, ntimers !itimerCount(jt)
          times_ranks(1,itimer,jt) = times(itimer,jt)
          times_ranks(2,itimer,jt) = get_rank()
       end do
    end do


    ! For each timed region, find the maximum time spent inside it and
    ! the rank of the corresponding process.
    ! ARPDBG this is just for thread 1 currently
    call MPI_Reduce(times_ranks(:,:,1), max_times(:,:,1), ntimers,  &
                    MPI_2DOUBLE_PRECISION, MPI_MAXLOC, 0,     &
                    MPI_COMM_WORLD, ierr)
    ! Ditto for the minimum
    call MPI_Reduce(times_ranks(:,:,1), min_times(:,:,1), ntimers, &
                    MPI_2DOUBLE_PRECISION, MPI_MINLOC, 0,    &
                    MPI_COMM_WORLD, ierr)
    ! The total time spent in each region summed over all processes
    call MPI_Reduce(times(:,1), sum_times(:,1), ntimers,  &
                    MPI_DOUBLE_PRECISION, MPI_SUM, 0, MPI_COMM_WORLD, ierr)

    return
  end subroutine calc_dm_timer_stats

end module dl_timer_parallel
