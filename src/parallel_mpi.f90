module dl_timer_parallel
  !> Module containing all routines that involve calls to MPI routines.
  use mpi
  use dl_timer_constants_mod
  implicit none

contains

  function is_parallel()
    !> Returns .TRUE. to indicate that dl_timer is built with MPI support.
    !! Aborts if MPI_Init() has not yet been called.
    logical :: is_parallel
    integer :: ierr

    call MPI_INITIALIZED(is_parallel, ierr)

    if(.not. is_parallel)then
      write(*, &
           "('TIMING: ERROR: timer_init() must be called after MPI_Init()!')")
      stop
    end if

  end function is_parallel

  !=========================================================================

  function get_rank()
    !> Returns the rank of this process in MPI_COMM_WORLD
    integer :: get_rank
    integer :: ierr
    call MPI_COMM_RANK(MPI_COMM_WORLD, get_rank, ierr)
    return
  end function get_rank

  !=========================================================================

  function num_ranks()
    !> Returns the number of ranks in MPI_COMM_WORLD
    integer :: num_ranks
    integer :: ierr
    call MPI_COMM_SIZE(MPI_COMM_WORLD, num_ranks, ierr)
    return
  end function num_ranks

  !=========================================================================

  subroutine calc_dm_timer_stats(nThreads, ntimers, region_names, &
                                 visit_counts, times,             &
                                 max_times, min_times, sum_times)
    integer,                                  intent(in) :: nThreads, ntimers
    !> The name of each timer on this process
    character(len=LABEL_LEN), dimension(ntimers,nThreads) :: region_names
    !> The total time spent in each region by each thread on this process
    real(wp), dimension(ntimers,nThreads),    intent(in) :: times
    !> The number of times each thread on this process has visited each
    !! timed region. (Can be zero.)
    integer,  dimension(ntimers, nThreads),   intent(in) :: visit_counts
    real(wp), dimension(2,ntimers,nThreads), intent(out) :: max_times, min_times
    real(wp), dimension(ntimers,nThreads),   intent(out) :: sum_times
    ! Locals
    real(wp), allocatable, dimension(:,:,:) :: times_ranks
    ! The names of all timers on all ranks packed into a 1D array. We can do
    ! this because we know that each label is LABEL_LEN chars long
    character(len=1), allocatable, dimension(:) :: labels_ranks
    ! The names of all timers on the local process packed into a 1D array.
    character(len=1), allocatable, dimension(:) :: labels_merged
    character(len=LABEL_LEN), allocatable, dimension(:,:) :: region_names_by_rank
    !> Array to store gather of the counts from all timers on all ranks
    integer, allocatable :: all_counts(:)
    !> Array to store gather of the times from all timers on all ranks
    real(wp), allocatable :: all_times(:)
    !> The number of uniquely-named timed-regions across all PEs
    integer :: unique_region_count
    !> The name of each of these unique regions
    character(len=LABEL_LEN), allocatable, dimension(:) :: unique_region_labels
    !> The number of PEs that have each region
    integer, allocatable, dimension(:) :: unique_region_pe_count
    !> unique_timer_map(timer, rank) gives the index of the unique timed
    !! region on PE rank. If that PE does not have the region then we
    !! store a zero.
    integer, allocatable, dimension(:,:) :: unique_region_map
    logical :: is_unique
    
    integer :: ierr, myrank, nranks
    integer :: j, jt, itimer, irank, index, ichar

    myrank = get_rank()
    nranks = num_ranks()

    allocate(times_ranks(2,ntimers,nThreads), &
             labels_merged(ntimers*LABEL_LEN), &
             labels_ranks(ntimers*nranks*LABEL_LEN), &
             region_names_by_rank(nranks,ntimers),   &
             unique_region_labels(ntimers),          &
             unique_region_pe_count(ntimers),        &
             unique_region_map(ntimers, nranks),     &
             all_counts(ntimers*nranks), all_times(ntimers*nranks), &
             Stat=ierr)
    if(ierr /= 0)then
       write (*,*) 'TIMING: calc_dm_timer_stats: failed to allocate memory'
       return
    end if
    
    labels_ranks(:) = ""
    region_names_by_rank(:,:) = ""

    ! Pack all the timed-region labels on this rank into one long
    ! array of chars
    DO itimer = 1, ntimers
       index = (itimer-1)*LABEL_LEN
       do ichar = 1, LABEL_LEN
          labels_merged(index+ichar) = region_names(itimer,1)(ichar:ichar)
       end do
    END DO

    ! We must pack the timing data into an array suitable for the
    ! reduction operations
    do jt = 1, nThreads, 1
       do itimer = 1, ntimers !itimerCount(jt)
          times_ranks(1,itimer,jt) = times(itimer,jt)
          times_ranks(2,itimer,jt) = myrank
       end do
    end do

    ! ARPDBG this is just for thread 1 currently
    call MPI_Gather(labels_merged, ntimers*LABEL_LEN, MPI_CHARACTER, &
                    labels_ranks, ntimers*LABEL_LEN, MPI_CHARACTER, 0,   &
                    MPI_COMM_WORLD, ierr)

    call MPI_Gather(visit_counts(:,1), ntimers, MPI_INTEGER, &
                    all_counts, ntimers, MPI_INTEGER, 0, &
                    MPI_COMM_WORLD, ierr)
    call MPI_Gather(times(:,1), ntimers, MPI_DOUBLE_PRECISION, &
                    all_times, ntimers, MPI_DOUBLE_PRECISION, 0, &
                    MPI_COMM_WORLD, ierr)

    if(myrank == 0)then

       unique_region_pe_count(:) = 0
       unique_region_map(:,:) = 0

       ! Unpack the timed-region labels from arrays back into strings
       do irank = 1, nranks
          !write (*,*) "Timer labels on rank ", irank-1
          do itimer = 1, ntimers
             index = ((irank-1)*ntimers + itimer - 1)*LABEL_LEN + 1
             do ichar = 1, LABEL_LEN
                region_names_by_rank(irank,itimer)(ichar:ichar) = labels_ranks(index+ichar-1)
             end do
             !write (*,"('  ',I3,': ', (A))") itimer, &
             !                        TRIM(region_names_by_rank(irank, itimer))
          end do
       end do

       ! Now we must work out how the different timed regions on different
       ! ranks are related.
       unique_region_count = 0
       do irank = 1, nranks
          do itimer = 1, ntimers
             ! Skip blank labels
             if(len_trim(region_names_by_rank(irank,itimer)) == 0)cycle
             is_unique = .TRUE.
             do j = 1, unique_region_count
                if (region_names_by_rank(irank,itimer) == unique_region_labels(j)) then
                   is_unique = .FALSE.
                   exit
                end if
             end do
             if (is_unique) then
                ! We haven't seen a region with this name before
                unique_region_count = unique_region_count + 1
                unique_region_labels(unique_region_count) = region_names_by_rank(irank,itimer)
                ! Store the index of this region on this PE
                unique_region_pe_count(j) = 1
                unique_region_map(unique_region_count,irank) = itimer
             else
                ! We have seen this region before. Store its index on this PE.
                unique_region_pe_count(j) = unique_region_pe_count(j) + 1
                unique_region_map(j, irank) = itimer
             end if
          end do
       end do

       write (*,*) "We have ",unique_region_count," unique timed regions"
       do itimer = 1, unique_region_count
          write (*,"(I3,': ',(A), ' - Appears on ',I3,' ranks')") itimer, TRIM(unique_region_labels(itimer)), unique_region_pe_count(itimer)
       end do

    end if ! rank 0

    ! For each timed region, find the maximum time spent inside it and
    ! the rank of the corresponding process.
    ! ARPDBG this is just for thread 1 currently
    call MPI_Reduce(times_ranks(:,:,1), max_times(:,:,1), ntimers,  &
                    MPI_2DOUBLE_PRECISION, MPI_MAXLOC, 0,           &
                    MPI_COMM_WORLD, ierr)

    ! Ditto for the minimum
    call MPI_Reduce(times_ranks(:,:,1), min_times(:,:,1), ntimers, &
                    MPI_2DOUBLE_PRECISION, MPI_MINLOC, 0,          &
                    MPI_COMM_WORLD, ierr)
    ! The total time spent in each region summed over all processes
    call MPI_Reduce(times(:,1), sum_times(:,1), ntimers,  &
                    MPI_DOUBLE_PRECISION, MPI_SUM, 0, MPI_COMM_WORLD, ierr)
    return
  end subroutine calc_dm_timer_stats

end module dl_timer_parallel
