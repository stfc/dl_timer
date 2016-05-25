MODULE dl_timer
  use iso_c_binding
  use intel_timer_mod
!$ USE omp_lib
  IMPLICIT none

  PRIVATE

   !-------------------------------------------------------------------
   ! Define some constants to identify the different timers that
   ! we support

   !> Intel-specific rdtsc timer (reads the Time Stamp Counter register)
   INTEGER, PARAMETER :: RDTSC_TIMER = 0
   !> Use the OpenMP wtime routine (must link against OpenMP)
   INTEGER, PARAMETER :: OMP_TIMER=1
   !> Use the Fortran intrinsic timer (precision may be limited)
   INTEGER, PARAMETER :: INTRINSIC_TIMER=2
   !> Use the C routine gettimeofday() (microsecond precision)
   INTEGER, PARAMETER :: TOFDAY_TIMER=3
   !> Use the POSIX, montonic clock
   INTEGER, PARAMETER :: POSIX_TIMER=4

   !-------------------------------------------------------------------
   ! Section that configures which timer is used

   !> Which timer type to use by default
   INTEGER, PARAMETER :: base_timer = POSIX_TIMER

   !> Whether to record time-series data. When dl-timer is built
   !! DM parallel, only rank 0 writes out time-line data.
   LOGICAL, PARAMETER :: record_time_series = .FALSE.

   !------------------------------------------------------------------
   ! Type definitions
   !> double precision (real 8)
   INTEGER, PARAMETER :: wp = SELECTED_REAL_KIND(12,307)
   !> 32-bit integer
   INTEGER, PARAMETER :: idef32 = selected_int_kind(9)
   !> 64-bit integer
   INTEGER, PARAMETER :: idef64 = selected_int_kind(12)
   !> Single precision
   INTEGER, PARAMETER :: sp = KIND(1.0)

   !> Tolerance below which we consider a number to be zero
   REAL(wp), PARAMETER :: TOL_ZERO  = 1.0E-10

   !> Unit for stdout
   INTEGER, PARAMETER :: OUT_UNIT = 6
   INTEGER, PARAMETER :: ERR_UNIT = 6

   !-------------------------------------------------------------------
   ! Parameters and types for the timing routines

   INTEGER :: iclk_rate !< Ticks per second of Fortran timer
   INTEGER :: iclk_max  !< Max value that Fortran timer can return
   REAL(wp) :: clock_tick_s !< Time in seconds between clock ticks
   REAL(wp) :: systematic_err(2) !< Measured systematic error (in the
                                 !! units of the chosen clock) and 
                                 !! associated std err
   REAL(wp) :: noreg_overhead !< Overhead of calling dl_timer start/stop API
                              !! without pre-registering timer
   REAL(wp) :: prereg_overhead !< Overhead of calling dl_timer start/stop
                               !! API when timer is pre-registered

   !> Maximum length of the label for a timed region
   INTEGER, PARAMETER :: LABEL_LEN  = 128
   !> Maximum number of distinct timed regions that an application
   !! may have 
   INTEGER, PARAMETER :: MAX_TIMERS = 30
   !> How many samples to keep when recording a time-line
   INTEGER, PARAMETER :: TIME_SERIES_LEN = 10000

   TYPE :: timer_type
      !> The name of this timed region
      CHARACTER (LABEL_LEN) :: label
      !> Time at which region was most recently entered
      REAL       (KIND=wp)  :: istart
      !> Total time spent in this timed region (accumulated over
      !! all visits).
      REAL       (KIND=wp)  :: total
      !> Sum of the square of the times spent in this timed region
      !! (accumulated over all visits).
      REAL       (KIND=wp)  :: totalsq
      !> The no. of times this timed region has been executed.
      INTEGER (KIND=idef64) :: count
      !> The no. of repeated intervals within this timed region.
      !! Used in timer_report() to produce a mean time per repeat.
      !! Default value is 1. User can specify value in call to
      !! timer_start().
      INTEGER (KIND=idef64) :: nrepeat
      !> Single-precision array to hold the individual time periods that we
      !! collect if producing a time-line
      REAL(KIND=sp), ALLOCATABLE :: time_series(:)
   END TYPE timer_type

   INTEGER, SAVE :: nThreads ! No. of OMP threads being used (1 if no OMP)
                             ! Set in timer_init().

   TYPE(timer_type), ALLOCATABLE, SAVE, DIMENSION(:,:) :: timer

   !> The number of timed regions created for each thread
   INTEGER, ALLOCATABLE, SAVE, DIMENSION(:) :: itimerCount
   
   !-------------------------------------------------------------------
   ! Interfaces to other routines

   INTERFACE
      !> Wrapper for the C code that calls gettimeofday which has
      !! microsecond resolution.
      function time_of_day() bind(c)
        ! An interface body does not automatically import names from
        ! surrounding scope
        import :: C_DOUBLE
        real(C_DOUBLE) :: time_of_day
      end function time_of_day

      function posix_clock_init(resolution) bind(c)
        import :: C_INT, C_DOUBLE
        integer(C_INT) :: posix_clock_init
        real(C_DOUBLE) :: resolution
      end function posix_clock_init

      function posix_clock() bind(c)
        import :: C_DOUBLE
        real(C_DOUBLE) :: posix_clock
      end function posix_clock
   END INTERFACE

  !-------------------------------------------------------------------
  ! Publicly-accessible routines

  PUBLIC timer_init, timer_register, timer_start, timer_stop, timer_report

CONTAINS

   !======================================================================

   !> Returns the current system time using the selected timer
   function time_now()
     implicit none
     real(wp) :: time_now
     integer :: iclk

     select case(base_timer)
     case(OMP_TIMER)
! Requires that this file be compiled with OpenMP enabled
!$      time_now = omp_get_wtime()         
     case(RDTSC_TIMER)
        time_now = REAL(getticks(), wp)
     case(INTRINSIC_TIMER)
        CALL SYSTEM_CLOCK(iclk)
        time_now = REAL(iclk, wp)
     case(TOFDAY_TIMER)
        time_now = time_of_day()
     case(POSIX_TIMER)
        time_now = posix_clock()
     end select

   end function time_now

   !======================================================================

   SUBROUTINE timer_init()
      use dl_timer_parallel, only: is_parallel, get_rank
      IMPLICIT none
      ! Set-up timing
      INTEGER :: ji, ith, ierr
      integer :: myrank
      logical :: dm_parallel

      ! Query whether or not we have been built DM parallel. If we have
      ! but MPI_Init() has not yet been called then we abort.
      dm_parallel = is_parallel()
      myrank = get_rank()

      ! Check that timer_init hasn't been called from within an OMP PARALLEL
      ! region.
!$    IF(omp_get_num_threads() > 1)THEN
!$OMP MASTER
!$      WRITE(OUT_UNIT, &
!$            "('timer_init: ERROR: cannot be called from within OpenMP PARALLEL region.')")
!$OMP END MASTER
!$OMP BARRIER
!$      STOP
!$    END IF

      ! Initialise the timer structures
      iclk_rate = 1
      iclk_max = 1

      select case(base_timer)

      case(OMP_TIMER)
!$       clock_tick_s = omp_get_wtick()

      case(RDTSC_TIMER)
         ! Check that the RDTSC timer is available (must have been compiled
         ! with the Intel compiler). If it isn't then we abort.
         if(rdtsc_available() /= 1)then
            write (OUT_UNIT,"('TIMING: ERROR: dl_timer configured to use ' &
                          & 'RDTSC but not built with Intel compiler.')")
            stop
         end if
         clock_tick_s = 1.0d0 ! TODO work out how to get this quantity

      case(INTRINSIC_TIMER)
         call SYSTEM_CLOCK(COUNT_RATE=iclk_rate, COUNT_MAX=iclk_max)
         clock_tick_s = 1.0d0/REAL(iclk_rate)

      case(TOFDAY_TIMER)
         continue

      case(POSIX_TIMER)
         if( posix_clock_init(clock_tick_s) /= 1)then
            write (OUT_UNIT,"('TIMING: ERROR: dl_timer configured to use ' &
                          & 'POSIX timer but system does not support it.')")
            stop
         end if
      end select

      nThreads = 1
!$    nThreads = omp_get_max_threads()

      ALLOCATE(timer(MAX_TIMERS,nThreads), itimerCount(nThreads), &
               Stat=ierr)

      IF(ierr /= 0)THEN
         WRITE (OUT_UNIT,*) 'timer_init: ERROR: failed to allocate timer structures'
         stop
      END IF

      ! Allocate memory required for recording time-series data
      IF(RECORD_TIME_SERIES)THEN
         DO ith = 1, nThreads, 1
            DO ji=1,MAX_TIMERS,1
               allocate(timer(ji,ith)%time_series(TIME_SERIES_LEN), &
                        Stat=ierr)
               if(ierr /= 0)then
                  write (OUT_UNIT, &
                       "('timer_init: ERROR: failed to allocate time-series array')")
                  return
               end if
            END DO
         END DO
      END IF

      call clear_timers()

      ! We have to estimate our systematic error here because we use the
      ! dl_timer data structures to do that and so we must wipe them
      ! afterwards.
      call estimate_systematic_error()

   END SUBROUTINE timer_init

   !=========================================================================

   function timer_granularity()
     implicit none
     real(wp) :: timer_granularity
     !> Measure the effective granularity of the timer by calling
     !! it repeatedly and looking at the minimum amount of time
     !! between the times it returns
     integer,  parameter :: ntimes = 10000
     real(wp) :: times(ntimes)
     real(wp) :: diff, min_diff
     integer  :: i, j
     do i=1,ntimes
       times(i) = time_now()
     end do
     min_diff = 1.0E10
     do i=1,ntimes-1
        j = i+1
        do while(j<ntimes)
           diff = times(j) - times(i)
           if(diff > TOL_ZERO)exit
           j = j + 1
        end do
        if(diff > TOL_ZERO .and. diff < min_diff) min_diff = diff
     end do
     timer_granularity = min_diff
   end function timer_granularity

   !=========================================================================

   subroutine estimate_systematic_error()
     implicit none
     !> Estimate the systematic error and overheads associated with dl_timer
     integer, parameter :: ntimes = 50000
     integer :: i, itime1, itime2, itime3, itime4

     ! We time a completely empty region using the full dl_timer 'public'
     ! API...
     call timer_start(itime1, label='Timed region')
     do i=1, ntimes
        call timer_start(itime2, label='Empty region')
        call timer_stop(itime2)
     end do
     call timer_stop(itime1)

     ! Repeat but register the innermost timer first to reduce overhead
     call timer_register(itime4, label="Empty region 2")
     call timer_start(itime3, label='Timed region 2')
     do i=1, ntimes
        call timer_start(itime4)
        call timer_stop(itime4)
     end do
     call timer_stop(itime3)

     ! Calculate and store the average time spent 'doing nothing'.
     ! This is then reported in timer_report() at the end of the run.
     systematic_err(1) = timer(itime2,1)%total / &
                         REAL(timer(itime2,1)%count, wp)
     ! Calculate and store the statistical error in this result
     systematic_err(2) = time_err(timer(itime2,1))

     ! Estimate the overhead in calling the dl_timer start+stop API when
     ! the timer is not pre-registered
     noreg_overhead = (timer(itime1,1)%total - timer(itime2,1)%total) / &
                REAL(timer(itime2,1)%count, wp)

     ! Estimate the overhead in calling the dl_timer start+stop API when
     ! the timer has been previously registered
     prereg_overhead = (timer(itime3,1)%total - timer(itime4,1)%total) / &
                REAL(timer(itime4,1)%count, wp)

     ! Reset our timers
     call clear_timers()

   end subroutine estimate_systematic_error

   !=========================================================================

   subroutine clear_timers()
     implicit none
     !> Clear all accumulated timing data
     integer :: ith, ji

!$OMP PARALLEL DO default(none), shared(nThreads,itimerCount,timer), &
!$OMP             private(ith, ji)
      DO ith = 1, nThreads, 1
         DO ji=1,MAX_TIMERS,1
            itimerCount(ith) = 0
            timer(ji,ith)%label  = ""
            timer(ji,ith)%istart = 0_int64
            timer(ji,ith)%total  = 0_wp
            timer(ji,ith)%totalsq= 0_wp
            timer(ji,ith)%count  = 0
            if(RECORD_TIME_SERIES)then
               timer(ji,ith)%time_series(:) = 0.0
            end if
         END DO
      END DO
!$OMP END PARALLEL DO

   end subroutine clear_timers

   !=========================================================================

   REAL(wp) FUNCTION time_in_s(clk0,clk1)
      IMPLICIT none
      REAL(wp),    INTENT(in) :: clk0
      REAL(wp), INTENT(inout) :: clk1
      ! This routine only actually returns time in seconds if the
      ! Fortran intrinsic timer (SYSTEM_CLOCK) is being used. Otherwise
      ! iclk_rate has been set to unity and this routine simply returns
      ! the difference between its arguments.

      IF(clk1 < clk0)THEN
         clk1 = clk1 + REAL(iclk_max,wp)
      END IF

      time_in_s =  (clk1 - clk0)/REAL(iclk_rate,wp)

   END FUNCTION time_in_s

!============================================================================

   subroutine timer_register(idx, label, num_repeats)
     implicit none
     !> Register a timer with the supplied string as its name.
     !! Return an integer handle.
     !> The name of the timed region
     character (*), intent(in) :: label
     !> The handle for this region
     integer, intent(out) :: idx
     !> The number of repeated intervals inside this timed region.
     !! Used to report a time per interval in the output generated
     !! by timer_report().
     integer(idef64), intent(in), optional :: num_repeats
     ! Locals
     !> Index of current thread (1 if not using OpenMP)
     integer :: ith
     integer :: ji

     if(len_trim(label) > LABEL_LEN)then
        write(ERR_UNIT, &
             "('timer_register: ERROR: length of label >>',(A),'<< exceeds ',I2,' chars')") &
             trim(label), LABEL_LEN
        idx = -1
        return
     end if
      
     ith = 1
!$   ith = 1 + omp_get_thread_num()

     ! Check that there is no existing timer with this name already
     do ji=1,itimerCount(ith),1
        ! Shorter string is padded with blanks so that lengths match
        if(timer(ji,ith)%label == label)then
           idx = ji
           return
        end if
     end do

     if(PRESENT(num_repeats) .AND. num_repeats < 1)then
        write(ERR_UNIT, &
             "('timer_register: ERROR: num_repeats must be > 1 but got ',I4)") &
             num_repeats
        idx = -1
        return
     end if

     ! Create a new timer
     itimerCount(ith) = itimerCount(ith) + 1
     IF(itimerCount(ith) > MAX_TIMERS)THEN
        write(ERR_UNIT, &
             "('timer_register: ERROR: max. no. of timers exceeded!')")
        write(ERR_UNIT, &
             "('timer_register: ERROR: thread = ',I3,'label = ',(A))") &
             ith, label
        idx = -1
        itimerCount(ith) = itimerCount(ith) - 1
        return
     END IF

     idx = itimerCount(ith)
     
     ! Initialise this new timer structure
     timer(idx, ith)%label = TRIM(ADJUSTL(label))
     if(present(num_repeats))then
        timer(idx, ith)%nrepeat = num_repeats
     else
        ! No repeat specified so default to a value of unity.
        timer(idx, ith)%nrepeat = 1
     end if

   end subroutine timer_register
   
!============================================================================

   SUBROUTINE timer_start(handle, label, num_repeats)
      USE intel_timer_mod
      IMPLICIT none
      !> The handle of this timer. Greater than 0 if starting an
      !! existing timer. If creating a new timer then on return contains the
      !! handle for it.
      INTEGER, INTENT(inout) :: handle
      !> The name of this timer (if creating a new one), otherwise none.
      CHARACTER (*), INTENT(in), optional :: label
      !> The number of repeated intervals inside this timed region.
      !! Used to report a time per interval in the report generated
      !! by timer_report().
      INTEGER(idef64), INTENT(in), OPTIONAL :: num_repeats
      INTEGER :: ith

      ith = 1
!$    ith = 1 + omp_get_thread_num()

      if(.not. present(label))then
         ! Using an existing timer - check that supplied handle is valid
         if(handle < 1 .or. handle > itimerCount(ith))then
            write(ERR_UNIT, "('timer_start: ERROR: invalid handle value')")
            return
         end if
      else
         ! This is (potentially) a new timer so register it.
         call timer_register(handle, label, num_repeats)
         if(handle < 1)return
      end if

      ! Increment the count of no. of times we've used this timer
      timer(handle,ith)%count = timer(handle,ith)%count + 1

      ! And finally record the current timer value
      timer(handle,ith)%istart = time_now()

   END SUBROUTINE timer_start

!============================================================================

   SUBROUTINE timer_stop(itag)
      IMPLICIT none
      INTEGER, INTENT(in) :: itag ! Flag identifying the timer
      !> Stop the specified timer and record the elapsed number of ticks
      !! since it was started.
      INTEGER :: ith
      REAL(wp) :: thistime, delta_t

      ! Stop the clock
      thistime = time_now()

      IF(itag < 1)RETURN

      ith = 1
!$    ith = 1 + omp_get_thread_num()

      if(base_timer == INTRINSIC_TIMER)then
         IF( thistime < timer(itag,ith)%istart )THEN
            thistime = thistime + REAL(iclk_max,wp)
         END IF
      end if

      delta_t = thistime - timer(itag,ith)%istart
      timer(itag,ith)%total = timer(itag,ith)%total + delta_t
      timer(itag,ith)%totalsq = timer(itag,ith)%totalsq + delta_t*delta_t

      ! If we're recording a time-series then store this result. We use
      ! single precision for this to save space (so as to try to minimise
      ! the impact of this facility on cache use).
      if(record_time_series .AND. &
         timer(itag,ith)%count < TIME_SERIES_LEN)then
        timer(itag,ith)%time_series(timer(itag,ith)%count) = &
                                                  REAL(delta_t, kind=sp)
      end if

   END SUBROUTINE timer_stop

   !==========================================================================

   SUBROUTINE timer_report()
     use dl_timer_parallel, only: is_parallel, calc_dm_timer_stats
     implicit none
     integer       :: jt, itimer
     integer       :: ierr
     logical       :: have_repeats
     ! Arrays used to gather stats for each timed region when running
     ! MPI parallel
     real(wp), allocatable, dimension(:,:,:) :: max_times, min_times
     real(wp), allocatable, dimension(:,:) :: raw_times, sum_times
     !> The maximum number of header lines
     integer, parameter :: HEADER_LINES = 6
     !> The number of header lines that have content
     integer :: nlines
     !> The array of strings holding the header lines
     character(len=120) :: timer_str(HEADER_LINES)
     !> String holding the units of the chosen timer
     character(len=10) :: units_str

     nlines = 0
     timer_str(:) = ""
     units_str = "s"

     if( is_parallel() )then

        ! If this is a parallel run then, for each timed region, we want
        ! the minimum, maximum and sum (over all processes) of the time
        ! spent inside it.
        allocate(raw_times(MAX_TIMERS, nThreads), &
                 max_times(2, MAX_TIMERS, nThreads), &
                 min_times(2, MAX_TIMERS, nThreads), &
                 sum_times(MAX_TIMERS, nThreads), Stat=ierr)
        if(ierr /= 0)then
           write(*,"('Timer report: failed to allocate memory to gather ', &
                   & 'MPI stats: no timing report generated')")
           return
        end if

        ! We must pack the timing data into an array suitable for the
        ! reduction operations
        do jt = 1, nThreads, 1
           do itimer = 1, itimerCount(jt)
              raw_times(itimer,jt) = timer(itimer,jt)%total
           end do
        end do

        call calc_dm_timer_stats(nThreads, MAX_TIMERS, raw_times, &
                                 max_times, min_times, sum_times)
     end if

     select case(base_timer)
     case(OMP_TIMER)
        nlines = nlines + 1
        write(timer_str(nlines), &
             "('Timed using OpenMP omp_get_wtime. Units are seconds.')")
        nlines = nlines + 1
        write (timer_str(nlines), &
             "('TIMING: time between clock ticks =',1E13.5,' (s)')") &
             clock_tick_s
     case(RDTSC_TIMER)
        units_str = "counts"
        nlines = nlines + 1
        write(timer_str(nlines), &
             "('Timed using Intel Time Stamp Counter (RDTSC). Units are counts.')")
     case(INTRINSIC_TIMER)
        nlines = nlines + 1
        write(timer_str(nlines), &
             "('Timed using Fortran SYSTEM_CLOCK intrinsic. Units are seconds.')")
        nlines = nlines + 1
        write(timer_str(nlines),                          &
             "('cycles/sec = ',I7,', max count = ',I11)") &
             iclk_rate, iclk_max
     case(TOFDAY_TIMER)
        nlines = nlines + 1
        write(timer_str(nlines), &
             "('Timed using gettimeofday(). Units are seconds.')")
     case(POSIX_TIMER)
        nlines = nlines + 1
        write(timer_str(nlines), &
             "('Timed using POSIX timer. Units are seconds.')")        
        nlines = nlines + 1
        write(timer_str(nlines), &
             "('Reported resolution = ',1E10.4,' (s)')") clock_tick_s
     case default
        return
     end select

     nlines = nlines + 1
     write (timer_str(nlines), &
          "('Effective clock granularity = ', 1E11.5,' (',(A),')')")  &
          timer_granularity(), TRIM(units_str)
     nlines = nlines + 1
     write (timer_str(nlines), &
          "('Measured systematic error in dl_timer API = ', 1E11.5,' +/-',1E10.3,' (',(A),')')") &
          systematic_err(1), systematic_err(2), TRIM(units_str)
     nlines = nlines + 1
     write (timer_str(nlines), &
        "('Measured overhead in calling start/stop = ',1E10.4,' (',(A),')')") &
        noreg_overhead, TRIM(units_str)
     nlines = nlines + 1
     write (timer_str(nlines), &
        "('Measured overhead in calling start/stop for registered timer = ',1E10.4,' (',(A),')')") &
        prereg_overhead, TRIM(units_str)

     ! Check whether any of our timed regions have a non-unity
     ! no. of repeats.
     have_repeats = .FALSE.
     do jt = 1, nThreads, 1
        if( ANY( timer(1:itimerCount(jt),jt)%nrepeat > 1) )then
           have_repeats = .TRUE.
           exit
        end if
     end do

     ! Call the appropriate routine to generate the report
     if(is_parallel())then
        call timer_report_parallel(timer_str, nlines, max_times, &
                                   min_times, sum_times)
     else
        if(have_repeats)then
           call timer_report_with_repeats(timer_str, nlines)
        else
           call timer_report_no_repeats(timer_str, nlines)
        end if
     end if

     ! Output time-series data if requested
     if(RECORD_TIME_SERIES) call output_time_series()

    end subroutine timer_report

   !==========================================================================

    subroutine timer_report_no_repeats(timer_str, nlines)
      use dl_timer_parallel, only: get_rank
      implicit none
      integer,          intent(in) :: nlines
      character(len=*), intent(in) :: timer_str(nlines)
      integer       :: ji, jt
      real(kind=wp) :: wtime
      integer       :: rank

      rank = get_rank()
      if(rank == 0)then

         call write_report_header(77, timer_str, nlines)
         write(OUT_UNIT, &
              "('Region',26x,'Counts',5x,'Total',7x,'Average*',5x,'Std Err')")
         write(OUT_UNIT,"(77('-'))")

         do jt = 1, nThreads, 1

            if(itimerCount(jt) > 0 .AND. nThreads > 1)then
               if(jt > 1) write(OUT_UNIT, "(34('- '))")
               write(OUT_UNIT, " ('Thread ',I3)") jt-1
            end if

            do ji=1, itimerCount(jt), 1

               if(base_timer == RDTSC_TIMER)then
                  wtime = timer(ji,jt)%total
               else
                  wtime = time_in_s(0._wp,timer(ji,jt)%total)
               end if

               ! Truncate the label to 32 chars for table-formatting purposes
               write(OUT_UNIT, "((A),1x,I5,1x,E12.5,2x,E12.5,1x,E9.2)")      &
                       timer(ji,jt)%label(1:32), timer(ji,jt)%count, wtime,  &
                       MAX(wtime/REAL(timer(ji,jt)%count)-systematic_err(1), &
                           0.0d0),                                           &
                       time_err(timer(ji,jt))
            end do
         end do

        call write_report_footer(77)
      end if

   end subroutine timer_report_no_repeats

   !==========================================================================

   subroutine timer_report_with_repeats(timer_str, nlines)
     use dl_timer_parallel, only: get_rank
     !> Write the timing report for the case where one or more regions have an
     !! implicit repeat > 1.
     IMPLICIT none
     integer,          intent(in) :: nlines
     character(len=*), intent(in) :: timer_str(nlines)
     INTEGER       :: ji, jt
     REAL(KIND=wp) :: wtime, tmean, trepeat, terr
     integer :: rank

     rank = get_rank()
     if(rank == 0)then

        call write_report_header(88, timer_str, nlines)
        write(OUT_UNIT,"('Region',26x,'Counts',6x,'Total',7x,'Average*   Avg/repeat*  Std Err')")
        write(OUT_UNIT,"(88('-'))")

        do jt = 1, nThreads, 1

           if(itimerCount(jt) > 0 .AND. nThreads > 1)then
              if(jt > 1) WRITE(OUT_UNIT, "(36('- '))")
              WRITE(OUT_UNIT, " ('Thread ',I3)") jt-1
           end if

           do ji=1,itimerCount(jt),1

              if(base_timer == RDTSC_TIMER)then
                 wtime = timer(ji,jt)%total
              else
                 wtime = time_in_s(0._wp,timer(ji,jt)%total)
              end if

              ! Mean time spent in timed region corrected for systematic err
              tmean = MAX(wtime/REAL(timer(ji,jt)%count) - systematic_err(1), &
                          0.0d0)
              ! Mean time spent in the repeated section of code in the
              ! timed region
              trepeat = tmean/REAL(timer(ji,jt)%nrepeat)

              ! Error estimate using quadrature formula for
              ! the time spent in just one of the nrepeat 
              ! regions - use product formula (i.e. fractional error
              ! for time in region == that in one of the nrepeat regions)
              if(tmean > TOL_ZERO)then
                 terr = trepeat*time_err(timer(ji,jt))/tmean
              else
                 terr = 0.0d0
              end if

              ! Truncate the label to 32 chars for table-formatting purposes
              write(OUT_UNIT,                                         &
                   "((A),1x,I6,1x,E12.5,1x,E12.5,1x,E12.5,1x,E9.2)")  &
                   timer(ji,jt)%label(1:32), timer(ji,jt)%count,      &
                   wtime, tmean, trepeat, terr
           end do
        end do

        call write_report_footer(88)
     end if
   END SUBROUTINE timer_report_with_repeats

   !==========================================================================

   subroutine timer_report_parallel(timer_str, nlines, max_times, &
                                    min_times, sum_times)
     use dl_timer_parallel, only: get_rank, num_ranks
     !> Write the timing report when we're MPI parallel
     IMPLICIT none
     integer,          intent(in) :: nlines
     character(len=*), intent(in) :: timer_str(nlines)
     real(kind=wp),    intent(in) :: max_times(:,:,:), min_times(:,:,:), &
                                     sum_times(:,:)
     ! Locals
     integer       :: ji, jt
     integer       :: rank, nproc
     real(wp)      :: rnrepeat, rcount
     character(len=8)  :: minrank, maxrank
     character(len=8)  :: expcount, impcount
     character(len=18) :: repeat_str

     rank = get_rank()
     nproc = num_ranks()

     if(rank == 0)then

        call write_report_header(83, timer_str, nlines)
        write(OUT_UNIT,"(23x,'Counts',21x,'Time per repeat*')")
        write(OUT_UNIT,"('Region',14x,'Explicit(Implt)',2x,'Min[rank]',9x,'Mean',9x,'Max[rank]')")
        write(OUT_UNIT,"(83('-'))")
        do jt = 1, nThreads, 1

           if(itimerCount(jt) > 0 .AND. nThreads > 1)then
              if(jt > 1) WRITE(OUT_UNIT, "(39('- '))")
              WRITE(OUT_UNIT, " ('Thread ',I3)") jt-1
           end if

           do ji=1,itimerCount(jt),1

              ! Convert the ranks to strings as that allows us to produce nicer
              ! formatting
              write(minrank,"(I8)") INT(min_times(2,ji,jt))
              write(maxrank,"(I8)") INT(max_times(2,ji,jt))
              write(expcount, "(I8)") timer(ji,jt)%count
              write(impcount, "(I8)") timer(ji,jt)%nrepeat
              repeat_str = ""
              write(repeat_str,"((A),'(',(A),')')") TRIM(ADJUSTL(expcount)), &
                                                    TRIM(ADJUSTL(impcount))

              ! Total no. of repeats of the region is product of no. of visits
              ! with the number of repeats specified when the timed-region
              ! was created.
              rcount = 1.0d0/REAL(timer(ji,jt)%count, kind=wp)
              rnrepeat = 1.0d0/REAL(timer(ji,jt)%nrepeat, kind=wp)

              ! Truncate the label to 20 chars for table-formatting purposes
              write(OUT_UNIT, "((A),1x,A12,1x,E13.6,' [',(A),']',1x,E13.6,1x,E13.6,' [',(A),']')") &
                   timer(ji,jt)%label(1:20), TRIM(repeat_str),            &
                   MAX((min_times(1,ji,jt)*rcount-systematic_err(1))*rnrepeat,&
                       0.0d0), &
                   TRIM(ADJUSTL(minrank)),                                &
                   MAX((sum_times(ji,jt)*rcount/REAL(nproc)-systematic_err(1))*rnrepeat, 0.0d0), &
                   MAX((max_times(1,ji,jt)*rcount-systematic_err(1))*rnrepeat, &
                       0.0d0), &
                   TRIM(ADJUSTL(maxrank))
           end do
        end do
        call write_report_footer(83)
     end if
   END SUBROUTINE timer_report_parallel

   !===================================================================

   subroutine write_report_header(width, timer_str, nlines)
     !> Write the header section of the timer report
     implicit none
     integer, intent(in) :: width, nlines
     character(len=*), intent(in) :: timer_str(nlines)
     character(len=3) :: width_str, halfwidth_str
     integer :: lwidth, ji

     ! Ensure supplied width is not less than 17 chars because the
     ! central text is 15 chars on its own.
     lwidth = MAX(width, 17)

     write(width_str, "(I3)") lwidth
     write(halfwidth_str, "(I3)") (lwidth-15)/2

     write(OUT_UNIT,"(/"//TRIM(halfwidth_str)//"('='),' Timing report ',"// &
          & TRIM(halfwidth_str)//"('='))")
     write(OUT_UNIT,"((A))") (TRIM(timer_str(ji)), ji=1,nlines)
     write(OUT_UNIT,"("//TRIM(width_str)//"('-'))")

   end subroutine write_report_header

   !===================================================================

   subroutine write_report_footer(width)
     !> Write the footer section of the timer report
     implicit none
     integer, intent(in) :: width
     character(len=3) :: width_str
     write(width_str, "(I3)") width

     write(OUT_UNIT, "("//TRIM(width_str)//"('-'))")
     write(OUT_UNIT, "('* corrected for systematic error')")
     write(OUT_UNIT, "("//TRIM(width_str)//"('=')/)")

   end subroutine write_report_footer

   !===================================================================

   SUBROUTINE output_time_series()
     use dl_timer_parallel, only: get_rank
     implicit none
     !> For each thread and each timed region on process 0, output the
     !! raw time-series data that we've collected -  we do not correct
     !! for systematic error since that can be done in a post-processing
     !! step if required.
     integer :: ith, ji, thr_num, ierr
     !> Unit no. used to create each file
     integer, parameter :: funit=72
     !> The name of the file to create
     character(len=128) :: fname
     !> String representation of current thread idx + 10000
     character(len=5)   :: thr_idx_str

     ! Only process 0 writes out time-line data
     if(get_rank() /= 0)return

     DO ith = 1, nThreads, 1
        ! Loop over the timed regions for this thread
        DO ji = 1, itimerCount(ith)
           ! Construct a string containing the thread index prefixed by
           ! as many zeroes as required
           thr_num = 10000 + ith
           write(thr_idx_str, "(I5)") thr_num
           ! Construct the filename for this time series
           fname='times_'//TRIM(timer(ji,ith)%label)//'_t'//              &
                 & thr_idx_str(3:5)//'.dat'
           open(unit=funit, file=fname, status='unknown', action='write', &
                iostat=ierr)
           if(ierr /= 0)then
              write (*,"('output_time_series: error creating file ',(A),  &
                       & ' - skipping')") fname
              continue
           end if
           write(funit, "('# Time series for region [',(A),']')")         &
                TRIM(timer(ji,ith)%label)
           write(funit, "((E14.6))")                                      &
                timer(ji,ith)%time_series(1:timer(ji,ith)%count)
           close(funit)
        END DO ! timed regions
     END DO ! threads

   END SUBROUTINE output_time_series

   !===================================================================

   function time_err(timer)
     implicit none
     real(wp) :: time_err
     type(timer_type), intent(in) :: timer
     ! Calculate the error in the mean time duration using the
     ! formula for the standard devation = sqrt(<t^2> - <t>^2)

     time_err = SQRT(timer%totalsq/timer%count - (timer%total/timer%count)**2)
     ! The error in our estimate of a population mean from N samples 
     ! is stdev / sqrt(N - 1)
     if (timer%count > 1) then
        time_err = time_err / SQRT(REAL(timer%count - 1, wp))
     end if

   end function time_err

!============================================================================

END MODULE dl_timer
