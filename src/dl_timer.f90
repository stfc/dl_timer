MODULE dl_timer

  USE intel_timer_mod
!$ USE omp_lib
  IMPLICIT none

  PRIVATE

   !-------------------------------------------------------------------
   ! Define some constants to identify the different timers that
   ! we support

   ! Intel-specific rdtsc timer (reads the Time Stamp Counter register)
   INTEGER, PARAMETER :: RDTSC_TIMER = 0
   ! Use the OpenMP wtime routine (must link against OpenMP)
   INTEGER, PARAMETER :: OMP_TIMER=1
   ! Use the Fortran intrinsic timer (precision may be limited)
   INTEGER, PARAMETER :: INTRINSIC_TIMER=2

   !-------------------------------------------------------------------
   ! Section that configures which timer is used

   !> Whether to use the Intel-specific rdtsc timer (reads the Time Stamp 
   !! Counter register). If false then the Fortran intrinsic SYSTEM_CLOCK 
   !! is used.
   LOGICAL, PARAMETER :: use_rdtsc_timer = .FALSE.
   !> Which timer type to use by default
   INTEGER :: base_timer = OMP_TIMER
   !> Whether to record time-series data - currently only supported
   !! for the OMP timer
   LOGICAL, PARAMETER :: record_time_series = .FALSE.

   !------------------------------------------------------------------
   ! Type definitions
   !: double precision (real 8)
   INTEGER, PARAMETER :: wp = SELECTED_REAL_KIND(12,307)
   ! Single precision
   INTEGER, PARAMETER :: sp = KIND(1.0)

   !> Tolerance below which we consider a number to be zero
   REAL(wp), PARAMETER :: TOL_ZERO  = 1.0E-10

   !> Unit for stdout
   INTEGER, PARAMETER :: numout = 6

   !-------------------------------------------------------------------
   ! Parameters and types for the timing routines

   INTEGER :: iclk_rate ! Ticks per second of Fortran timer
   INTEGER :: iclk_max  ! Max value that Fortran timer can return
   REAL(wp) :: clock_tick_s ! Time in seconds between clock ticks

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
      !> The no. of times this timed region has been executed.
      INTEGER               :: count
      !> The no. of repeated intervals within this timed region.
      !! Used in timer_report() to produce a mean time per repeat.
      !! Default value is 1. User can specify value in call to
      !! timer_start().
      INTEGER               :: nrepeat
      !> Array to hold the individual time periods that we collect
      !! if producing a time-line
      REAL(KIND=sp), ALLOCATABLE :: time_series(:)
   END TYPE timer_type

   INTEGER, SAVE :: nThreads ! No. of OMP threads being used (1 if no OMP)
                             ! Set in init_time().

   TYPE(timer_type), ALLOCATABLE, SAVE, DIMENSION(:,:) :: timer

   !> The number of timed regions created for each thread
   INTEGER, ALLOCATABLE, SAVE, DIMENSION(:) :: itimerCount
   
   !-------------------------------------------------------------------
   ! Publicly-accessible routines

   PUBLIC timer_init, time_in_s, timer_start, timer_stop, timer_report

 CONTAINS

   !======================================================================

   SUBROUTINE timer_init()
      IMPLICIT none
      ! Set-up timing
      INTEGER :: ji, ith, ierr

! Check that init_time hasn't been called from within an OMP PARALLEL
! region.
!$      IF(omp_get_num_threads() > 1)THEN
!$OMP MASTER
!$         WRITE(*,"('init_time: ERROR: cannot be called from within OpenMP PARALLEL region.')")
!$OMP END MASTER
!$OMP BARRIER
!$         STOP
!$      END IF

      ! Initialise the timer structures
      select case(base_timer)

      case(OMP_TIMER)
         iclk_rate = 1
         iclk_max = 1
!$       clock_tick_s = omp_get_wtick()
         write (*,"('TIMING: using OpenMP omp_get_wtime()')")
         write (*,"('TIMING: time between clock ticks =',1E13.5,' (s)')") &
               clock_tick_s
      case(RDTSC_TIMER)
         iclk_rate = 1
         iclk_max = 1
         clock_tick_s = 1.0d0 ! TODO work out how to get this quantity
         write (*,"('TIMING: using Intel Time Stamp Counter register')")
      case(INTRINSIC_TIMER)
         call SYSTEM_CLOCK(COUNT_RATE=iclk_rate, COUNT_MAX=iclk_max)
         write (*,"('TIMING: using Fortran intrinsic system clock, cycles/sec =',I7, &
              &   ', max count = ',I11)") iclk_rate, iclk_max
         clock_tick_s = 1.0d0/REAL(iclk_rate)
      end select

      nThreads = 1
!$    nThreads = omp_get_max_threads()

      WRITE (*,"('TIMING: Allocating timer structures for ',I3,' threads.')") &
           nThreads

      ALLOCATE(timer(MAX_TIMERS,nThreads), itimerCount(nThreads), &
               Stat=ierr)

      IF(ierr /= 0)THEN
         WRITE (*,*) 'init_time: ERROR: failed to allocate timer structures'
         RETURN
      END IF

      ! Allocate memory required for recording time-series data
      IF(RECORD_TIME_SERIES)THEN
         DO ith = 1, nThreads, 1
            DO ji=1,MAX_TIMERS,1
               allocate(timer(ji,ith)%time_series(TIME_SERIES_LEN), &
                        Stat=ierr)
               if(ierr /= 0)then
                  write (*,*) 'init_time: ERROR: failed to allocate time-series array'
                  return
               end if
            END DO
         END DO
      END IF

!$OMP PARALLEL DO default(none), shared(nThreads,itimerCount,timer), &
!$OMP             private(ith, ji)
      DO ith = 1, nThreads, 1
         DO ji=1,MAX_TIMERS,1
            itimerCount(ith) = 0
            timer(ji,ith)%label  = ""
            timer(ji,ith)%istart = 0_int64
            timer(ji,ith)%total  = 0_wp
            timer(ji,ith)%count  = 0
         END DO
      END DO
!$OMP END PARALLEL DO

   END SUBROUTINE timer_init

!============================================================================

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

   SUBROUTINE timer_start(label, idx, nrepeat)
      USE intel_timer_mod
      IMPLICIT none
      CHARACTER (*), INTENT(in) :: label
      INTEGER, INTENT(out) :: idx
      !> The number of repeated intervals inside this timed region.
      !! Used to report a time per interval in the report generated
      !! by timer_report().
      INTEGER, INTENT(in), OPTIONAL :: nrepeat
      INTEGER :: ji, ith, iclk

      IF(LEN_TRIM(label) > LABEL_LEN)THEN
         WRITE(*,"('timer_start: ERROR: length of label >>',(A),'<< exceeds ',I2,' chars')") &
              TRIM(label), LABEL_LEN
         idx = -1
         RETURN
      END IF

      ith = 1
!$    ith = 1 + omp_get_thread_num()

      ! Search for existing timer
      DO ji=1,itimerCount(ith),1
         ! Shorter string is padded with blanks so that lengths match
         IF(timer(ji,ith)%label == label)EXIT 
      END DO

      IF( ji > itimerCount(ith) )THEN
         ! Create a new timer
         itimerCount(ith) = itimerCount(ith) + 1
         IF(itimerCount(ith) > MAX_TIMERS)THEN
            WRITE(*,"('timer_start: ERROR: max. no. of timers exceeded!')")
            WRITE(*,"('timer_start: ERROR: thread = ',I3,'label = ',(A))") &
                  ith, label
            idx = -1
            itimerCount(ith) = itimerCount(ith) - 1
            RETURN
         END IF

         ji = itimerCount(ith)

         ! Initialise this new timer structure
         timer(ji, ith)%label = TRIM(ADJUSTL(label))
         if(present(nrepeat))then
            timer(ji, ith)%nrepeat = nrepeat
         else
            ! No repeat specified so default to a value of unity.
            timer(ji, ith)%nrepeat = 1
         end if

      END IF

      ! Increment the count of no. of times we've used this timer
      timer(ji,ith)%count = timer(ji,ith)%count + 1

      ! Return integer tag
      idx = ji

      ! And finally record the current timer value
      select case(base_timer)
      case(OMP_TIMER)
! Requires that this file be compiled with OpenMP enabled
!$       timer(ji,ith)%istart = omp_get_wtime()         
      case(RDTSC_TIMER)
         timer(ji,ith)%istart = REAL(getticks(), wp)
      case(INTRINSIC_TIMER)
         CALL SYSTEM_CLOCK(iclk)
         timer(ji,ith)%istart = REAL(iclk, wp)
      end select

   END SUBROUTINE timer_start

!============================================================================

   SUBROUTINE timer_stop(itag)
      IMPLICIT none
      INTEGER, INTENT(in) :: itag ! Flag identifying the timer
      ! Stop the specified timer and record the elapsed number of ticks
      ! since it was started.
      INTEGER :: iclk, ith
      INTEGER (kind=int64) :: iclk64
      REAL(wp) :: time_now, delta_t

      select case(base_timer)
      case(OMP_TIMER)
!$       time_now = omp_get_wtime()
      case(RDTSC_TIMER)
         iclk64 = getticks()
      case(INTRINSIC_TIMER)
         CALL SYSTEM_CLOCK(iclk)
         iclk64 = INT(iclk, int64)
      end select

      IF(itag < 1)RETURN

      ith = 1
!$    ith = 1 + omp_get_thread_num()

      select case(base_timer)
      case(OMP_TIMER)
         delta_t = time_now - timer(itag,ith)%istart
         timer(itag,ith)%total = timer(itag,ith)%total + delta_t             
         ! If we're recording a time-series then store this result. Currently
         ! only implemented for the OpenMP timer as awaiting 'time_now()'
         ! routine being developed in MPI branch.
         if(record_time_series .AND. &
            timer(itag,ith)%count < TIME_SERIES_LEN)then
            timer(itag,ith)%time_series(timer(itag,ith)%count) = REAL(delta_t, kind=sp)
         end if
      case(RDTSC_TIMER)
         timer(itag,ith)%total = timer(itag,ith)%total + &
                          (REAL(iclk64,wp) - timer(itag,ith)%istart)
      case(INTRINSIC_TIMER)
         IF( iclk < timer(itag,ith)%istart )THEN
            iclk64 = iclk64 + INT(iclk_max,int64)
         END IF

         timer(itag,ith)%total = timer(itag,ith)%total + &
                          (REAL(iclk64,wp) - timer(itag,ith)%istart)
      end select

   END SUBROUTINE timer_stop

   !==========================================================================

   SUBROUTINE timer_report()
     use dl_timer_parallel, only: is_parallel, get_rank
     implicit none
     integer       :: jt, itimer, rank
     integer       :: ierr
     logical       :: have_repeats
     ! Arrays used to gather stats for each timed region when running
     ! MPI parallel
     real(wp), allocatable, dimension(:,:,:) :: times, max_times, &
                                                min_times, sum_times
     character(len=120) :: timer_str = ""

     if( is_parallel() )then
        ! If this is a parallel run then, for each timed region, we want
        ! the minimum, maximum and sum (over all processes) of the time
        ! spent inside it.
        allocate(times(2,MAX_TIMERS,nThreads), &
                 max_times(2,MAX_TIMERS,nThreads), &
                 min_times(2,MAX_TIMERS,nThreads), &
                 sum_times(2,MAX_TIMERS,nThreads), Stat=ierr)
        if(ierr /= 0)then
           write(*,"('Timer report: failed to allocate memory to gather ', &
                   & 'MPI stats: no timing report generated')")
           return
        end if

        ! We must pack the timing data into an array suitable for the
        ! reduction operations
        do jt = 1, nThreads, 1
           do itimer = 1, itimerCount(jt)
              times(1,itimer,jt) = timer(itimer,jt)%total
              times(2,itimer,jt) = get_rank()
           end do
        end do

        ! Call the reduction operations here ARPDBG
     end if

     select case(base_timer)
     case(OMP_TIMER)
        write(timer_str, &
             "('Timed using OpenMP omp_get_wtime. Units are seconds.')")
     case(RDTSC_TIMER)
        write(timer_str, &
             "('Timed using Intel Time Stamp Counter. Units are counts.')")
     case(INTRINSIC_TIMER)
        write(timer_str, &
             "('Timed using Fortran SYSTEM_CLOCK intrinsic. Units are seconds.')")
     case default
        return
     end select

     ! Check whether any of our timed regions have a non-unity
     ! no. of repeats.
     have_repeats = .FALSE.
     do jt = 1, nThreads, 1
        if( ANY( timer(1:itimerCount(jt),jt)%nrepeat > 1) )then
           have_repeats = .TRUE.
           exit
        end if
     end do

     if(have_repeats)then
        call timer_report_with_repeats(timer_str)
     else
        call timer_report_no_repeats(timer_str)
     end if

      if(RECORD_TIME_SERIES) call output_time_series()

    end subroutine timer_report

   !==========================================================================

    subroutine timer_report_no_repeats(timer_str)
      use dl_timer_parallel, only: get_rank
      implicit none
      CHARACTER(len=*), INTENT(in) :: timer_str
      INTEGER       :: ji, jt
      REAL(KIND=wp) :: wtime
      integer       :: rank

      rank = get_rank()
      if(rank == 0)then

         WRITE(*,"(/22('='),' Timing report ',22('='))")
         WRITE(*,"(4x, (A))") TRIM(timer_str)
         WRITE(*,"(67('-'))")
         WRITE(*,"('Region',26x,'Counts',6x,'Total',9x,'Average',6x,'Error')")
         WRITE(*,"(67('-'))")
         DO jt = 1, nThreads, 1

            IF(itimerCount(jt) > 0)THEN
               if(jt > 1) WRITE(*, "(34('- '))")
               WRITE(*," ('Thread ',I3)") jt-1
            end if

            DO ji=1,itimerCount(jt),1

               IF(use_rdtsc_timer)THEN
                  wtime = timer(ji,jt)%total
               ELSE
                  wtime = time_in_s(0._wp,timer(ji,jt)%total)
               END IF

               ! Truncate the label to 32 chars for table-formatting purposes
               WRITE(*,"((A),1x,I5,1x,E13.6,2x,E13.6,1x,E13.6)") &
                            timer(ji,jt)%label(1:32), timer(ji,jt)%count, &
                            wtime, wtime/REAL(timer(ji,jt)%count), &
                            time_err(timer(ji,jt)%count)
            END DO
         END DO
         WRITE(*," (67('='))")
      end if
   END SUBROUTINE timer_report_no_repeats

   !==========================================================================

   subroutine timer_report_with_repeats(timer_str)
     use dl_timer_parallel, only: get_rank
     !> Write the timing report for the case where one or more regions have an
     !! implicit repeat > 1.
     IMPLICIT none
     CHARACTER(len=*), INTENT(in) :: timer_str
     INTEGER       :: ji, jt
     REAL(KIND=wp) :: wtime, tmean, trepeat
     integer :: rank

     rank = get_rank()
     if(rank == 0)then

        write(*,"(/34('='),' Timing report ',34('='))")
        write(*,"(4x,(A))") TRIM(timer_str)
        write(*,"(83('-'))")
        write(*,"('Region',26x,'Counts',6x,'Total',9x,' Average    Average/repeat   Error')")
        write(*,"(83('-'))")
        do jt = 1, nThreads, 1

           if(itimerCount(jt) > 0)then
              if(jt > 1) WRITE(*, "(34('- '))")
              WRITE(*," ('Thread ',I3)") jt-1
           end if

           do ji=1,itimerCount(jt),1

              if(use_rdtsc_timer)then
                 wtime = timer(ji,jt)%total
              else
                 wtime = time_in_s(0._wp,timer(ji,jt)%total)
              end if

              ! Mean time spent in timed region
              tmean = wtime/REAL(timer(ji,jt)%count)
              ! Mean time spent in the repeated section of code in the
              ! timed region
              trepeat = tmean/REAL(timer(ji,jt)%nrepeat)
              ! Truncate the label to 32 chars for table-formatting purposes
              write(*,"((A),1x,I6,1x,E13.6,1x,E13.6,1x,E13.6,1x,E13.6)")  &
                   timer(ji,jt)%label(1:32), timer(ji,jt)%count,          &
                   wtime, tmean, trepeat,                                 & 
                   ! Error estimate using quadrature formula for
                   ! the time spent in just one of the nrepeat 
                   ! regions - use product formula
                   trepeat*time_err(timer(ji,jt)%count)/tmean
           end do
        end do
        write(*,"(83('='))")
     end if
   END SUBROUTINE timer_report_with_repeats

   !===================================================================

   SUBROUTINE output_time_series()
     implicit none
     !> For each thread and each timed region output the time-series
     !! data that we've collected
     integer :: ith, ji, thr_num, ierr
     !> Unit no. used to create each file
     integer, parameter :: funit=72
     !> The name of the file to create
     character(len=128) :: fname
     !> String representation of current thread idx + 10000
     character(len=5)   :: thr_idx_str

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

   FUNCTION time_err(ncount)
     implicit none
     integer :: ncount
     real(wp) :: time_err
     ! Calculate the error in the mean time duration using the reported
     ! granularity of the clock and quadrature formula
     ! Every time we time a region we get a start time and a stop time.
     ! There is an error of +/-clock_tick_s in each of these so the error
     ! in the measured duration is sqrt(2*clock_tick_s^2). We then have
     ! ncount measured durations so the error in the mean duration is:
     time_err = clock_tick_s*sqrt(2.0d0/REAL(ncount))
   END FUNCTION time_err

!============================================================================

END MODULE dl_timer
