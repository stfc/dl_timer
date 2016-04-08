#define _POSIX_C_SOURCE 500

#include <sys/time.h>
#include <stdio.h>
/*
struct timespec {
        __kernel_time_t tv_sec;                 * seconds *
        long            tv_nsec;                * nanoseconds *
}; */
double posix_clock_init(void){
  int ierr;
  struct timespec ts;
  double res;

#ifdef CLOCK_MONOTONIC
  //ierr = clock_getres(clockid_t clock_id, struct timespec *res);
  ierr = clock_getres(CLOCK_MONOTONIC, &ts);
  res = (double)(ts.tv_sec) + 1.0e-9*ts.tv_nsec;
#else
  fprintf(stderr, "TIMING: POSIX monotonic clock not available.\n");
  res = 0.0;
#endif
  return res;
}

double posix_clock(void){
  int ierr;
  struct timespec ts;
  double tnow;

#ifdef CLOCK_MONOTONIC
  ierr = clock_gettime(CLOCK_MONOTONIC, &ts);
  tnow = (double)(ts.tv_sec) + 1.0e-9*ts.tv_nsec;
#else
  tnow = 0.0;
#endif

  return tnow;
}
