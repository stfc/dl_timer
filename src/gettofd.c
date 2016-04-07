/* A gettimeofday routine to give access to the wall
   clock timer on most UNIX-like systems. */

#include <sys/time.h>

double time_of_day()
{
/* struct timeval { long tv_sec;
                    long tv_usec;        };

   struct timezone { int tz_minuteswest;
                     int tz_dsttime;      };     */
  struct timeval tp;
  struct timezone tzp;
  int i;

  i = gettimeofday(&tp,&tzp);
  return ( (double) tp.tv_sec + (double) tp.tv_usec * 1.e-6 );
}

