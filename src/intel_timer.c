#include <stdio.h>
#include <stdint.h>

int rdtsc_available(void)
{
#if defined __INTEL_COMPILER
  return 1;
#else
  return 0;
#endif
}

/* Timer for use on Intel chips. Results are only for inter-comparison
   and not for conversion into some human measure of time.
   See http://en.wikipedia.org/wiki/Time_Stamp_Counter */
uint64_t getticks(void)
{
    uint32_t lo, hi;

    /* We can use the Intel intrinsic __rdtsc() when building with
       the Intel compiler so don't need the below */

    /* We cannot use "=A", since this would use %rax on x86_64 and 
       return only the lower 32bits of the TSC *
    __asm__ __volatile__ ("rdtsc" : "=a" (lo), "=d" (hi));
    return (uint64_t)hi << 32 | lo;*/

#if defined __INTEL_COMPILER
    return __rdtsc();
#else
    fprintf(stderr, "TIMING: ERROR: attempting to use RDTSC timer when "
	    "not compiled with the Intel compiler\n");
    return 0;
#endif
}
