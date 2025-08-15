/* From https://github.com/bos/criterion */

#include <mach/mach.h>
#include <time.h>

void unliftio_inittime(void)
{
}

double unliftio_gettime(void)
{
    return clock_gettime_nsec_np(CLOCK_UPTIME_RAW) / 1e9;
}
