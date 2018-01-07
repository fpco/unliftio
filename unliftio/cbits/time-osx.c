#include <mach/mach.h>
#include <mach/mach_time.h>

static mach_timebase_info_data_t timebase_info;
static double timebase_recip;

void unliftio_inittime(void)
{
    if (timebase_recip == 0) {
	mach_timebase_info(&timebase_info);
	timebase_recip = (timebase_info.denom / timebase_info.numer) / 1e9;
    }
}

double unliftio_gettime(void)
{
    return mach_absolute_time() * timebase_recip;
}
