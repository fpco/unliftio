/*
 * Windows has the most amazingly cretinous time measurement APIs you
 * can possibly imagine.
 *
 * Our first possibility is GetSystemTimeAsFileTime, which updates at
 * roughly 60Hz, and is hence worthless - we'd have to run a
 * computation for tens or hundreds of seconds to get a trustworthy
 * number.
 *
 * Alternatively, we can use QueryPerformanceCounter, which has
 * undefined behaviour under almost all interesting circumstances
 * (e.g. multicore systems, CPU frequency changes). But at least it
 * increments reasonably often.
 */

#include <windows.h>

static double freq_recip;
static LARGE_INTEGER firstClock;

void unliftio_inittime(void)
{
    LARGE_INTEGER freq;

    if (freq_recip == 0) {
	QueryPerformanceFrequency(&freq);
	QueryPerformanceCounter(&firstClock);
	freq_recip = 1.0 / freq.QuadPart;
    }
}

double unliftio_gettime(void)
{
    LARGE_INTEGER li;

    QueryPerformanceCounter(&li);

    return ((double) (li.QuadPart - firstClock.QuadPart)) * freq_recip;
}
