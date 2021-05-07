#include <fcntl.h>
#include <unistd.h>
#include <sys/stat.h>

int unliftio_o_tmpfile( void )
{
#ifdef __O_TMPFILE
  return __O_TMPFILE;
#else
  return 0;
#endif
}

#if OLD_GLIBC
#else
int unliftio_at_fdcwd( void )
{
  return AT_FDCWD;
}

int unliftio_at_symlink_follow( void )
{
  return AT_SYMLINK_FOLLOW;
}
#endif

int unliftio_s_irusr( void )
{
  return S_IRUSR;
}

int unliftio_s_iwusr( void )
{
  return S_IWUSR;
}

