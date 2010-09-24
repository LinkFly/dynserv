/*
 * This file has been automatically generated by cffi-grovel.
 * Do not edit it by hand.
 */

#if defined(__linux__)
#undef _GNU_SOURCE
#define _XOPEN_SOURCE 600
#define _LARGEFILE_SOURCE
#define _LARGEFILE64_SOURCE
#define _FILE_OFFSET_BITS 64
#endif
#include <errno.h>
int iolib_get_errno_cffi_wrap()
{
  return errno;
}

int iolib_set_errno_cffi_wrap(int value)
{
  errno = value; return errno;
}

#include <sys/types.h>
#include <sys/wait.h>
int WIFEXITED_cffi_wrap(int status)
{
  return WIFEXITED(status);
}

int WEXITSTATUS_cffi_wrap(int status)
{
  return WEXITSTATUS(status);
}

int WIFSIGNALED_cffi_wrap(int status)
{
  return WIFSIGNALED(status);
}

int WTERMSIG_cffi_wrap(int status)
{
  return WTERMSIG(status);
}

int iolib_wcoredump_cffi_wrap(int status)
{
  
  #ifdef WCOREDUMP
  return WCOREDUMP(status);
  #else
  return 0;
  #endif

}

int WIFSTOPPED_cffi_wrap(int status)
{
  return WIFSTOPPED(status);
}

int WSTOPSIG_cffi_wrap(int status)
{
  return WSTOPSIG(status);
}

int WIFCONTINUED_cffi_wrap(int status)
{
  return WIFCONTINUED(status);
}

#include <stdlib.h>
#include <sys/socket.h>
unsigned int CMSG_SPACE_cffi_wrap(unsigned int data_size)
{
  return CMSG_SPACE(data_size);
}

unsigned int CMSG_LEN_cffi_wrap(unsigned int data_size)
{
  return CMSG_LEN(data_size);
}

void* CMSG_FIRSTHDR_cffi_wrap(struct msghdr* msg)
{
  return CMSG_FIRSTHDR(msg);
}

void* CMSG_DATA_cffi_wrap(struct cmsghdr* cmsg)
{
  return CMSG_DATA(cmsg);
}

#include <sys/types.h>
#include <dirent.h>
int dirfd_cffi_wrap(DIR* dirp)
{
  return dirfd(dirp);
}
