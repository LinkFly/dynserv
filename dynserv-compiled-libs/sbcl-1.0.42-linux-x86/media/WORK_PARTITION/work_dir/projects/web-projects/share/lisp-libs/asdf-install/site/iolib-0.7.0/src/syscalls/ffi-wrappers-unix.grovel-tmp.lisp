;;;; This file was automatically generated by cffi-grovel.
;;;; Do not edit by hand.

(CFFI:LOAD-FOREIGN-LIBRARY
 #P"/media/WORK_PARTITION/work_dir/projects/web-projects/dynserv/dynserv-compiled-libs/sbcl-1.0.42-linux-x86/media/WORK_PARTITION/work_dir/projects/web-projects/share/lisp-libs/asdf-install/site/iolib-0.7.0/src/syscalls/ffi-wrappers-unix.so")

(IN-PACKAGE :IOLIB.SYSCALLS) 
(DECLAIM (INLINE ERRNO %SET-ERRNO)) 
(DEFCFUN ("iolib_get_errno_cffi_wrap" ERRNO :CALLING-CONVENTION :CDECL :LIBRARY
          :DEFAULT)
    :INT) 
(DEFCFUN ("iolib_set_errno_cffi_wrap" %SET-ERRNO :CALLING-CONVENTION :CDECL
          :LIBRARY :DEFAULT)
    :INT
  (VALUE :INT)) 
(DECLAIM
 (INLINE WIFEXITED WEXITSTATUS WTERMSIG WCOREDUMP WIFSTOPPED WSTOPSIG
  WIFCONTINUED)) 
(DEFCFUN ("WIFEXITED_cffi_wrap" WIFEXITED :CALLING-CONVENTION :CDECL :LIBRARY
          :DEFAULT)
    :INT
  (STATUS :INT)) 
(DEFCFUN ("WEXITSTATUS_cffi_wrap" WEXITSTATUS :CALLING-CONVENTION :CDECL
          :LIBRARY :DEFAULT)
    :INT
  (STATUS :INT)) 
(DEFCFUN ("WIFSIGNALED_cffi_wrap" WIFSIGNALED :CALLING-CONVENTION :CDECL
          :LIBRARY :DEFAULT)
    :INT
  (STATUS :INT)) 
(DEFCFUN ("WTERMSIG_cffi_wrap" WTERMSIG :CALLING-CONVENTION :CDECL :LIBRARY
          :DEFAULT)
    :INT
  (STATUS :INT)) 
(DEFCFUN ("iolib_wcoredump_cffi_wrap" WCOREDUMP :CALLING-CONVENTION :CDECL
          :LIBRARY :DEFAULT)
    :INT
  (STATUS :INT)) 
(DEFCFUN ("WIFSTOPPED_cffi_wrap" WIFSTOPPED :CALLING-CONVENTION :CDECL :LIBRARY
          :DEFAULT)
    :INT
  (STATUS :INT)) 
(DEFCFUN ("WSTOPSIG_cffi_wrap" WSTOPSIG :CALLING-CONVENTION :CDECL :LIBRARY
          :DEFAULT)
    :INT
  (STATUS :INT)) 
(DEFCFUN ("WIFCONTINUED_cffi_wrap" WIFCONTINUED :CALLING-CONVENTION :CDECL
          :LIBRARY :DEFAULT)
    :INT
  (STATUS :INT)) 
(DECLAIM (INLINE CMSG.SPACE CMSG.LEN CMSG.FIRSTHDR CMSG.DATA)) 
(DEFCFUN ("CMSG_SPACE_cffi_wrap" CMSG.SPACE :CALLING-CONVENTION :CDECL :LIBRARY
          :DEFAULT)
    :UNSIGNED-INT
  (DATA-SIZE :UNSIGNED-INT)) 
(DEFCFUN ("CMSG_LEN_cffi_wrap" CMSG.LEN :CALLING-CONVENTION :CDECL :LIBRARY
          :DEFAULT)
    :UNSIGNED-INT
  (DATA-SIZE :UNSIGNED-INT)) 
(DEFCFUN ("CMSG_FIRSTHDR_cffi_wrap" CMSG.FIRSTHDR :CALLING-CONVENTION :CDECL
          :LIBRARY :DEFAULT)
    :POINTER
  (MSG :POINTER)) 
(DEFCFUN ("CMSG_DATA_cffi_wrap" CMSG.DATA :CALLING-CONVENTION :CDECL :LIBRARY
          :DEFAULT)
    :POINTER
  (CMSG :POINTER)) 
(DECLAIM (INLINE DIRFD)) 
(DEFCFUN ("dirfd_cffi_wrap" DIRFD :CALLING-CONVENTION :CDECL :LIBRARY :DEFAULT)
    :INT
  (DIRP :POINTER)) 
