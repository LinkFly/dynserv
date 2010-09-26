;;;; restas-daemon.lisp
;;;;
;;;; Usage:
;;;; sbcl --noinform --no-userinit --no-sysinit --load /path/to/restas-daemon.lisp /path/to/daemon.conf COMMAND
;;;; where COMMAND one of: start stop zap kill restart nodaemon
;;;;
;;;; If successful, the exit code is 0, otherwise 1
;;;;
;;;; Error messages look in /var/log/messages (usually, depend on syslog configuration)
;;;;
;;;; This file is part of the RESTAS library, released under Lisp-LGPL.
;;;; See file COPYING for details.
;;;;
;;;; Main Author: Moskvitin Andrey <archimag@gmail.com>
;;;; After of change: Katrevich Sergey <linkfly1@newmail.ru>
#+debug   (declaim (optimize (debug 3) (safety 3) (speed 0)))
#+release (declaim (optimize (debug 0) (safety 3) (speed 3)))

(with-output-to-string (*standard-output*)
  (require :sb-posix))

(defpackage #:sbcl.daemon
  (:use #:cl #:sb-alien #:sb-ext #:sb-posix)
  (:shadowing-import-from #:cl #:close #:time #:ftruncate #:truncate #:open)
  (:import-from #:sb-unix #:tiocnotty))

(in-package #:sbcl.daemon)

(defun log-error (format &rest args)
  (apply #'sb-posix:syslog sb-posix:log-err format args))

(defun log-info (format &rest args)
  (apply #'sb-posix:syslog sb-posix:log-info format args))

(defun as-dir (path &aux len)
  (setq len (length path))
  (when (zerop len) (return-from as-dir path))
  (if (char= #\/
	     (elt path (1- len)))
      path
      (concatenate 'string path "/")))

(defvar *start-path* (as-dir (sb-posix:getenv "START_PATH")))
(defvar *daemon-config-pathname* (second *posix-argv*))
(defvar *daemon-command* (third *posix-argv*))

(defparameter *as-daemon* (not (string= *daemon-command* "nodaemon")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; WARNING!
;;;; plantform-depends constant :(
;;;; changes for you platform... or make path for sbcl ;)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(unless (boundp 'sb-unix:tiocnotty)
  (defconstant sb-unix:tiocnotty 21538))

(defconstant +PR_SET_KEEPCAPS+ 8)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; aux
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro with-exit-on-error (&body body)
  `(if *as-daemon*
       (handler-case (progn ,@body)
         (error (err)
           (with-output-to-string (*standard-output*)
             (let ((*print-escape* nil))
               (print-object err *error-output*)
               (write #\Newline :stream *error-output*)
               (sb-ext:quit :unix-status 1)))))
       (progn ,@body)))

(defmacro with-silence (&body body)
  `(with-output-to-string (*trace-output*)
     (with-output-to-string (*standard-output*)
       ,@body)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; basic parameters
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defpackage #:sbcl.daemon.preferences
  (:use #:cl)
  (:export #:*name*
           #:*user*
           #:*group*
           #:*fasldir*
           #:*pidfile*
           #:*swankport*
           #:*default-host-redirect*
           #:*asdf-central-registry*
           #:*asdf-load-systems*
           #:*sites*
	   #:*source-registry*))

(with-exit-on-error
  (let ((*package* (find-package '#:sbcl.daemon.preferences)))
    (load *daemon-config-pathname*)))

(defmacro defpref (name &optional default)
  `(with-exit-on-error
       (defparameter ,name
	 (let ((symbol (find-symbol (symbol-name ',name) 
				    '#:sbcl.daemon.preferences)))
	   (if (boundp symbol)
	       (symbol-value symbol)
	       ,default)))))

(defmacro relative-to-absolute (path)  
  `(setf ,path (add-path-to-relative-path ,path)))

(defun relative-path-p (pathname)
  (not (eq :absolute (first (pathname-directory pathname)))))

(defun get-base-path ()
  (make-pathname :defaults *load-pathname*
		 :name nil
		 :type nil))

(defun fix-base-p (path)
  (setq path (princ-to-string path))
  (when (>= (length path) 2)
    (string= "./"
	     (subseq (princ-to-string path) 0 2))))      

(defun add-path-to-relative-path (path)
  (if (relative-path-p path)
      (apply #'merge-pathnames 
	     (if (fix-base-p path)
		 (list (subseq (princ-to-string path) 2) 
		       *start-path*)
		 (list path (get-base-path))))
      path))

(defpref *name* (error "The param *name* is unbound"))

(defpref *user* *name*)

(defpref *group*)

(defpref *fasldir* (format nil "instances/~A/fasl/" *name*))
(relative-to-absolute *fasldir*)

(defpref *pidfile* (format nil "instances/~A/.pid" *name*))
(relative-to-absolute *pidfile*)

(defpref *swankport*)

(defpref *source-registry* "")
(relative-to-absolute *source-registry*)

(defpref *asdf-central-registry*)
(setf *asdf-central-registry* 
      (mapcar #'add-path-to-relative-path *asdf-central-registry*))

(defpref *asdf-load-systems*)

(defpref *sites*)

(defpref *default-host-redirect*)

;;; Else parameters ;;;
(defpref *change-user-p* t) 
;;;;;;;;;;;;;;;;;;;;;;;

(delete-package '#:sbcl.daemon.preferences)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Processing command line arguments
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; command-line COMMAND

;;;; quit if COMMAND is unknown

(unless (find *daemon-command* '("start" "stop" "zap" "kill" "restart" "nodaemon") :test #'string-equal)
  (with-exit-on-error
    (error "Bad command-line options")))

;;;; zap - remove pid file

(when (string-equal *daemon-command* "zap")
  (with-exit-on-error     
    (delete-file *pidfile*)
    (sb-ext:quit :unix-status 0)))

;;;; stop - send to daemon sigusr1 signal, wait and remove pid file

(with-silence
  (require 'sb-posix))

(defun read-pid ()
  (with-open-file (in *pidfile*)
    (read in)))

(defun stop-daemon ()
  (let ((pid (read-pid)))
    (sb-posix:kill pid sb-posix:sigusr1)
    (loop
       while (not (null (ignore-errors (sb-posix:kill pid 0))))
       do (sleep 0.1)))
  (delete-file *pidfile*))

(when (string-equal *daemon-command* "stop")
;  (sb-ps
  (with-exit-on-error 
    (stop-daemon)
    (sb-ext:quit :unix-status 0)))

;;;; kill - send to daemon kill signal and remove pid file

(when (string-equal *daemon-command* "kill")
  (with-exit-on-error
    (sb-posix:kill (read-pid)
                   sb-posix:sigkill)
    (delete-file *pidfile*)
    (sb-ext:quit :unix-status 0)))

;;;; restart daemon

(when (string-equal *daemon-command* "restart")
  (with-exit-on-error
    (stop-daemon)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; Start daemon!
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;; required path for sbcl :(
(sb-posix::define-call "grantpt" int minusp (fd sb-posix::file-descriptor))
(sb-posix::define-call "unlockpt" int minusp (fd sb-posix::file-descriptor))
(sb-posix::define-call "ptsname" c-string null (fd sb-posix::file-descriptor))
(sb-posix::define-call "initgroups" int minusp (user c-string) (group sb-posix::gid-t))

(defun switch-to-slave-pseudo-terminal (&optional (out #P"/dev/null") (err #P"/dev/null"))
  (flet ((c-bit-or (&rest args)
           (reduce #'(lambda (x y) (boole boole-ior x y))
                   args)))
    (let* ((fdm (sb-posix:open #P"/dev/ptmx" sb-posix:O-RDWR))
           (slavename (progn
                        (sb-posix:grantpt fdm)
                        (sb-posix:unlockpt fdm)
                        (sb-posix:ptsname fdm)))
           (fds (sb-posix:open slavename sb-posix:O-RDONLY))
           (out-fd (sb-posix:open out
                               (c-bit-or sb-posix:O-WRONLY sb-posix:O-CREAT sb-posix:O-TRUNC)
                               (c-bit-or sb-posix:S-IREAD sb-posix:S-IWRITE sb-posix:S-IROTH)))
           (err-fd (if (not (equal err out))
                       (sb-posix:open err
                                      (c-bit-or sb-posix:O-WRONLY sb-posix:O-CREAT sb-posix:O-TRUNC)
                                      (c-bit-or sb-posix:S-IREAD sb-posix:S-IWRITE sb-posix:S-IROTH))
                       (if out (sb-posix:dup out-fd)))))
      (sb-posix:dup2 fds 0)
      (sb-posix:dup2 out-fd 1)
      (sb-posix:dup2 err-fd 2))))

(defun change-user (name &optional group)
  (let ((gid)
        (uid))
    (when group
      (setf gid
            (sb-posix:group-gid (sb-posix:getgrnam group))))
    (let ((passwd (sb-posix:getpwnam name)))
      (unless passwd (error "Bad passwd struct. Bad user name? User name: ~A" name))
      (unless group
        (setf gid
              (sb-posix:passwd-gid passwd))
        (setf uid
              (sb-posix:passwd-uid passwd))))    
    (sb-posix:setresgid gid gid gid)
    (sb-posix:initgroups name gid)
    (sb-posix:setresuid uid uid uid)))

(defvar *status* nil)

(defun signal-handler (sig info context)
  (declare (ignore info context))
  (setf *status* sig))

(when *as-daemon*
  (sb-sys:enable-interrupt sb-posix:sigusr1 #'signal-handler)
  (sb-sys:enable-interrupt sb-posix:sigchld #'signal-handler))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; change uid and gid
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; required for start hunchentoot on port 80
(sb-posix::define-call "prctl" int minusp (option int) (arg int))
(sb-posix:prctl +PR_SET_KEEPCAPS+ 1)

(with-exit-on-error 
  (if *change-user-p* (change-user *user* *group*)))
      

;;;; required for start hunchentoot on port 80
(load-shared-object (find-if #'probe-file
                             '("/lib/libcap.so.2" "/lib/libcap.so")))

(sb-posix::define-call "cap_from_text" (* char) null-alien (text c-string))
(sb-posix::define-call "cap_set_proc" int minusp (cap_p (* char)))
(sb-posix::define-call "cap_free" int minusp (cap_p (* char)))

(when (string= "root"
	       (sb-posix:passwd-name
		(sb-posix:getpwuid (sb-posix:getuid))))       
  (let ((cap_p (sb-posix:cap-from-text "CAP_NET_BIND_SERVICE=ep")))
    (sb-posix:cap-set-proc cap_p)
    (sb-posix:cap-free cap_p)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; fork!
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(when *as-daemon*
  (unless (= (sb-posix:fork) 0)
    (loop
       while (null *status*)
       do (sleep 0.1))
    (quit :unix-status (if (= *status* sb-posix:sigusr1)
                           0
                           1))))


(defparameter *ppid* (sb-posix:getppid))

;;;; set global error handler 
(defun global-error-handler (condition x)
  (declare (ignore x))
  (let ((err (with-output-to-string (out)
                     (let ((*print-escape* nil))
                       (print-object condition out)))))
    (print err *error-output*)
    (log-error err))
  (quit :unix-status 1))

(when *as-daemon*
  (setf *debugger-hook* #'global-error-handler)

  (sb-sys:enable-interrupt sb-posix:sigusr1 :default)
  (sb-sys:enable-interrupt sb-posix:sigchld :default))


;;;; change current directory
(sb-posix:chdir #P"/")

;;;; umask
(sb-posix:umask 0)


;;;; detach from tty
(when *as-daemon*
  (let ((fd (ignore-errors (sb-posix:open #P"/dev/tty" sb-posix:O-RDWR))))
    (when fd
      (sb-posix:ioctl fd sb-unix:tiocnotty)
      (sb-posix:close fd))))

;;;; rebind standart input, output and error streams
(when *as-daemon*
  (switch-to-slave-pseudo-terminal))

;;;; start new session
(when *as-daemon*
  (sb-posix:setsid))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; load asdf
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'asdf)

(asdf:initialize-source-registry 
 `(:source-registry
   (:tree ,*source-registry*) :inherit-configuration))
(asdf:initialize-output-translations 
 `(:output-translations 
   (T (,*fasldir* :implementation)) 
   :inherit-configuration))

(loop
   for path in *asdf-central-registry*
   do (push path asdf:*central-registry*))

;(asdf:oos 'asdf:load-op 'asdf-binary-locations)

;(setf asdf:*centralize-lisp-binaries* t)

;(setf asdf:*default-toplevel-directory* *fasldir*)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; start swank server
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(when *swankport*
  (asdf:oos 'asdf:load-op :swank)
  (let ((*package* (find-package :swank)))
    (setf (symbol-value 
	   (find-symbol "*USE-DEDICATED-OUTPUT-STREAM*" :swank))
	  nil)
    (funcall (find-symbol "CREATE-SERVER" :swank)
	     :port *swankport*
	     :coding-system "utf-8-unix"
	     :dont-close t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Start restas server
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(loop
   for system in *asdf-load-systems*
   do (asdf:operate 'asdf:load-op system))

(when *sites*
  (asdf:operate 'asdf:load-op '#:restas)
  (let ((*package* (find-package :restas)))
    (setf (symbol-value (read-from-string "restas:*default-host-redirect*"))
	  *default-host-redirect*)
    (loop
       for site in *sites*
       do (if (consp site)
	      (funcall (find-symbol "START" :restas)
		       (first site)
		       :hostname (second site)
		       :port (third site))
	      (funcall (find-symbol "START" :restas)
		       site)))))

(when *as-daemon*
  (sb-sys:enable-interrupt sb-posix:sigusr1
                           #'(lambda (sig info context)                             
                               (declare (ignore sig info context))
                               (handler-case
                                   (progn 
                                     (log-info "Stop ~A daemon" *name*)
                                     (error "~A stop" *name*)
                                     )
                                 (error (err)
                                   (log-error (with-output-to-string (out)
						(let ((*print-escape* nil))
						  (print-object err out)))
                                                    )))
                               (sb-ext:quit :unix-status 0))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; end daemon initialize
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; write pid file
(when *as-daemon*
  (with-open-file (out *pidfile* :direction :output :if-exists :error :if-does-not-exist :create)
    (write (sb-posix:getpid) :stream out))

  (sb-posix:kill *ppid* sb-posix:sigusr1)
  (setf *debugger-hook* nil)

  (log-info "Start ~A daemon" *name*))


