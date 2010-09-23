#+debug   (declaim (optimize (debug 3) (safety 0) (speed 0)))
#+release (declaim (optimize (debug 0) (safety 3) (speed 3)))

(with-output-to-string (*standard-output*)
  (require :sb-posix))

(defpackage #:temp-package (:use #:cl))
(in-package #:temp-package)

(defun as-dir (path &aux len)
  (setq path (princ-to-string path))
  (setq len (length path))
  (when (zerop len) (return-from as-dir path))
  (if (char= #\/
	     (elt path (1- len)))
      path
      (concatenate 'string path "/")))

(defvar *start-path* (as-dir (sb-posix:getenv "START_PATH")))

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

(defvar *lisp-libs-dir* 
      (second sb-ext:*posix-argv*))
(relative-to-absolute *lisp-libs-dir*)

(defvar *compiled-libs*
      (third sb-ext:*posix-argv*))
(relative-to-absolute *compiled-libs*)

(defvar *asdf-systems*
  (mapcar #'read-from-string 
	  (rest (rest (rest sb-ext:*posix-argv*)))))

(asdf:initialize-source-registry 
 `(:source-registry
   (:tree ,(as-dir *lisp-libs-dir*))
   :inherit-configuration))

(asdf:initialize-output-translations 
 `(:output-translations 
   (T (,(as-dir *compiled-libs*)
       :implementation)) 
   :inherit-configuration))

(in-package :cl-user)

(loop for system in temp-package::*asdf-systems*
   do (asdf:load-system system))

(delete-package '#:TEMP-PACKAGE)

(sb-ext:save-lisp-and-die
 "lisp/lib/sbcl/dynserv.core")
