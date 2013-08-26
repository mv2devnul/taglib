;;; -*- Mode: Lisp;  show-trailing-whitespace: t; Base: 10; indent-tabs: nil; Syntax: ANSI-Common-Lisp; Package: UTILS; -*-
;;; Copyright (c) 2013, Mark VandenBrink. All rights reserved.
(in-package #:utils)

(defparameter *break-on-warn-user* nil "set to T if you'd like to stop in warn user")

;;; COMPLETELY UNPORTABLE!!!
(defun warn-user (format-string &rest args)
  "print a warning error to *ERROR-OUTPUT* and continue"
  (when *break-on-warn-user* (break "Breaking in WARN-USER"))
  (format *error-output* "~&********************************************************************************~%")
  (format *error-output* "~&WARNING in ~a:: " (ccl::%last-fn-on-stack 1))
  (apply #'format *error-output* format-string args)
  (format *error-output* "~&**********************************************************************************~%"))

(defparameter *max-raw-bytes-print-len* 10 "Max number of octets to print from an array")

(defun printable-array (array &optional (max-len *max-raw-bytes-print-len*))
  "Given an array, return a string of the first *MAX-RAW-BYTES-PRINT-LEN* bytes"
  (let* ((len (length array))
         (print-len (min len max-len))
         (printable-array (make-array print-len :displaced-to array)))
    (format nil "[~:d of ~:d bytes] <~x>" print-len len printable-array)))

(defun upto-null (string)
  "Trim STRING to end at first NULL found"
  (subseq string 0 (position #\Null string)))

(defun dump-data (file-name data)
  (with-open-file (f file-name :direction :output :if-exists :supersede :element-type '(unsigned-byte 8))
    (write-sequence data f)))

(defmethod has-extension ((n string) ext)
  "Probably should use CL's PATHNAME methods, but simply looking at the .XXX portion of a filename
to see if it matches. This is the string version that makes a PATHNAME and calls the PATHNAME version."
  (has-extension (parse-namestring n) ext))

(defmethod has-extension ((p pathname) ext)
  "Probably should use CL's PATHNAME methods , but simply looking at the .XXX portion of a filename
to see if it matches. PATHNAME version."
  (let ((e (pathname-type p)))
    (if e
      (string= (string-downcase e) (string-downcase ext))
      nil)))

(defmacro redirect (filename &rest body)
  "Temporarily set *STANDARD-OUTPUT* to FILENAME and execute BODY."
  `(let ((*standard-output* (open ,filename :direction :output :if-does-not-exist :create :if-exists :supersede)))
     ,@body
     (finish-output *standard-output*)))
