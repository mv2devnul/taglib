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

(defmacro redirect (filename &rest body)
  "Temporarily set *STANDARD-OUTPUT* to FILENAME and execute BODY."
  `(let ((*standard-output* (open ,filename :direction :output :if-does-not-exist :create :if-exists :supersede)))
     ,@body
     (finish-output *standard-output*)))

(defun get-bitmask(start width)
  "Create a bit mask that begins at bit START (31 is MSB) and is WIDTH bits wide.
Example: (get-bitmask 31 11) -->> #xffe00000"
  (ash (- (ash 1 width) 1) (- (1+ start) width)))

(defmacro get-bitfield (int start width)
  "Extract WIDTH bits from INT starting at START
Example: (get-bitfield #xFFFBB240 31 11) -->> #x7ff.
The above will expand to (ash (logand #xFFFBB240 #xFFE00000) -21) at COMPILE time."
  `(ash (logand ,int ,(utils::get-bitmask start width)) ,(- ( - start width -1))))

;;;;;;;;;;;;;;;;;;;; convenience macros ;;;;;;;;;;;;;;;;;;;;
(defmacro with-gensyms (syms &body body)
  `(let ,(mapcar #'(lambda (s)
                     `(,s (gensym)))
                 syms)
     ,@body))

(defun make-keyword (name)
  (intern (string name) :keyword))

(defmacro while (test &body body)
  `(do ()
       ((not ,test))
     ,@body))

(defmacro aif (test-form then-form &optional else-form)
  `(let ((it ,test-form))
     (if it ,then-form ,else-form)))

(defmacro awhen (test-form &body body)
  `(aif ,test-form
        (progn ,@body)))

(defmacro fastest (&body body)
  `(locally (declare (optimize (speed 3) (safety 0) (debug 0)))
     ,@body))
