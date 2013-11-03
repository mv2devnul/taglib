;;; -*- Mode: Lisp;  show-trailing-whitespace: t; Base: 10; indent-tabs: nil; Syntax: ANSI-Common-Lisp; Package: UTILS; -*-
;;; Copyright (c) 2013, Mark VandenBrink. All rights reserved.
(in-package #:utils)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (pushnew :INSTRUMENT-MEMOIZED *features*)
  (defvar *standard-optimize-settings* '(optimize (speed 3) (safety 0) (space 0) (debug 0))))

(defparameter *break-on-warn-user* nil "set to T if you'd like to stop in warn-user")

(defun warn-user (format-string &rest args)
  "Print a warning error to *ERROR-OUTPUT* and continue"
  (when *break-on-warn-user* (break "Breaking in WARN-USER"))
  (format *error-output* "~&********************************************************************************~%")
  #+CCL (format *error-output* "~&WARNING in ~a:: " (ccl::%last-fn-on-stack 1))
  (apply #'format *error-output* format-string args)
  (format *error-output* "~&**********************************************************************************~%"))

(defparameter *max-raw-bytes-print-len* 10 "Max number of octets to print from an array")

(defun printable-array (array &optional (max-len *max-raw-bytes-print-len*))
  "Given an array, return a string of the first *MAX-RAW-BYTES-PRINT-LEN* bytes"
  (declare #.utils:*standard-optimize-settings*)
  (let* ((len (length array))
         (print-len (min len max-len))
         (printable-array (make-array print-len :displaced-to array)))
    (declare (fixnum max-len len)
             (type (array (unsigned-byte 8) 1) array))
    (format nil "[~:d of ~:d bytes] <~x>" print-len len printable-array)))

(defmacro upto-null (string)
  "Trim STRING to end at first NULL found"
  `(subseq ,string 0 (position #\Null ,string)))

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
  (declare #.utils:*standard-optimize-settings*)
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

#+INSTRUMENT-MEMOIZED (progn
                        (defstruct memoized-funcs
                          name
                          table
                          calls
                          finds
                          news)
                        (defvar *memoized-funcs* nil))

(defun mk-memoize (func-name)
  "Takes a normal function object and returns a memoized one"
  (let* ((func (symbol-function func-name))
         (hash-table (make-hash-table :test 'equal))
          #+INSTRUMENT-MEMOIZED (s (make-memoized-funcs :table hash-table :calls 0 :finds 0 :news 0 :name func-name))
         )

    #+INSTRUMENT-MEMOIZED (push s *memoized-funcs*)

    #'(lambda (arg)
        (multiple-value-bind (value foundp) (gethash arg hash-table)
          #+INSTRUMENT-MEMOIZED (incf (memoized-funcs-calls s))
          (if foundp
              (progn
                #+INSTRUMENT-MEMOIZED (incf (memoized-funcs-finds s))
                value)
              (progn
                #+INSTRUMENT-MEMOIZED (incf (memoized-funcs-news s))
                (setf (gethash arg hash-table) (funcall func arg))))))))

(defmacro memoize (func-name)
  "Memoize function associated with Function-Name. Simplified version"
  `(setf (symbol-function ,func-name) (utils::mk-memoize ,func-name)))

(defun timings (function)
  (let ((real-base (get-internal-real-time)))
    (funcall function)
    (float (/ (- (get-internal-real-time) real-base) internal-time-units-per-second))))
