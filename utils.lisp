;;; -*- Mode: Lisp;  show-trailing-whitespace: t; Base: 10; indent-tabs: nil; Syntax: ANSI-Common-Lisp; Package: UTILS; -*-
;;; Copyright (c) 2013, Mark VandenBrink. All rights reserved.
(in-package #:utils)

(eval-when (:compile-toplevel :load-toplevel :execute)
  #+DBG (defvar *standard-optimize-settings* '(optimize (debug 3)))
  #-DBG (defvar *standard-optimize-settings* '(optimize (speed 3) (safety 0) (space 0) (debug 0)))
  )

(defparameter *break-on-warn-user* nil "set to T if you'd like to stop in warn-user")

(defun warn-user (format-string &rest args)
  "Print a warning error to *ERROR-OUTPUT* and continue"
  (declare #.utils:*standard-optimize-settings*)
  (when *break-on-warn-user*
    (break "Breaking in WARN-USER"))
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

(declaim (inline upto-null))
(defun upto-null (string)
  "Trim STRING to end at first NULL found"
  (declare #.utils:*standard-optimize-settings*)
  (subseq string 0 (position #\Null string)))

(defun dump-data (file-name data)
  (declare #.utils:*standard-optimize-settings*)
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

;;; in multi-thread mode, need to protect insertions into hash-table
;;; Note: CCL hash-tables are thread-safe, but some other implementations
;;; don't appear to be...
(defstruct locked-hash-table lock hash-table)
#+(or :ccl :sbcl :abcl)
(defmacro with-lock ((l) &body body)
  `(bt:with-lock-held (,l)
     ,@body))

#-(or :ccl :sbcl :abcl)
(defmacro with-lock ((l) &body body)
  (declare (ignore l))
  `(progn
     ,@body))

(defun mk-memoize (func-name)
  "Takes a normal function object and returns a memoized one"
  (declare #.utils:*standard-optimize-settings*)
  (let* ((func (symbol-function func-name))
         (the-hash-table (make-locked-hash-table
                          :lock #+ENABLE-MP (bt:make-lock) #-ENABLE-MP nil
                          :hash-table (make-hash-table :test 'equal))))

    (with-slots (lock hash-table) the-hash-table
      #'(lambda (arg)
          (multiple-value-bind (value foundp) (gethash arg hash-table)
            (if foundp
                value
                (with-lock (lock)
                  (setf (gethash arg hash-table) (funcall func arg)))))))))

(defmacro memoize (func-name)
  "Memoize function associated with FUNC-NAME. Simplified version"
  `(setf (symbol-function ,func-name) (utils::mk-memoize ,func-name)))

(defun timings (function)
  (declare #.utils:*standard-optimize-settings*)
  (let ((real-base (get-internal-real-time)))
    (funcall function)
    (float (/ (- (get-internal-real-time) real-base) internal-time-units-per-second))))

;;; Taken from ASDF
(defmacro DBG (tag &rest exprs)
  "debug macro for print-debugging:
TAG is typically a constant string or keyword to identify who is printing,
but can be an arbitrary expression returning a tag to be princ'ed first;
if the expression returns NIL, nothing is printed.
EXPRS are expressions, which when the TAG was not NIL are evaluated in order,
with their source code then their return values being printed each time.
The last expression is *always* evaluated and its multiple values are returned,
but its source and return values are only printed if TAG was not NIL;
previous expressions are not evaluated at all if TAG returned NIL.
The macro expansion has relatively low overhead in space or time."
  (let* ((last-expr (car (last exprs)))
         (other-exprs (butlast exprs))
         (tag-var (gensym "TAG"))
         (thunk-var (gensym "THUNK")))
    `(let ((,tag-var ,tag))
       (flet ,(when exprs `((,thunk-var () ,last-expr)))
         (if ,tag-var
             (DBG-helper ,tag-var
                         (list ,@(loop :for x :in other-exprs :collect
                                       `(cons ',x #'(lambda () ,x))))
                         ',last-expr ,(if exprs `#',thunk-var nil))
             ,(if exprs `(,thunk-var) '(values)))))))

(defun DBG-helper (tag expressions-thunks last-expression last-thunk)
  ;; Helper for the above debugging macro
  (declare #.utils:*standard-optimize-settings*)
  (labels
      ((f (stream fmt &rest args)
         (with-standard-io-syntax
           (let ((*print-readably* nil)
                 (*package* (find-package :cl)))
             (apply 'format stream fmt args)
             (finish-output stream))))
       (z (stream)
         (f stream "~&"))
       (e (fmt arg)
         (f *error-output* fmt arg))
       (x (expression thunk)
         (e "~&  ~S => " expression)
         (let ((results (multiple-value-list (funcall thunk))))
           (e "~{~S~^ ~}~%" results)
           (apply 'values results))))
    (map () #'z (list *standard-output* *error-output* *trace-output*))
    (e "~A~%" tag)
    (loop :for (expression . thunk) :in expressions-thunks
          :do (x expression thunk))
    (if last-thunk
        (x last-expression last-thunk)
        (values))))
