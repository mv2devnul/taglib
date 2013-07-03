;;; -*- Mode: Lisp;  show-trailing-whitespace: t; Base: 10; indent-tabs: nil; Syntax: ANSI-Common-Lisp; Package: BASE-FILE; -*-
;;; Copyright (c) 2013, Mark VandenBrink. All rights reserved.

(in-package #:base-file)

(log5:defcategory cat-log-base-file)
(defmacro log-base-file (&rest log-stuff) `(log5:log-for (cat-log-base-file) ,@log-stuff))

(deftype octet () '(unsigned-byte 8))
(defmacro make-octets (len) `(make-array ,len :element-type 'octet))

(defclass base-file ()
  ((filename  :accessor filename  :initarg :filename)
   (instream  :accessor instream  :initform nil)
   (endian    :accessor endian    :initarg :endian :initform nil)   ; controls endian-ness of read/writes
   (modified  :accessor modified  :initform nil)					; for when we implement writing tags
   (file-size :accessor file-size))
  (:documentation "Base class for all audio file types"))

(defmethod initialize-instance :after ((me base-file) &key read-only &allow-other-keys)
  (log5:with-context "base-file-initializer"
  (with-slots (instream filename file-size endian) me
	(setf instream (if read-only
					   (open filename :direction :input :element-type 'octet)
					   (open filename :direction :io :if-exists :overwrite :element-type 'octet)))
	(setf binary-types:*endian* endian)
	(setf file-size (file-length instream))
	(log-base-file "stream = ~a, name = ~a, size = ~:d~%endian = ~a"
				   instream filename file-size endian))))

(defmethod close-audio-file ((me base-file))
  "Close an open file"
  (with-slots (instream modified) me
	(when modified
	  (warn "at some point, should I add code to auto-write modified audio-files?")
	  (setf modified nil))
	(when instream
	  (close instream)
	  (setf instream nil))))

(defmethod seek ((me base-file) offset from)
  "C-library like seek function. from can be one of :current, :start, :end.
Returns the current offset into the stream"
  (assert (member from '(:current :start :end)) () "seek takes one of :current, :start, :end")
  (with-slots (instream file-size) me
	(ecase from
	  (:start (file-position instream offset))
	  (:current
	   (let ((current (file-position instream)))
		 (file-position instream (+ current offset))))
	  (:end
	   (file-position instream (- file-size offset))))))

(defmethod read-u8 ((me base-file))
  "read 1 byte from file"
  (multiple-value-bind (value size) (binary-types:read-binary 'u8 (slot-value me 'instream))
	(assert (= size 1) () "Expected to read 1 byte, got ~d instead" size)
	value))

(defmethod read-u16 ((me base-file))
  "read 2 bytes from file"
  (multiple-value-bind (value size) (binary-types:read-binary 'u16 (slot-value me 'instream))
	(assert (= size 2) () "Expected to read 2 bytes, got ~d instead" size)
	value))

;;; read 3-bytes
(binary-types:define-unsigned u24 3)

(defmethod read-u24 ((me base-file))
  "read 3 bytes from file"
  (multiple-value-bind (value size) (binary-types:read-binary 'u24 (slot-value me 'instream))
	(assert (= size 3) () "Expected to read 3 bytes, got ~d instead" size)
	value))

(defmethod read-u32 ((me base-file))
  "read 4 bytes from file"
  (multiple-value-bind (value size) (binary-types:read-binary 'u32 (slot-value me 'instream))
	(assert (= size 4) () "Expected to read 4 bytes, got ~d instead" size)
	value))

(defmethod read-string ((me base-file) &key size (terminators nil))
  "Read normal string from file. If size is provided, read exactly that many octets.
If terminators is supplied, it is a list of characters that can terminate a string (and hence stop read)"
  (multiple-value-bind (value read-size)
	  (binary-types:read-binary-string (slot-value me 'instream) :size size :terminators terminators)
	(declare (ignore read-size))
	;; what checks should happen here?
	value))

(defmethod read-octets ((me base-file) size)
  "Read SIZE octets from input-file"
  (let* ((octets (make-octets size))
		 (read-len (read-sequence octets (slot-value me 'instream))))
	(assert (= read-len size))
	octets))

