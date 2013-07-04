;;; -*- Mode: Lisp;  show-trailing-whitespace: t; Base: 10; indent-tabs: nil; Syntax: ANSI-Common-Lisp; Package: STREAMS; -*-
;;; Copyright (c) 2013, Mark VandenBrink. All rights reserved.

(in-package #:audio-streams)

(log5:defcategory cat-log-stream)
(defmacro log-stream (&rest log-stuff) `(log5:log-for (cat-log-stream) ,@log-stuff))

(deftype octet () '(unsigned-byte 8))
(defmacro make-octets (len) `(make-array ,len :element-type 'octet))

(defclass base-stream ()
  ((filename  :accessor filename  :initarg :filename)
   (instream  :accessor instream  :initform nil)
   (endian    :accessor endian    :initarg :endian :initform nil)   ; controls endian-ness of read/writes
   (modified  :accessor modified  :initform nil)					; for when we implement writing tags
   (file-size :accessor file-size))
  (:documentation "Base class for all audio file types"))

(defmethod initialize-instance :after ((me base-stream) &key read-only &allow-other-keys)
  (log5:with-context "base-stream-initializer"
  (with-slots (instream filename file-size endian) me
	(setf instream (if read-only
					   (open filename :direction :input :element-type 'octet)
					   (open filename :direction :io :if-exists :overwrite :element-type 'octet)))
	(setf binary-types:*endian* endian)
	(setf file-size (file-length instream))
	(log-stream "stream = ~a, name = ~a, size = ~:d~%endian = ~a"
				   instream filename file-size endian))))

(defmethod stream-close ((me base-stream))
  "Close an open stream."
  (with-slots (instream modified) me
	(when modified
	  (warn "at some point, should I add code to auto-write modified audio-files?")
	  (setf modified nil))
	(when instream
	  (close instream)
	  (setf instream nil))))

(defmethod stream-seek ((me base-stream) offset from)
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

(defmethod stream-read-u8 ((me base-stream))
  "read 1 byte from file"
  (multiple-value-bind (value size) (binary-types:read-binary 'u8 (slot-value me 'instream))
	(assert (= size 1) () "Expected to read 1 byte, got ~d instead" size)
	value))

(defmethod stream-read-u16 ((me base-stream))
  "read 2 bytes from file"
  (multiple-value-bind (value size) (binary-types:read-binary 'u16 (slot-value me 'instream))
	(assert (= size 2) () "Expected to read 2 bytes, got ~d instead" size)
	value))

;;; read 3-bytes
(binary-types:define-unsigned u24 3)

(defmethod stream-read-u24 ((me base-stream))
  "read 3 bytes from file"
  (multiple-value-bind (value size) (binary-types:read-binary 'u24 (slot-value me 'instream))
	(assert (= size 3) () "Expected to read 3 bytes, got ~d instead" size)
	value))

(defmethod stream-read-u32 ((me base-stream))
  "read 4 bytes from file"
  (multiple-value-bind (value size) (binary-types:read-binary 'u32 (slot-value me 'instream))
	(assert (= size 4) () "Expected to read 4 bytes, got ~d instead" size)
	value))

(defmethod stream-read-string ((me base-stream) &key size (terminators nil))
  "Read normal string from file. If size is provided, read exactly that many octets.
If terminators is supplied, it is a list of characters that can terminate a string (and hence stop read)"
  (multiple-value-bind (value read-size)
	  (binary-types:read-binary-string (slot-value me 'instream) :size size :terminators terminators)
	(declare (ignore read-size))
	;; what checks should happen here?
	value))

(defmethod stream-read-octets ((me base-stream) size)
  "Read SIZE octets from input-file"
  (let* ((octets (make-octets size))
		 (read-len (read-sequence octets (slot-value me 'instream))))
	(assert (= read-len size))
	octets))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; MP4 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(log5:defcategory cat-log-mp4-stream)
(defmacro log-mp4-stream (&rest log-stuff) `(log5:log-for (cat-log-mp4-stream) ,@log-stuff))

(defclass mp4-stream (base-stream)
  ((mp4-atoms :accessor mp4-atoms :initform nil))
  (:documentation "Class to access m4a/mp4 files"))

(defun make-mp4-stream (filename read-only &key)
  "Convenience function to create an instance of MP4-FILE with appropriate init args"
  (log5:with-context "make-mp4-stream"
	(log-mp4-stream "opening ~a" filename)
	(let (handle)
	  (handler-case 
		  (progn
			(setf handle (make-instance 'mp4-stream :filename filename :endian :big-endian :read-only read-only))
			(with-slots (mp4-atoms) handle
			  (log-mp4-stream "getting atoms")
			  (setf mp4-atoms (mp4-atom:find-mp4-atoms handle))))
		(condition (c)
		  (warn "make-mp4-stream got condition: ~a" c)
		  (when handle (stream-close handle))
		  (setf handle nil)))
		handle)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; MP3 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(log5:defcategory cat-log-mp3-stream)

(defmacro log-mp3-stream (&rest log-stuff) `(log5:log-for (cat-log-mp3-stream) ,@log-stuff))

(defclass mp3-stream (base-stream)
  ((mp3-header :accessor mp3-header :initform nil))
  (:documentation "Class to access mp3 files"))

(defun make-mp3-stream (filename read-only &key)
  "Convenience function to create an instance of MP3-FILE with appropriate init args.
NB: we assume non-syncsafe as default"
  (log5:with-context "make-mp3-stream"
	(log-mp3-stream "opening ~a" filename)
	(let (handle)
	  (handler-case 
		  (progn
			(setf handle (make-instance 'mp3-stream :filename filename :endian :little-endian :read-only read-only))
			(with-slots (mp3-header) handle
			  (log-mp3-stream "getting frames")
			  (setf mp3-header (mp3-frame:find-mp3-frames handle))))
		(condition (c)
		  (warn "make-mp3-stream got condition: ~a" c)
		  (when handle (stream-close handle))
		  (setf handle nil)))
	  handle)))

(defmethod stream-read-sync-safe-u32 ((me mp3-stream))
  "Read a sync-safe integer from file.  Used by mp3 files"
  (let* ((ret 0))
	(setf (ldb (byte 7 21) ret) (stream-read-u8 me))
	(setf (ldb (byte 7 14) ret) (stream-read-u8 me))
	(setf (ldb (byte 7 7) ret)  (stream-read-u8 me))
	(setf (ldb (byte 7 0) ret)  (stream-read-u8 me))
	ret))

(defmethod stream-read-sync-safe-octets ((me mp3-stream) len)
  "Used to undo sync-safe read of file"
  (let* ((last-byte-was-FF nil)
		 (byte nil)
		 (de-synced-data (binary-types:with-binary-output-to-vector (out)
						   (dotimes (i len)
							 (setf byte (stream-read-u8 me))
							 (if last-byte-was-FF
								 (if (not (zerop byte))
									 (write-byte byte out))
								 (write-byte byte out))
							 (setf last-byte-was-FF (= byte #xFF))))))
	de-synced-data))

