;;; -*- Mode: Lisp;  show-trailing-whitespace: t; Base: 10; indent-tabs: nil; Syntax: ANSI-Common-Lisp; Package: STREAMS; -*-
;;; Copyright (c) 2013, Mark VandenBrink. All rights reserved.

(in-package #:audio-streams)

(log5:defcategory cat-log-stream)
(defmacro log-stream (&rest log-stuff) `(log5:log-for (cat-log-stream) ,@log-stuff))

(deftype octet () '(unsigned-byte 8))
(defmacro make-octets (len) `(make-array ,len :element-type 'octet))

(defclass base-file-stream ()
  ((filename  :accessor filename  :initarg :filename)
   (instream  :accessor instream  :initform nil)
   (endian    :accessor endian    :initarg :endian :initform nil)   ; controls endian-ness of read/writes
   (modified  :accessor modified  :initform nil)					; for when we implement writing tags
   (file-size :accessor file-size))
  (:documentation "Base class for all audio file types"))

(defmethod initialize-instance :after ((me base-file-stream) &key read-only &allow-other-keys)
  (log5:with-context "base-file-stream-initializer"
  (with-slots (instream filename file-size endian) me
	(setf instream (if read-only
					   (open filename :direction :input :element-type 'octet)
					   (open filename :direction :io :if-exists :overwrite :element-type 'octet)))
	(setf file-size (file-length instream))
	(log-stream "base-file-stream-initializer built stream = ~a, name = ~a, size = ~:d, endian = ~a"
				instream filename file-size endian))))

(defmethod stream-close ((me base-file-stream))
  "Close an open stream."
  (with-slots (instream modified) me
	(when modified
	  (warn "at some point, should I add code to auto-write modified audio-files?")
	  (setf modified nil))
	(when instream
	  (close instream)
	  (setf instream nil))))

(defmethod stream-seek ((me base-file-stream) offset from)
  "C-library like seek function. from can be one of :current, :start, :end.
Returns the current offset into the stream"
  (with-slots (instream file-size) me
	(ecase from
	  (:start (file-position instream offset))
	  (:current (file-position instream (+ (file-position instream) offset)))
	  (:end (file-position instream (- file-size offset))))))

;;
;; Based on a function from Practical Common Lisp by Peter Seibel.
(defun read-octets (instream bytes &key (bits-per-byte 8) (endian :little-endian))
  (ecase endian
	(:big-endian
	 (loop with value = 0
		   for low-bit from 0 to (* bits-per-byte (1- bytes)) by bits-per-byte do
			 (setf (ldb (byte bits-per-byte low-bit) value) (read-byte instream))
		   finally (return value)))
	(:little-endian
	 (loop with value = 0
		   for low-bit downfrom (* bits-per-byte (1- bytes)) to 0 by bits-per-byte do
			 (setf (ldb (byte bits-per-byte low-bit) value) (read-byte instream))
		   finally (return value)))))

(defmethod stream-read-u8 ((me base-file-stream))
  "read 1 byte from file"
  (with-slots (endian instream) me
	(read-octets instream 1 :endian endian)))

(defmethod stream-read-u16 ((me base-file-stream))
  "read 2 bytes from file"
  (with-slots (endian instream) me
	(read-octets instream 2 :endian endian)))

(defmethod stream-read-u24 ((me base-file-stream))
  "read 3 bytes from file"
  (with-slots (endian instream) me
	(read-octets instream 3 :endian endian)))

(defmethod stream-read-u32 ((me base-file-stream))
  "read 4 bytes from file"
  (with-slots (endian instream) me
	(read-octets instream 4 :endian endian)))

(defmethod stream-read-string ((me base-file-stream) &key size (terminators nil))
  "Read normal string from file. If size is provided, read exactly that many octets.
If terminators is supplied, it is a list of characters that can terminate a string (and hence stop read)"
  (with-output-to-string (s)
	(with-slots (instream) me
	  (let ((terminated nil)
			(count 0)
			(byte))
		(loop
		  (when (if size (= count size) terminated) (return))
		  (setf byte (read-byte instream))
		  (incf count)
		  ;;;(log-stream "count = ~d, terminators = ~a, byte-read was ~c" count terminators (code-char byte))
		  (when (member byte terminators :test #'=)
			(setf terminated t))
		  (when (not terminated)
			(write-char (code-char byte) s)))))))

(defmethod stream-read-octets ((me base-file-stream) size &key (bits-per-byte 8))
  "Read SIZE octets from input-file.  If bits-per-byte"
  (ecase bits-per-byte
	(8
	 (let ((octets (make-octets size)))
	   (read-sequence octets (slot-value me 'instream))))
	(7
	 (let* ((last-byte-was-FF nil)
			(byte nil)
			(octets (ccl:with-output-to-vector (out)
					  (dotimes (i size)
						(setf byte (stream-read-u8 me))
						(if last-byte-was-FF
							(if (not (zerop byte))
								(write-byte byte out))
							(write-byte byte out))
						(setf last-byte-was-FF (= byte #xFF))))))
	   octets))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; MP4 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass mp4-stream (base-file-stream)
  ((mp4-atoms :accessor mp4-atoms :initform nil))
  (:documentation "Class to access m4a/mp4 files"))

(defun make-mp4-stream (filename read-only &key)
  "Convenience function to create an instance of MP4-FILE with appropriate init args"
  (log5:with-context "make-mp4-stream"
	(log-stream "make-mp4-stream is opening ~a" filename)
	(let (handle)
	  (handler-case 
		  (progn
			(setf handle (make-instance 'mp4-stream :filename filename :endian :little-endian :read-only read-only))
			(with-slots (mp4-atoms) handle
			  (log-stream "getting atoms")
			  (setf mp4-atoms (mp4-atom:find-mp4-atoms handle))))
		(condition (c)
		  (warn "make-mp4-stream got condition: ~a" c)
		  (when handle (stream-close handle))
		  (setf handle nil)))
		handle)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; MP3 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defclass mp3-stream (base-file-stream)
  ((mp3-header :accessor mp3-header :initform nil))
  (:documentation "Class to access mp3 files"))

(defun make-mp3-stream (filename read-only &key)
  "Convenience function to create an instance of MP3-FILE with appropriate init args.
NB: we assume non-syncsafe as default"
  (log5:with-context "make-mp3-stream"
	(log-stream "opening ~a, read-only = ~a" filename read-only)
	(let (handle)
	  (handler-case
		  (progn
			(setf handle (make-instance 'mp3-stream :filename filename :endian :big-endian :read-only read-only))
			(with-slots (mp3-header) handle
			  (log-stream "getting frames")
			  (setf mp3-header (mp3-frame:find-mp3-frames handle))))
		(condition (c)
		  (warn "make-mp3-stream got condition: ~a" c)
		  (when handle (stream-close handle))
		  (setf handle nil)))
	  handle)))

(defmethod stream-read-sync-safe-u32 ((me mp3-stream))
  "Read a sync-safe integer from file.  Used by mp3 files"
  (read-octets (slot-value me 'instream) 4 :bits-per-byte 7 :endian :little-endian))


#|
(defun tst ()
  (let ((foo (ccl:with-output-to-vector (f) 
			   (write-byte #xDE f)
			   (write-byte #xAD f)
			   (write-byte #xBE f)
			   (write-byte #xEF f))))
	(ccl:with-input-from-vector (f foo)
	  (format t "Length is ~d~%" (ccl::stream-length f))
	  (dotimes (j 2)
		(format t "Iteration ~d~%" j)
		(ccl::stream-position f 0)
		(dotimes (i (ccl::stream-length f))
		  (format t "~d: ~x~%" i (read-byte f)))))))
|#
