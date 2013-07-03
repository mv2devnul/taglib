;;; -*- Mode: Lisp;  show-trailing-whitespace: t; Base: 10; indent-tabs: nil; Syntax: ANSI-Common-Lisp; Package: MP3-FILE; -*-
;;; Copyright (c) 2013, Mark VandenBrink. All rights reserved.

(in-package :mp3-file)

(log5:defcategory cat-log-mp3-file)

(defmacro log-mp3-file (&rest log-stuff) `(log5:log-for (cat-log-mp3-file) ,@log-stuff))

(defclass mp3-file (base-file:base-file)
  ((header :accessor header :initform nil))
  (:documentation "Class to access mp3 files"))

(defun make-mp3-file (filename read-only &key)
  "Convenience function to create an instance of MP3-FILE with appropriate init args.
NB: we assume non-syncsafe as default"
  (log5:with-context "make-mp3-file"
	(log-mp3-file "opening ~a" filename)
	(let (handle)
	  (handler-case 
		  (progn
			(setf handle (make-instance 'mp3-file :filename filename :endian :little-endian :read-only read-only))
			(with-slots (header) handle
			  (log-mp3-file "getting frames")
			  (setf header (mp3-frame:find-mp3-frames handle))))
		(condition (c)
		  (warn "make-mp3-file got condition: ~a" c)
		  (when handle (base-file:close-audio-file handle))
		  (setf handle nil)))
	  handle)))

(defmethod read-sync-safe-u32 ((me mp3-file))
  "Read a sync-safe integer from file.  Used by mp3 files"
  (let* ((ret 0))
	(setf (ldb (byte 7 21) ret) (base-file:read-u8 me))
	(setf (ldb (byte 7 14) ret) (base-file:read-u8 me))
	(setf (ldb (byte 7 7) ret)  (base-file:read-u8 me))
	(setf (ldb (byte 7 0) ret)  (base-file:read-u8 me))
	ret))

