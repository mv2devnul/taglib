;;; -*- Mode: Lisp;  show-trailing-whitespace: t; Base: 10; indent-tabs: nil; Syntax: ANSI-Common-Lisp; Package: MP4-FILE; -*-
;;; Copyright (c) 2013, Mark VandenBrink. All rights reserved.
(in-package #:mp4-file)

(log5:defcategory cat-log-mp4-file)
(defmacro log-mp4-file (&rest log-stuff) `(log5:log-for (cat-log-mp4-file) ,@log-stuff))

(defclass mp4-file (base-file:base-file)
  ((atoms :accessor atoms :initform nil))
  (:documentation "Class to access m4a/mp4 files"))

(defun make-mp4-file (filename read-only &key)
  "Convenience function to create an instance of MP4-FILE with appropriate init args"
  (log5:with-context "make-mp4-file"
	(log-mp4-file "opening ~a" filename)
	(let (handle)
	  (handler-case 
		  (progn
			(setf handle (make-instance 'mp4-file :filename filename :endian :big-endian :read-only read-only))
			(with-slots (atoms) handle
			  (log-mp4-file "getting atoms")
			  (setf atoms (mp4-atom:find-mp4-atoms handle))))
		(condition (c)
		  (warn "make-mp4-file got condition: ~a" c)
		  (when handle (base-file:close-audio-file handle))
		  (setf handle nil)))
	  handle)))
