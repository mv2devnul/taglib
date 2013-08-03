;;; -*- Mode: Lisp;  show-trailing-whitespace: t; Base: 10; indent-tabs: nil; Syntax: ANSI-Common-Lisp; Package: LOGGING; -*-
;;; Copyright (c) 2013, Mark VandenBrink. All rights reserved.

(in-package #:logging)

(defmacro start-logging ((name spec) &body body)
  `(unwind-protect 
		(progn
		  (log5:start-sender 'trace-log
			  (log5:stream-sender :location ,name)
			  :category-spec ,spec
			  :output-spec '(log5:message log5:context))
		  ,@body)
	 (log5:stop-sender 'trace-log)))


(defparameter *logging-categories* '(mp4-atom::cat-log-mp4-atom
									 audio-streams::cat-log-stream
									 mpeg::cat-log-mpeg-frame
									 id3-frame::cat-log-id3-frame))


(defmacro with-logging ((&key (file nil) (categories *logging-categories*)) &body body)
  (alexandria:with-gensyms (output-stream)
	`(let (,output-stream)
	   (unwind-protect
			(setf ,output-stream (if ,file
									 (open ,file :direction :output :if-exists :overwrite :if-does-not-exist :create)
									 *standard-output*))
			(log5:start-sender 'trace-log (log5:stream-sender :location ,output-stream)
							   :category-spec ',categories
							   :output-spec '(log5:message log5:context))
			,@body)
	   (if ,file (close ,output-stream))
	   (log5:stop-sender 'trace-log))))

