;;; -*- Mode: Lisp;  show-trailing-whitespace: t; Base: 10; indent-tabs: nil; Syntax: ANSI-Common-Lisp; Package: PROFILE; -*-
;;; Copyright (c) 2013, Mark VandenBrink. All rights reserved.

;;; Handy, dandy profile functions.
;;;   "profile:on" enables profiling for taglib modules
;;;   "profile:report" shows a profile listing
;;;   "profile:reset" clears counters
;;;   "profile:off" turns off profiling
;;;
;;; Only for CCL (it might work on other Lisps, but I haven't tried it)
;;;
(in-package #:profile)

#-CCL (progn
        (defun on     () (error "Not yet implemented on this Lisp"))
        (defun off    () (error "Not yet implemented on this Lisp"))
        (defun report () (error "Not yet implemented on this Lisp"))
        (defun reset  () (error "Not yet implemented on this Lisp")))

#+CCL (progn
        (defun on ()
          (let ((last-len 0)
                (cur-len 0))
            (dolist (p '("TREE" "MP4-ATOM" "MPEG" "AUDIO-STREAMS" "ID3-FRAME" "UTILS" "ISO-639-2" "ABSTRACT-TAG" "FLAC-FRAME"))
              (let ((pkg (find-package p)))
                (swank-monitor:monitor-all pkg)
                (setf cur-len (length swank-monitor:*monitored-functions*))
                (format t "~4d functions/methods being monitored in package ~a~%" (- cur-len last-len) p)
                (setf last-len cur-len)))
            (format t "~4d total functions/methods being monitored~%" last-len))
          (format t "~&~%~%~%~&WARNING: you MUST turn profiling off before recompiling library, or weirdness will ensue!!!~%~%"))

        (defun report (&key (sort-key :percent-time) (nested :exclusive))
          (swank-monitor:report :sort-key sort-key :nested nested :threshold 0.0 :names :all))

        (defun reset ()
          (swank-monitor:reset-all-monitoring))

        (defun off ()
          (swank-monitor:unmonitor)))
