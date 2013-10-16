;;; -*- Mode: Lisp;  show-trailing-whitespace: t; Base: 10; indent-tabs: nil; Syntax: ANSI-Common-Lisp; Package: PROFILE; -*-
;;; Copyright (c) 2013, Mark VandenBrink. All rights reserved.

;;;;;;;;;;;;;;;;;;;; Handy, dandy CCL profile functions ;;;;;;;;;;;;;;;;;;;;
;;;
;;; Usage: load and compile this file, then at REPL, type "profile:on".  After
;;; running programs, type "profile:report" to get a profile listing.
;;; "profile:reset" clears counters
;;; "profil:off" turns of profiling
(in-package #:profile)
#-CCL (progn
        (defun on     () (error "Not Yet"))
        (defun off    () (error "Not Yet"))
        (defun report () (error "Not Yet"))
        (defun reset  () (error "Not Yet")))

#+CCL (progn
        (defun on ()
          (dolist (p '("MP4-ATOM" "MPEG" "AUDIO-STREAMS" "ID3-FRAME" "UTILS" "LOGGING" "ISO-639-2" "ABSTRACT-TAG" "FLAC-FRAME"))
            (let ((pkg (find-package p)))
              (mon:monitor-all pkg)
              (format t "Package ~a, ~:d~%" pkg (length mon:*monitored-functions*))))
          (format t "~&~&WARNING: YOU MUST TURN PROFILING OFF BEFORE RECOMPILING LIBRARY!!!~%"))

        (defun report (&key (sort-key :percent-time) (nested :exclusive))
          (mon:report :sort-key sort-key :nested nested :threshold 0.0 :names :all))

        (defun reset ()
          (mon:reset-all-monitoring))

        (defun off ()
          (mon:unmonitor)))
