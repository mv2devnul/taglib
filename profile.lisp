;;; -*- Mode: Lisp;  show-trailing-whitespace: t; Base: 10; indent-tabs: nil; Syntax: ANSI-Common-Lisp; Package: CL-USER; -*-
;;; Copyright (c) 2013, Mark VandenBrink. All rights reserved.

;;;;;;;;;;;;;;;;;;;; Handy, dandy CCL profile functions ;;;;;;;;;;;;;;;;;;;;
;;;
;;; Usage: load and compile this file, then at REPL, type "profile-on".  After
;;; running programs, type "profile-report" to get a profile listing.
;;; "profile-reset" clears counters
;;; "profil-off" turns of profiling
(in-package #:cl-user)

(defun profile-on ()
  (dolist (p '("MP4-ATOM" "MPEG" "AUDIO-STREAMS" "ID3-FRAME" "UTILS" "LOGGING" "ISO-639-2" "ABSTRACT-TAG" "FLAC-FRAME"))
    (let ((pkg (find-package p)))
      (mon:monitor-all pkg)
      (format t "Package ~a, ~:d~%" pkg (length mon:*monitored-functions*)))))

(defun profile-report ()
  (mon:report :nested :inclusive :threshold 0.0 :names :all))

(defun profile-reset ()
  (mon:reset-all-monitoring))

(defun profile-off ()
  (mon:unmonitor))
