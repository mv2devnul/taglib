;;; -*- Mode: Lisp;  show-trailing-whitespace: t; Base: 10; indent-tabs: nil; Syntax: ANSI-Common-Lisp; Package: UTILS; -*-
;;; Copyright (c) 2013, Mark VandenBrink. All rights reserved.
(in-package #:utils)

(defun warn-user (format-string &rest args)
  "print a warning error to *ERROR-OUTPUT* and continue"
  ;; COMPLETELY UNPORTABLE!!!
  (format *error-output* "~&~&WARNING in ~a:: " (ccl::%last-fn-on-stack 1))
  (format *error-output* format-string args)
  (format *error-output* "~%~%"))
