;;; -*- Mode: Lisp;  show-trailing-whitespace: t; Base: 10; indent-tabs: nil; Syntax: ANSI-Common-Lisp; Package: MP3-TAG; -*-
;;; Copyright (c) 2013, Mark VandenBrink. All rights reserved.
(in-package #:mp3-tag)

(defmethod show-tags ((me mp3-file:mp3-file))
  (format t "~a:~a~%" (base-file:filename me) (mp3-frame:vpprint (mp3-file:header me) nil)))
