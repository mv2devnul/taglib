;;; -*- Mode: Lisp;  show-trailing-whitespace: t; Base: 10; indent-tabs: nil; Syntax: ANSI-Common-Lisp; Package: MP3-TAG; -*-
;;; Copyright (c) 2013, Mark VandenBrink. All rights reserved.
(in-package #:mp3-tag)

(defmethod show-tags ((me mp3-stream))
  (format t "~a:~a~%" (filename me) (mp3-frame:vpprint (audio-streams:mp3-header me) nil)))
