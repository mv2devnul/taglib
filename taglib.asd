;;; -*- Mode: Lisp;  show-trailing-whitespace: t; Base: 10; indent-tabs: nil; Syntax: ANSI-Common-Lisp; Package: CL-USER; -*-
;;; Copyright (c) 2013, Mark VandenBrink. All rights reserved.

(asdf:defsystem #:taglib
  :description "Pure Lisp implementation to read (and write, perhaps, one day) tags"
  :author "Mark VandenBrink"
  :license "Public Domain"
  :depends-on (#:optima #:optima.ppcre)
  :components ((:file "packages")
               (:file "profile"       :depends-on ("packages"))
               (:file "utils"         :depends-on ("packages"))
               (:file "tree"          :depends-on ("packages"))
               (:file "audio-streams" :depends-on ("packages" "utils"))
               (:file "mpeg"          :depends-on ("packages" "audio-streams" "utils"))
               (:file "iso-639-2"     :depends-on ("packages" "utils"))
               (:file "id3-frame"     :depends-on ("packages" "utils"))
               (:file "flac-frame"    :depends-on ("packages" "utils"))
               (:file "abstract-tag"  :depends-on ("packages" "id3-frame" "audio-streams" "mp4-atom" "utils"))
               (:file "mp4-atom"      :depends-on ("packages" "utils"))))
