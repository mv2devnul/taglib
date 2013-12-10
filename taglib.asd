;;; -*- Mode: Lisp;  show-trailing-whitespace: t; Base: 10; indent-tabs: nil; Syntax: ANSI-Common-Lisp; Package: CL-USER; -*-
;;; Copyright (c) 2013, Mark VandenBrink. All rights reserved.

;;;;(pushnew :dbg *features*)

(asdf:defsystem #:taglib
  :description "Pure Lisp implementation to read (and write, perhaps, one day) tags"
  :author "Mark VandenBrink"
  :license "UNLICENSE <http://unlicense.org/>"
  :depends-on (#:optima #:optima.ppcre #:flexi-streams
                        #+(or :ccl :sbcl :abcl) #:bordeaux-threads)
  :components ((:file "packages")
               (:file "profile"       :depends-on ("packages"))
               (:file "utils"         :depends-on ("packages"))
               (:file "tree"          :depends-on ("packages"))
               (:file "audio-streams" :depends-on ("packages" "utils"))
               (:file "mpeg"          :depends-on ("packages" "audio-streams" "utils"))
               (:file "iso-639-2"     :depends-on ("packages" "utils"))
               (:file "id3"           :depends-on ("packages" "utils"))
               (:file "flac"          :depends-on ("packages" "utils"))
               (:file "abstract-tag"  :depends-on ("packages" "id3" "audio-streams" "m4a" "utils"))
               (:file "m4a"           :depends-on ("packages" "utils"))))
