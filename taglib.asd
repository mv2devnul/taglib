;;; taglib.asd
;;; Copyright (c) 2013, Mark VandenBrink. All rights reserved.
;;;
(asdf:defsystem #:taglib
  :description "Pure Lisp implementation to read (and write?) tags"
  :author "Mark VandenBrink"
  :license "Public Domain"
  :depends-on (#:log5 #:alexandria)
  :components ((:file "packages")
			   (:file "audio-streams" :depends-on ("packages"))
			   (:file "mpeg"          :depends-on ("packages" "audio-streams"))
			   (:file "id3-frame"     :depends-on ("packages"))
			   (:file "mp3-tag"       :depends-on ("packages" "id3-frame" "audio-streams"))
			   (:file "logging"       :depends-on ("packages" "mp4-atom" "audio-streams"))
			   (:file "mp4-atom"      :depends-on ("packages"))
			   (:file "mp4-tag"       :depends-on ("packages"))))


