;;; taglib.asd
;;; Copyright (c) 2013, Mark VandenBrink. All rights reserved.
;;;
(asdf:defsystem #:taglib
  :description "Pure Lisp implementation to read (and write?) tags"
  :author "Mark VandenBrink"
  :license "Public Domain"
  :depends-on (#:log5 #:flexi-streams #:alexandria)
  :components ((:file "packages")
			   (:file "tag"       :depends-on ("packages"))
			   (:file "streams"   :depends-on ("packages"))
			   (:file "mp3-frame" :depends-on ("packages"))
			   (:file "mp3-tag"   :depends-on ("packages" "mp3-frame" "streams"))
			   (:file "logging"   :depends-on ("packages" "mp4-atom" "streams"))
			   (:file "mp4-atom"  :depends-on ("packages"))
			   (:file "mp4-tag"   :depends-on ("packages"))))


