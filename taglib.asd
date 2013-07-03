;;; taglib.asd
;;; Copyright (c) 2013, Mark VandenBrink. All rights reserved.
;;;
(asdf:defsystem #:taglib
  :description "Pure Lisp implementation to read (and write?) tags"
  :author "Mark VandenBrink"
  :license "Public Domain"
  :depends-on (#:log5 #:binary-types #:alexandria)
  :components ((:file "packages")
			   (:file "tag"       :depends-on ("packages"))
			   (:file "base-file" :depends-on ("packages"))
			   (:file "mp3-file"  :depends-on ("packages" "base-file" "mp3-frame"))
			   (:file "mp3-frame" :depends-on ("packages"))
			   (:file "mp3-tag"   :depends-on ("packages" "mp3-frame" "mp3-file"))
			   (:file "logging"   :depends-on ("packages" "mp4-atom" "mp4-file" "base-file"))
			   (:file "mp4-atom"  :depends-on ("packages"))
			   (:file "mp4-tag"   :depends-on ("packages"))
			   (:file "mp4-file"  :depends-on ("packages" "base-file" "mp4-atom"))))

