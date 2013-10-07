;;; taglib.asd
;;; Copyright (c) 2013, Mark VandenBrink. All rights reserved.
;;;

(asdf:defsystem #:taglib
  :description "Pure Lisp implementation to read (and write, perhaps, one day) tags"
  :author "Mark VandenBrink"
  :license "Public Domain"
  :depends-on (#:log5 #:optima #:optima.ppcre)
  :components ((:file "packages")
               (:file "utils"         :depends-on ("packages"))
               (:file "audio-streams" :depends-on ("packages" "utils"))
               (:file "mpeg"          :depends-on ("packages" "audio-streams" "utils"))
               (:file "iso-639-2"     :depends-on ("packages" "utils"))
               (:file "id3-frame"     :depends-on ("packages" "utils"))
			   (:file "flac-frame"    :depends-on ("packages" "utils"))
               (:file "abstract-tag"  :depends-on ("packages" "id3-frame" "audio-streams" "mp4-atom" "utils"))
			   ;;(:file "mp3-tag"       :depends-on ("packages" "id3-frame" "audio-streams" "utils"))
               ;;(:file "mp4-tag"       :depends-on ("packages" "utils")))
               (:file "logging"       :depends-on ("packages" "mp4-atom" "audio-streams" "utils"))
               (:file "mp4-atom"      :depends-on ("packages" "utils"))))
