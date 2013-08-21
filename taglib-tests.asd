;;; taglib-tests.asd
;;; Copyright (c) 2013, Mark VandenBrink. All rights reserved.
;;;

;(pushnew :I-AM-MARKV *features*)
;(pushnew :USE-MMAP *features*)

(asdf:defsystem #:taglib-tests
  :description "Simple demo/test code for taglib"
  :author "Mark VandenBrink"
  :license "Public Domain"
  :depends-on (#:taglib #:osicat)
  :components ((:file "taglib-tests")))
