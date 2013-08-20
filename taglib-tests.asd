;;; taglib-tests.asd
;;; Copyright (c) 2013, Mark VandenBrink. All rights reserved.
;;;

;;; should only be set when on markv machines...
(pushnew :I-AM-MARKV *features*)

(asdf:defsystem #:taglib-tests
  :description "Simple demo/test code for taglib"
  :author "Mark VandenBrink"
  :license "Public Domain"
  :depends-on (#:taglib #:osicat)
  :components ((:file "taglib-tests")))
