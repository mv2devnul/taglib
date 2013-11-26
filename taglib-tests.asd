;;; taglib-tests.asd
;;; Copyright (c) 2013, Mark VandenBrink. All rights reserved.
;;;
(asdf:defsystem #:taglib-tests
  :description "Simple demo/test code for taglib"
  :author "Mark VandenBrink"
  :license "Public Domain"
  :depends-on (#:taglib #:chanl #:cl-fad)
  :components ((:file "taglib-tests")))
