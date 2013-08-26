;;; -*- Mode: Lisp;  show-trailing-whitespace: t; Base: 10; indent-tabs: nil; Syntax: ANSI-Common-Lisp; Package: TAGLIB-TESTS; -*-
;;; Copyright (c) 2013, Mark VandenBrink. All rights reserved.
(in-package #:cl-user)

(defpackage #:taglib-tests
  (:use #:common-lisp #:logging #:audio-streams))

(in-package #:taglib-tests)

;;; some convenient songs to parse
(defparameter *song-m4a* "Queen/Queen I/01 Keep Yourself Alive.m4a")
(defparameter *song-mp3* "Queen/Sheer Heart Attack/07 In The Lap Of The Gods.mp3")

;;;
;;; Set the pathname (aka filename) encoding in CCL for appropriate platorm
(defun set-pathname-encoding (enc)        (setf (ccl:pathname-encoding-name) enc))
(defun set-pathname-encoding-for-osx ()   (set-pathname-encoding :utf-8))
(defun set-pathname-encoding-for-linux () (set-pathname-encoding nil))

;;; A note re filesystem encoding: my music collection is housed on a Mac and shared via SAMBA.
;;; In order to make sure we get valid pathnames, we need to set CCL's filesystem encoding to
;;; :UTF-8

(defun do-audio-file (&optional (file *song-m4a*) &key (func (constantly t)))
  "Parse one audio file (with condition handling)."
  (let ((foo))
    (unwind-protect
         (handler-case
             (progn
               (setf foo (make-file-stream file))
               (when foo
                 (parse-audio-file foo)
                 (funcall func foo)))
           (condition (c)
             (utils:warn-user "File: ~a~%Got condition: <~a>" file c)))
      (when foo
        (stream-close foo)))))



(defun do-audio-dir (&optional (dir "Queen") &key (file-system-encoding :utf-8)
                                                  (mp3-func #'mp3-tag:show-tags)
                                                  (mp4-func #'mp4-tag:show-tags))
  "Walk :DIR and FUNCALL specified function for each file (MP4/MP3) found."
  (set-pathname-encoding file-system-encoding)
  (let ((mp3-count 0)
        (mp4-count 0)
        (other-count 0))

    (osicat:walk-directory dir (lambda (f)
                                 (let ((full-name (merge-pathnames (ccl:current-directory) (pathname f))))
                                   (do-audio-file full-name :func (lambda (s)
                                                            (cond ((typep s 'mp3-file-stream)
                                                                   (incf mp3-count)
                                                                   (when mp3-func
                                                                     (funcall mp3-func s)))
                                                                  ((typep s 'mp4-file-stream)
                                                                   (incf mp4-count)
                                                                   (when mp4-func
                                                                     (funcall mp4-func s)))
                                                                  (t (incf other-count))))))))

    (format t "~&~:d MP3s, ~:d MP4s, ~:d Others, for a total of ~:d~%"
            mp3-count mp4-count other-count (+ mp3-count mp4-count other-count))))

(defun time-test (&optional (dir "Queen") &key (file-system-encoding :utf-8) (do-audio-processing t))
  "Time parsing of DIR."
  (let ((audio-streams:*get-audio-info* do-audio-processing))
    (time (do-audio-dir dir :file-system-encoding file-system-encoding :mp3-func nil :mp4-func nil))))
