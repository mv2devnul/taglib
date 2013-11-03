;;; -*- Mode: Lisp;  show-trailing-whitespace: t; Base: 10; indent-tabs: nil; Syntax: ANSI-Common-Lisp; Package: TAGLIB-TESTS; -*-
;;; Copyright (c) 2013, Mark VandenBrink. All rights reserved.
(in-package #:cl-user)

(defpackage #:taglib-tests
  (:use #:common-lisp #:logging #:audio-streams))

(in-package #:taglib-tests)

;;; some convenient songs to parse
(defparameter *song-m4a*  "Queen/Queen I/01 Keep Yourself Alive.m4a")
(defparameter *song-mp3*  "Queen/Sheer Heart Attack/07 In The Lap Of The Gods.mp3")
(defparameter *song-flac* "Frank Zappa/Baby Snakes/02. Baby Snakes.flac")

;;;
;;; Set the pathname (aka filename) encoding in CCL for appropriate platform
;;;
;;; A note re filesystem encoding: my music collection is housed on a Mac and shared via SAMBA.
;;; In order to make sure we get valid pathnames, we need to set CCL's filesystem encoding to
;;; :UTF-8
(defun set-pathname-encoding (enc)        (setf (ccl:pathname-encoding-name) enc))
(defun set-pathname-encoding-for-osx ()   (set-pathname-encoding :utf-8))
(defun set-pathname-encoding-for-linux () (set-pathname-encoding nil))


(defun do-audio-file (&optional (file *song-m4a*) &key (func (constantly t)))
  "Parse one audio file (with condition handling)."
  (let ((foo))
    (unwind-protect
         (handler-case
             (progn
               (setf foo (make-file-stream file))
               (when foo
                 (parse-audio-file foo))    ; only call parse-audio if we got back a known file type
               (funcall func foo))          ; call func even if foo is null so it can account for unkown file types
           (condition (c)
             (utils:warn-user "File: ~a~%Got condition: <~a>" file c)))
      (when foo
        (stream-close foo)))))


(defun do-audio-dir (&optional (dir "Queen") &key (file-system-encoding :utf-8)
                                                  (func #'abstract-tag:show-tags))
  "Walk :DIR and FUNCALL specified function for each file audio found."
  (set-pathname-encoding file-system-encoding)
  (let ((mp3-count 0)
        (flac-count 0)
        (mp4-count 0)
        (other-count 0))

    (cl-fad:walk-directory dir (lambda (f)
                                 (do-audio-file f :func (lambda (s)
                                                          (cond ((typep s 'mp3-file-stream)  (incf mp3-count))
                                                                ((typep s 'flac-file-stream) (incf flac-count))
                                                                ((typep s 'mp4-file-stream)  (incf mp4-count))
                                                                ((null s)                    (incf other-count)))
                                                          (when (and (not (null s)) func) (funcall func s))))))

    (format t "~&~:d MP3s, ~:d MP4s, ~:d FLACs, ~:d Others, for a total of ~:d~%"
            mp3-count mp4-count flac-count other-count (+ mp3-count mp4-count flac-count other-count))))

(defun time-test (&optional (dir "Queen") &key (file-system-encoding :utf-8) (do-audio-processing t))
  "Time parsing of DIR."
  (let ((audio-streams:*get-audio-info* do-audio-processing))
    (time (do-audio-dir dir :file-system-encoding file-system-encoding :func nil))))

;;;;;;;;;;;;;;;;;;;; multi-thread code below ;;;;;;;;;;;;;;;;;;;;
(defparameter *end-thread*  #xdeadbeef)
(defparameter *max-threads* 4)

;;; Simple structure to hold a thread's results
(defstruct chanl-results
  name
  mp3-count
  flac-count
  mp4-count
  other-count)

(defun mp-do-audio-dir (&optional (dir "Queen") &key (file-system-encoding :utf-8)
                                                     (func #'abstract-tag:show-tags))
  "Walk :DIR and FUNCALL specified function for each file audio found."
  (set-pathname-encoding file-system-encoding)
  (let ((channel (make-instance 'chanl:unbounded-channel))
        (dead-channel (make-instance 'chanl:unbounded-channel))
        (mp3-count 0)
        (flac-count 0)
        (mp4-count 0)
        (other-count 0))

    ;; This function is run by each thread
    ;; Thread sits in a loop, reading from CHAN.  If that read
    ;; returns the integer *END-THREAD*, then thread exits; otherwise,
    ;; it runs DO-AUDIO-FILE on the file passed in.
    (labels ((thread-reader ()
               (declare (special *me*))
               (let ((f)
                     (results (make-chanl-results :name *me* :mp3-count 0 :flac-count 0 :mp4-count 0 :other-count 0)))
                 (loop
                   (with-slots (name mp3-count mp4-count flac-count other-count) results
                     (setf f (chanl:recv channel))
                     (when (and (typep f 'integer)
                                (= f *end-thread*))
                       (chanl:send dead-channel results) ; send structure of stats back to parent
                       (return-from thread-reader nil))

                     (do-audio-file f :func (lambda (s)
                                              (cond ((typep s 'mp3-file-stream)  (incf mp3-count))
                                                    ((typep s 'flac-file-stream) (incf flac-count))
                                                    ((typep s 'mp4-file-stream)  (incf mp4-count))
                                                    ((null s)                    (incf other-count)))
                                              (when (and (not (null s)) func) (funcall func s)))))))))

      ;; first, add all files in DIR to CHANNEL
      (cl-fad:walk-directory dir (lambda (f) (chanl:send channel f)))

      ;; At this point, CHANNEL is stuffed with files.
      ;; Now, send *MAX-THREADS* "ends" (at end of CHANNEL) and
      ;; spawn *MAX-THREADS* threads
      (dotimes (i *max-threads*)
        (chanl:send channel *end-thread*))
      (dotimes (i *max-threads*)
        (chanl:pcall #'thread-reader :initial-bindings `((*me* ,(format nil "reader-thread-~d" i)))))

      ;; sit in loop until we read *MAX-THREADS* results
      (block thread-reap
        (let ((i 0)
              results)

          (format t "Waiting on ~d threads~%" *max-threads*)
          (loop
            (force-output *standard-output*)
            (setf results (chanl:recv dead-channel))
            (format t "~4t~a died, ~:d MP3s, ~:d MP4s, ~:d FLACs, ~:d Others~%"
                    (chanl-results-name results)
                    (chanl-results-mp3-count results)
                    (chanl-results-mp4-count results)
                    (chanl-results-flac-count results)
                    (chanl-results-other-count results))
            (force-output *standard-output*)

            (incf mp3-count (chanl-results-mp3-count results))
            (incf mp4-count (chanl-results-mp4-count results))
            (incf flac-count (chanl-results-flac-count results))
            (incf other-count (chanl-results-other-count results))
            (incf i)

            (when (= i *max-threads*)
              (return-from thread-reap *max-threads*)))))

      (format t "All threads done~%")
      (format t "~&~:d MP3s, ~:d MP4s, ~:d FLACS, ~:d Others, for a total of ~:d files~%"
              mp3-count mp4-count flac-count other-count (+ mp3-count mp4-count flac-count other-count)))))

(defun mp-time-test (&optional (dir "Queen") &key (file-system-encoding :utf-8) (do-audio-processing t))
  "Time parsing of DIR."
  (let ((audio-streams:*get-audio-info* do-audio-processing))
    (time (mp-do-audio-dir dir :file-system-encoding file-system-encoding :func nil))))
