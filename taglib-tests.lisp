;;; -*- Mode: Lisp;  show-trailing-whitespace: t; Base: 10; indent-tabs: nil; Syntax: ANSI-Common-Lisp; Package: TAGLIB-TESTS; -*-
;;; Copyright (c) 2013, Mark VandenBrink. All rights reserved.
(in-package #:cl-user)

(defpackage #:taglib-tests
  (:use #:common-lisp #:audio-streams #:utils))

(in-package #:taglib-tests)

;;; some convenient songs to parse
(defparameter *song-m4a*
  "/home/markv/Music/Queen/Queen I/01 Keep Yourself Alive.m4a")
(defparameter *song-mp3*
  "/home/markv/Music/Queen/Sheer Heart Attack/07 In The Lap Of The Gods.mp3")
(defparameter *song-flac*
  "/home/markv/Music/Frank Zappa/Baby Snakes/02. Baby Snakes.flac")

;;;
;;; Set the pathname (aka filename) encoding for appropriate platform
;;;
;;; A note re filesystem encoding: my music collection is housed on a Mac and shared via SAMBA.
;;; In order to make sure we get valid pathnames, we need to set CCL's filesystem encoding to
;;; :UTF-8
(defun set-pathname-encoding (enc)
  #+CCL (setf (ccl:pathname-encoding-name) enc)
  #-CCL (declare (ignore enc))
  t)

(defun set-pathname-encoding-for-osx ()   (set-pathname-encoding :utf-8))
(defun set-pathname-encoding-for-linux () (set-pathname-encoding nil))

(defun do-audio-file (&key (file *song-m4a*)
                           (func #'abstract-tag:show-tags))
  "Parse one audio file and display the tags"
  (awhen (open-audio-file file)
    (funcall func it)))

(defstruct file-counts
  (mp3-count   0 :type fixnum)
  (flac-count  0 :type fixnum)
  (mp4-count   0 :type fixnum)
  (other-count 0 :type fixnum))

(defmethod print-object ((me file-counts) stream)
  (with-slots (mp3-count flac-count mp4-count other-count) me
    (format stream
            "~&~:d MP3s, ~:d MP4s, ~:d FLACs, ~:d Others, for a total of ~:d files~%"
            mp3-count mp4-count flac-count other-count
            (+ mp3-count mp4-count flac-count other-count))))

(defun do-audio-dir (&key (dir "/home/markv/Music/Queen")
                          (file-system-encoding :utf-8)
                          (func #'abstract-tag:show-tags))
  "Walk :DIR and FUNCALL specified function for each file audio found."
  (set-pathname-encoding file-system-encoding)
  (let ((file-counts (make-file-counts)))
    (with-slots (mp3-count flac-count mp4-count other-count) file-counts
      (cl-fad:walk-directory dir
                             (lambda (f)
                               (do-audio-file :file f
                                 :func (lambda (s)
                                         (cond ((typep s 'id3:mp3-file)
                                                (incf mp3-count))
                                               ((typep s 'flac:flac-file)
                                                (incf flac-count))
                                               ((typep s 'm4a:mp4-file)
                                                (incf mp4-count))
                                               ((null s)
                                                (incf other-count)))
                                         (when (and (not (null s)) func)
                                           (funcall func s)))))))
    file-counts))

(defun time-test (&key (dir "/home/markv/Music/Queen")
                       (file-system-encoding :utf-8) (do-audio-processing t))
  "Time parsing of DIR."
  (set-pathname-encoding file-system-encoding)
  (let ((audio-streams:*get-audio-info* do-audio-processing))
    (time (format t "~a~%"
                  (do-audio-dir :dir dir
                    :file-system-encoding file-system-encoding :func nil)))))

;; (defun get-stats (&key (dir "/home/markv/Music/Queen")
;;                        (file-system-encoding :utf-8)
;;                        (do-audio-processing t))
;;   "Gen up some interesting statistics on DIR"
;;   (let ((m4-ht (make-hash-table :test #'equalp))
;;         (m3-ht (make-hash-table :test #'equalp))
;;         (fl-ht (make-hash-table :test #'equalp)))
;;     (do-audio-dir
;;       :dir dir
;;       :file-system-encoding file-system-encoding
;;       :func (lambda (s)
;;               (cond ((typep s 'id3:mp3-file)
;;                      (id3:map-id3-frames
;;                       s
;;                       :func (lambda (f)
;;                               (multiple-value-bind (value foundp)
;;                                   (gethash (id3:id f) m3-ht)
;;                                 (setf (gethash (id3:id f) m3-ht)
;;                                       (if foundp
;;                                           (1+ value)
;;                                           1))))))
;;                       ((typep s 'flac:flac-file)
;;                        t)
;;                       ((typep s 'm4a:mp4-file)
;;                        (tree:traverse
;;                         (m4a:mp4-atoms s)
;;                         (lambda (node depth)
;;                           (declare (ignore depth))
;;                           (setf node (tree:data node))
;;                           (multiple-value-bind (value foundp)
;;                               (gethash (m4a:atom-type node) m3-ht)
;;                             (setf (gethash (m4a:atom-type node) m3-ht)
;;                                   (if foundp
;;                                       (1+ value)
;;                                       1))))))
;;                     ((null s)
;;                      (incf other-count)))))
;;     (format t "MP3 Stats:~%")
;;     (loop for key being the hash-keys of m3-ht
;;           using (hash-value value)
;;           do (format t "~a:~:d~%" key value))
;;     (format t "M4A Stats:~%")
;;     (loop for key being the hash-keys of m4-ht
;;           using (hash-value value)
;;           do (format t "~a:~:d~%" key value))
;;   (values m3-ht m4-ht fl-ht)))

;;;; multi-thread code below
#+(or :ccl :sbcl :abcl)
(progn

  (defparameter *end-thread*  #xdeadbeef)
  (defparameter *max-threads* 4)

;;; Simple structure to hold a thread's results
  (defstruct chanl-results
    name
    mp3-count
    flac-count
    mp4-count
    other-count)

  (defun mp-do-audio-dir (&key (dir "/home/markv/Music/Queen")
                               (file-system-encoding :utf-8)
                               (func nil))
    "Walk :DIR and FUNCALL specified function for each file audio found."
    (set-pathname-encoding file-system-encoding)
    (let ((channel      (make-instance 'chanl:unbounded-channel))
          (dead-channel (make-instance 'chanl:unbounded-channel))
          (mp3-count   0)
          (flac-count  0)
          (mp4-count   0)
          (other-count 0))

      ;; This function is run by each thread
      ;; Thread sits in a loop, reading from CHAN.  If that read
      ;; returns the integer *END-THREAD*, then thread exits; otherwise,
      ;; it runs DO-AUDIO-FILE on the file passed in.
      (labels ((thread-reader ()
                 (declare (special *me*))
                 (let ((f)
                       (results (make-chanl-results :name *me* :mp3-count 0
                                                    :flac-count 0 :mp4-count 0
                                                    :other-count 0)))
                   (loop
                     (with-slots (name mp3-count mp4-count flac-count other-count) results
                       (setf f (chanl:recv channel))
                       (when (and (typep f 'integer)
                                  (= f *end-thread*))
                         (chanl:send dead-channel results)
                         (return-from thread-reader nil))

                       (do-audio-file :file f
                         :func (lambda (s)
                                 (cond ((typep s 'id3:mp3-file)
                                        (incf mp3-count))
                                       ((typep s 'flac:flac-file)
                                        (incf flac-count))
                                       ((typep s 'm4a:mp4-file)
                                        (incf mp4-count))
                                       ((null s)
                                        (incf other-count)))
                                 (when (and (not (null s)) func)
                                   (funcall func s)))))))))

        ;; first, add all files in DIR to CHANNEL
        (cl-fad:walk-directory dir (lambda (f) (chanl:send channel f)))

        ;; At this point, CHANNEL is stuffed with files.
        ;; Now, send *MAX-THREADS* "ends" (at end of CHANNEL) and
        ;; spawn *MAX-THREADS* threads
        (dotimes (i *max-threads*)
          (chanl:send channel *end-thread*))
        (dotimes (i *max-threads*)
          (chanl:pcall
           #'thread-reader
           :initial-bindings `((*me* ,(format nil "reader-thread-~d" i)))))

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
                mp3-count mp4-count flac-count other-count
                (+ mp3-count mp4-count flac-count other-count)))))

  (defun mp-time-test (&key (dir "/home/markv/Music/Queen")
                            (file-system-encoding :utf-8) (do-audio-processing t))
    "Time parsing of DIR."
    (set-pathname-encoding file-system-encoding)
    (let ((audio-streams:*get-audio-info* do-audio-processing))
      (time (mp-do-audio-dir :dir dir :file-system-encoding file-system-encoding :func nil))))
  )
