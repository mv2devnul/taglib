;;; -*- Mode: Lisp;  show-trailing-whitespace: t; Base: 10; indent-tabs: nil; Syntax: ANSI-Common-Lisp; Package: TAGLIB-TESTS; -*-
;;; Copyright (c) 2013, Mark VandenBrink. All rights reserved.
(in-package #:cl-user)

(defpackage #:taglib-tests
  (:use #:common-lisp #:logging #:audio-streams))

(in-package #:taglib-tests)

;;; some convenient songs to parse
(defparameter *song-m4a* "Queen/Queen 01 Keep Yourself Alive.m4a")
(defparameter *song-mp3* "Queen/Sheer Heart Attack/07 In The Lap Of The Gods.mp3")

;;;
;;; Set the pathname (aka filename) encoding in CCL for appropriate platorm
(defun set-pathname-encoding (enc)        (setf (ccl:pathname-encoding-name) enc))
(defun set-pathname-encoding-for-osx ()   (set-pathname-encoding :utf-8))
(defun set-pathname-encoding-for-linux () (set-pathname-encoding nil))

(defmethod has-extension ((n string) ext)
  "Probably should use CL's PATHNAME methods, but simply looking at the .XXX portion of a filename
to see if it matches. This is the string version that makes a PATHNAME and calls the PATHNAME version."
  (has-extension (parse-namestring n) ext))

(defmethod has-extension ((p pathname) ext)
  "Probably should use CL's PATHNAME methods , but simply looking at the .XXX portion of a filename
to see if it matches. PATHNAME version."
  (let ((e (pathname-type p)))
    (if e
      (string= (string-downcase e) (string-downcase ext))
      nil)))

(defmacro redirect (filename &rest body)
  "Temporarily set *STANDARD-OUTPUT* to FILENAME and execute BODY."
  `(let ((*standard-output* (open ,filename :direction :output :if-does-not-exist :create :if-exists :supersede)))
     ,@body
     (finish-output *standard-output*)))

;;; A note re filesystem encoding: my music collection is housed on a Mac and shared via SAMBA.
;;; In order to make sure we get valid pathnames, we need to set CCL's filesystem encoding to
;;; :UTF-8

;;;;;;;;;;;;;;;;;;;; MP4 Tests ;;;;;;;;;;;;;;;;;;;;
(defun mp4-test0 (file)
  "Parse one MP3 file (with condition handling)."
  (let ((foo))
    (unwind-protect
         (handler-case
             (setf foo (parse-mp4-file file))
           (condition (c)
             (utils:warn-user "File: ~a~%Got condition: <~a>~%" file c)))
      (when foo (stream-close foo)))
    foo))

(defun mp4-test1 ()
  (mp4-test0 *song-m4a*))

(defun mp4-test2 (&key (dir "Queen") (raw nil) (file-system-encoding :utf-8))
  "Walk :DIR and call SHOW-TAGS for each file (MP4/MP3) found."
  (set-pathname-encoding file-system-encoding)
  (osicat:walk-directory dir (lambda (f)
                               (when (has-extension f "m4a")
                                 (let ((file (mp4-test0 (merge-pathnames (ccl:current-directory) (pathname f)))))
                                   (when file
                                     (mp4-tag:show-tags file :raw raw)))))))

;;;;;;;;;;;;;;;;;;;; MP3 Tests ;;;;;;;;;;;;;;;;;;;;
(defun mp3-test0 (file)
  "Parse one MP3 file (with condition handling)."
  (let ((foo))
    (unwind-protect
         (handler-case
             (setf foo (parse-mp3-file file))
           (condition (c)
             (utils:warn-user "File: ~a~%Got condition: <~a>~%" file c)))
      (when foo (stream-close foo)))
    foo))

(defun mp3-test1 ()
  (mp3-test0 *song-mp3*))

(defun mp3-test2 (&key (dir "Queen") (raw nil) (file-system-encoding :utf-8))
  "Walk :DIR and parse every MP3 we find."
  (set-pathname-encoding file-system-encoding)
  (osicat:walk-directory dir (lambda (f)
                               (when (has-extension f "mp3")
                                 (let ((file (mp3-test0 (merge-pathnames (ccl:current-directory) (pathname f)))))
                                   (when file
                                     (mp3-tag:show-tags file :raw raw)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun test2 (&key (dir "Queen") (raw nil) (file-system-encoding :utf-8))
  "Walk :DIR and call SHOW-TAGS for each file (MP4/MP3) found."
  (set-pathname-encoding file-system-encoding)
  (osicat:walk-directory dir (lambda (f)
                               (let ((full-name (merge-pathnames (ccl:current-directory) (pathname f))))
                                 (cond ((has-extension f "mp3")
                                        (let ((file (mp3-test0 full-name)))
                                          (when file
                                            (mp3-tag:show-tags file :raw raw))))
                                       ((has-extension f "m4a")
                                        (let ((file (mp4-test0 full-name)))
                                          (when file
                                            (mp4-tag:show-tags file :raw raw)))))))))

(defun time-test (dir &key (file-system-encoding :utf-8) (do-audio-processing t))
  "Time parsing of DIR."
  (let ((mp3-count 0)
        (mp4-count 0)
        (other-count 0))
    (labels ((do-dir (dir)
               (osicat:walk-directory dir (lambda (f)
                                            (let ((full-name (merge-pathnames (ccl:current-directory) (pathname f))))
                                              (cond ((has-extension f "mp3")
                                                     (incf mp3-count)
                                                     (mp3-test0 full-name))
                                                    ((has-extension f "m4a")
                                                     (incf mp4-count)
                                                     (mp4-test0 full-name))
                                                    (t
                                                     (incf other-count))))))))
      (set-pathname-encoding file-system-encoding)
      (let ((audio-streams:*get-audio-info* do-audio-processing))
        (time (do-dir dir)))
      (format t "~:d MP3s, ~:d MP4s, ~:d Others~%"
              mp3-count mp4-count other-count))))
