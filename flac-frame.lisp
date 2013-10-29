;;; -*- Mode: Lisp;  show-trailing-whitespace: t; Base: 10; indent-tabs: nil; Syntax: ANSI-Common-Lisp; Package: FLAC-FRAME; -*-
;;; Copyright (c) 2013, Mark VandenBrink. All rights reserved.
(in-package #:flac-frame)

(log5:defcategory cat-log-flac-frame)
(defmacro log-flac-frame (&rest log-stuff) `(log5:log-for (cat-log-flac-frame) ,@log-stuff))

;;; FLAC header types
(defconstant +metadata-streaminfo+  0)
(defconstant +metadata-padding+     1)
(defconstant +metadata-application+ 2)
(defconstant +metadata-seektable+   3)
(defconstant +metadata-comment+     4)
(defconstant +metadata-cuesheet+    5)
(defconstant +metadata-picture+     6)

(defclass flac-header ()
  ((pos         :accessor pos         :initarg :pos         :documentation "file location of this flac header")
   (last-bit    :accessor last-bit    :initarg :last-bit    :documentation "if set, this is the last flac header in file")
   (header-type :accessor header-type :initarg :header-type :documentation "one of the flac header types above")
   (header-len  :accessor header-len  :initarg :header-len  :documentation "how long the info associated w/header is"))
  (:documentation "Representation of FLAC stream header"))

(defmacro with-frame-slots ((instance) &body body)
  `(with-slots (pos last-bit header-type header-len) ,instance
     ,@body))

(defmethod vpprint ((me flac-header) stream)
  (with-slots (pos last-bit header-type header-len) me
    (format stream "pos = ~:d, last-bit = ~b, header-type = ~d, length = ~:d"
            pos
            last-bit
            header-type
            header-len)))

(defun is-valid-flac-file (flac-file)
  "Make sure this is a FLAC file. Look for FLAC header at begining"
  (declare #.utils:*standard-optimize-settings*)
  (log5:with-context "is-valid-flac-file"
    (stream-seek flac-file 0 :start)
    (let ((valid nil))
      (when (> (stream-size flac-file) 4)
        (unwind-protect
             (handler-case
                 (let ((hdr (stream-read-string-with-len flac-file 4)))
                   (log-flac-frame "got <~a> for flac header" hdr)
                   (setf valid (string= "fLaC" hdr))
                   (log-flac-frame "valid = ~a" valid))
               (condition (c)
                 (utils:warn-user "is-valid-flac-file: got condition ~a" c)))
          (stream-seek flac-file 0 :start)))
        valid)))

(defun make-flac-header (stream)
  "Make a flac header from current position in stream"
  (declare #.utils:*standard-optimize-settings*)
  (log5:with-context "make-flac-header"
    (let* ((header (stream-read-u32 stream))
           (flac-header (make-instance 'flac-header
                                       :pos (- (stream-seek stream) 4)
                                       :last-bit (utils:get-bitfield header 31 1)
                                       :header-type (utils:get-bitfield header 30 7)
                                       :header-len (utils:get-bitfield header 23 24))))
      (log-flac-frame "header = ~a" (vpprint flac-header nil))
      flac-header)))


(defparameter *flac-tag-pattern* "(^[a-zA-Z]+)=(.*$)" "used to parse FLAC/ORBIS comments")

(defclass flac-tags ()
  ((vendor-str :accessor vendor-str :initarg :vendor-str :initform nil)
   (comments   :accessor comments   :initarg :comments   :initform nil)
   (tags       :accessor tags                            :initform (make-hash-table :test 'equal))))

(defmethod flac-add-tag ((me flac-tags) new-tag new-val)
  (declare #.utils:*standard-optimize-settings*)
  (let ((l-new-tag (string-downcase new-tag)))
    (setf (gethash l-new-tag (tags me)) new-val)))

(defmethod flac-get-tag ((me flac-tags) key)
  (declare #.utils:*standard-optimize-settings*)
  (gethash (string-downcase key) (tags me)))

(defun flac-get-tags (stream)
  "Loop through file and find all comment tags."
  (declare #.utils:*standard-optimize-settings*)
  (log5:with-context "flac-get-tags"
    (let* ((tags (make-instance 'flac-tags))
           (vendor-len (stream-read-u32 stream :endian :big-endian))
           (vendor-str (stream-read-utf-8-string-with-len stream vendor-len))
           (lst-len (stream-read-u32 stream :endian :big-endian)))

      (setf (vendor-str tags) vendor-str)

      (dotimes (i lst-len)
        (let* ((comment-len (stream-read-u32 stream :endian :big-endian))
               (comment (stream-read-utf-8-string-with-len stream comment-len)))
          (push comment (comments tags))
          (optima:match comment ((optima.ppcre:ppcre *flac-tag-pattern* tag value)
                                 (log-flac-frame "got ~a/~a" tag value)
                                 (flac-add-tag tags tag value)))))
      (setf (comments tags) (nreverse (comments tags)))
      tags)))

(defmethod find-flac-frames ((stream flac-file-stream))
  "Loop through file and find all FLAC headers. If we find comment or audio-info headers, go ahead and parse them too."
  (declare #.utils:*standard-optimize-settings*)
  (log5:with-context "find-flac-frames"
    (stream-seek stream 4 :start)

    (handler-case
        (let (headers)
          (loop for h = (make-flac-header stream) then (make-flac-header stream) do
            (push h headers)
            (log-flac-frame "Found flac frame: ~a" (vpprint h nil))
            (cond
              ((= +metadata-comment+ (header-type h))
               (setf (flac-tags stream) (flac-get-tags stream)))
              ((= +metadata-streaminfo+ (header-type h))
               (setf (audio-info stream) (get-flac-audio-info stream)))
              (t (stream-seek stream (header-len h) :current)))
            (when (not (zerop (last-bit h))) (return)))
          (setf (flac-headers stream) (nreverse headers)))
      (condition (c)
        (utils:warn-user "find-flac-frames got condition ~a" c)
        (log-flac-frame "got condition ~a when finding flac frames" c)))))

(defclass flac-audio-properties ()
  ((min-block-size  :accessor min-block-size  :initarg :min-block-size  :initform 0)
   (max-block-size  :accessor max-block-size  :initarg :max-block-size  :initform 0)
   (min-frame-size  :accessor min-frame-size  :initarg :min-frame-size  :initform 0)
   (max-frame-size  :accessor max-frame-size  :initarg :max-frame-size  :initform 0)
   (sample-rate     :accessor sample-rate     :initarg :sample-rate     :initform 0)
   (num-channels    :accessor num-channels    :initarg :num-channels    :initform 0)
   (bits-per-sample :accessor bits-per-sample :initarg :bits-per-sample :initform 0)
   (total-samples   :accessor total-samples   :initarg :total-samples   :initform 0)
   (md5-sig         :accessor md5-sig         :initarg :md5-sig         :initform 0))
  (:documentation "FLAC audio file properties"))

(defmethod vpprint ((me flac-audio-properties) stream)
  (format stream
          "min/max block size: ~:d/~:d; min/max frame size: ~:d/~:d; sample rate: ~d Hz; # channels: ~d; bps: ~:d; total-samples: ~:d; sig: ~x"
          (min-block-size me) (max-block-size me)
          (min-frame-size me) (max-frame-size me)
          (sample-rate me) (num-channels me) (bits-per-sample me)
          (total-samples me) (md5-sig me)))

(defun get-flac-audio-info (flac-stream)
  "Read in the the audio properties from current file position."
  (declare #.utils:*standard-optimize-settings*)
  (let ((info (make-instance 'flac-audio-properties)))
    (setf (min-block-size info) (stream-read-u16 flac-stream))
    (setf (max-block-size info) (stream-read-u16 flac-stream))
    (setf (min-frame-size info) (stream-read-u24 flac-stream))
    (setf (max-frame-size info) (stream-read-u24 flac-stream))
    (let* ((int1 (stream-read-u32 flac-stream))
           (int2 (stream-read-u32 flac-stream)))
      (setf (total-samples info) (logior (ash (get-bitfield int1 3  4) -32) int2))
      (setf (bits-per-sample info)            (1+ (get-bitfield int1 8  5)))
      (setf (num-channels info)               (1+ (get-bitfield int1 11 3)))
      (setf (sample-rate info)                (get-bitfield int1 31 20)))
    (setf (md5-sig info) (stream-read-u128 flac-stream))
    info))

(defun flac-show-raw-tag (flac-file-stream out-stream)
  (declare #.utils:*standard-optimize-settings*)
  (format out-stream "Vendor string: <~a>~%" (vendor-str (flac-tags flac-file-stream)))
  (dotimes (i (length (comments (flac-tags flac-file-stream))))
    (format out-stream "~4t[~d]: <~a>~%" i (nth i (comments (flac-tags flac-file-stream))))))
