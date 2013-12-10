;;; -*- Mode: Lisp;  show-trailing-whitespace: t; Base: 10; indent-tabs: nil; Syntax: ANSI-Common-Lisp; Package: AUDIO-STREAMS; -*-
;;; Copyright (c) 2013, Mark VandenBrink. All rights reserved.

(in-package #:audio-streams)

(defun make-audio-stream (arg)
  "Creates a stream for ARG"
  (declare #.utils:*standard-optimize-settings*)

  (labels ((make-file-stream (name)
             (let ((fd (open name :direction :input :element-type 'octet)))
               (if fd
                   (flex:make-flexi-stream fd :element-type 'octet)
                   nil))))
    (etypecase arg
      (string   (make-file-stream arg))
      (pathname (make-file-stream arg))
      (vector   (flex:make-in-memory-input-stream arg)))))

(defgeneric stream-size (stream))

(defmethod stream-size ((stream flex:flexi-input-stream))
  (declare #.utils:*standard-optimize-settings*)
  (file-length (flex:flexi-stream-stream stream)))

(defmethod stream-size ((stream flex:in-memory-stream))
  (declare #.utils:*standard-optimize-settings*)
  (flex::vector-stream-end stream))

(defgeneric stream-filename (stream))

(defmethod stream-filename ((stream flex:flexi-stream))
  (declare #.utils:*standard-optimize-settings*)
  (pathname (flex:flexi-stream-stream stream)))

(defun stream-seek (stream
                    &optional (offset 0) (from :current))
  "Move the FILE-POSITION of a stream"
  (declare #.utils:*standard-optimize-settings*)
  (declare (fixnum offset))

  (ecase from
    (:start (file-position stream offset))
    (:current (file-position stream (+ (file-position stream) offset)))
    (:end (file-position stream (- (stream-size stream)
                                   offset)))))

(declaim (inline read-n-bytes))

;;;; Support for the uxx readers
(defun read-n-bytes (stream n-bytes
                     &key (bits-per-byte 8) (endian :little-endian))
  "Returns a FIXNUM constructed by reading N-BYTES.  BITS-PER-BYTE controls how
many bits should be used from each read byte."
  (declare #.utils:*standard-optimize-settings*)
  (declare (fixnum n-bytes))

  (ecase endian
    (:little-endian
     (loop with value = 0
           for low-bit downfrom (* bits-per-byte (1- n-bytes)) to 0
             by bits-per-byte do
               (awhen (read-byte stream nil nil)
                 (setf (ldb (byte bits-per-byte low-bit) value) it))
           finally (return-from read-n-bytes value)))
    (:big-endian
     (loop with value = 0
           for low-bit upfrom 0 to (* bits-per-byte (1- n-bytes))
             by bits-per-byte do
               (awhen (read-byte stream nil nil)
                 (setf (ldb (byte bits-per-byte low-bit) value) it))
           finally (return-from read-n-bytes value)))))

;;;; Number readers
(declaim (inline stream-read-u8
                 stream-read-u16
                 stream-read-u32
                 stream-read-u64
                 stream-read-u128))

(defun stream-read-u8 (stream)
  (declare #.utils:*standard-optimize-settings*)
  (read-byte stream nil nil))

(defun stream-read-u16  (stream &key (bits-per-byte 8) (endian :little-endian))
  (read-n-bytes stream 2  :bits-per-byte bits-per-byte :endian endian))

(defun stream-read-u24  (stream &key (bits-per-byte 8) (endian :little-endian))
  (read-n-bytes stream 3  :bits-per-byte bits-per-byte :endian endian))

(defun stream-read-u32  (stream &key (bits-per-byte 8) (endian :little-endian))
  (read-n-bytes stream 4  :bits-per-byte bits-per-byte :endian endian))

(defun stream-read-u64  (stream &key (bits-per-byte 8) (endian :little-endian))
  (read-n-bytes stream 8  :bits-per-byte bits-per-byte :endian endian))

(defun stream-read-u128 (stream &key (bits-per-byte 8) (endian :little-endian))
  (read-n-bytes stream 16 :bits-per-byte bits-per-byte :endian endian))

;;;; Sequences
(defun stream-read-sequence (stream size &key (bits-per-byte 8))
  "Read in a sequence of octets at BITS-PER-BYTE"
  (declare #.utils:*standard-optimize-settings*)

  (ecase bits-per-byte
    (8 (let ((octets (make-octets size)))
         (values octets (read-sequence octets stream))))
    (7 (let* ((last-byte-was-FF nil)
              (byte nil)
              (octets (flex:with-output-to-sequence (out :element-type 'octet)
                        (dotimes (i size)
                          (setf byte (stream-read-u8 stream))
                          (if last-byte-was-FF
                              (if (not (zerop byte))
                                  (write-byte byte out))
                              (write-byte byte out))
                          (setf last-byte-was-FF (= byte #xFF))))))
         (values octets size)))))

;;;; Strings: readers
(defun stream-read-iso-string (instream &optional (len nil))
  "Read an ISO-8859-1 string of &OPTIONAL LEN.  When len is NIL,
read in null-terminated ISO string w/o null at end"
  (declare #.utils:*standard-optimize-settings*)

  (let (octets)
    (if (null len)
        (progn
          (setf octets
                (flex:with-output-to-sequence (out)
                  (do ((b (stream-read-u8 instream) (stream-read-u8 instream)))
                      (nil)
                    (when (zerop b)
                      (return))         ; leave loop w/o writing
                    (write-byte b out))))
          (setf len (length octets)))
        (setf octets (stream-read-sequence instream len)))

    (when (= 0 len)
      (return-from stream-read-iso-string ""))

    (flex:octets-to-string octets :external-format :iso-8859-1)))

(defun get-byte-order-mark (octets)
  "Get the BOM from octets"
  (declare #.utils:*standard-optimize-settings*)

  (let ((retval 0))
    (setf (ldb (byte 8 0) retval) (aref octets 1)
          (ldb (byte 8 8) retval) (aref octets 0))
    (when (not (or (= #xfffe retval) (= #xfeff retval)))
      (error
       "Got invalid byte-order mark of ~x in STREAM-DECODE-UCS-STRING"
       retval))
    retval))

(defun stream-read-ucs-string (instream &key (len nil) (kind :ucs))
  "Read a UCS-2 string of length 'len'.  If len is nil read until we get null.
KIND is :ucs-2, :ucs-2be or :ucs-2le.  flexi-streams doesn't appear to handle
byte-order marks, so we have to do that here before calling."
  (declare #.utils:*standard-optimize-settings*)

  (let ((octets)
        (start 0))

    (if (null len)
        (progn
          (setf octets (flex:with-output-to-sequence (out)
                         (do* ((b0 (stream-read-u8 instream)
                                   (stream-read-u8 instream))
                               (b1 (stream-read-u8 instream)
                                   (stream-read-u8 instream)))
                              (nil)
                           (when (and (zerop b0) (zerop b1))
                             (return))
                           (write-byte b0 out)
                           (write-byte b1 out))))
          (setf len (length octets)))
        (setf octets (stream-read-sequence instream len)))

    (when (oddp len)
      (warn-user "UCS string has odd length, decrementing by 1")
      (decf len 1))

    (when (= 0 len)
      (return-from stream-read-ucs-string ""))

    (when (eql kind :ucs-2)
      (setf start 2)
      (let ((bom (get-byte-order-mark octets)))
        (ecase bom
          (#xfffe (setf kind :ucs-2le))
          (#xfeff (setf kind :ucs-2be)))))

    (flex:octets-to-string octets :external-format kind :start start :end len)))

(defun stream-read-utf-8-string (instream &optional (len nil))
  "Read an UTF-8 string of length LEN.  If LEN is nil, read until we get a null."
  (declare #.utils:*standard-optimize-settings*)

  (let (octets)
    (if (null len)
        (progn
          (setf octets (flex:with-output-to-sequence (out)
                         (do ((b (stream-read-u8 instream)
                                 (stream-read-u8 instream)))
                             (nil)
                           (when (zerop b)
                             (return))
                           (write-byte b out))))
          (setf len (length octets)))
        (setf octets  (stream-read-sequence instream len)))

    (when (= 0 len)
      (return-from stream-read-utf-8-string ""))

    (flex:octets-to-string octets :external-format :utf-8)))


;;;; Files
(defparameter *get-audio-info* t
  "Controls whether the parsing functions parse audio info like bit-rate, etc")

(defun open-audio-file (filename &optional (get-audio-info *get-audio-info*))
  "Open and parse FILENAME for tag and optionally audio info. Closes underlying
file upon return."
  (declare #.utils:*standard-optimize-settings*)

  (let ((stream)
        (info))

    (unwind-protect
         (progn
           (setf stream (make-audio-stream filename))
           (when stream
             (setf info
                   (cond ((id3:is-valid-mp3-file stream)
                          (id3:parse-audio-file stream get-audio-info))
                         ((m4a:is-valid-m4-file stream)
                          (m4a:parse-audio-file stream get-audio-info))
                         ((flac:is-valid-flac-file stream)
                          (flac:parse-audio-file stream get-audio-info))
                         (t nil)))))
      (when stream
        (close stream)))
      info))
