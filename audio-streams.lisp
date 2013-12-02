;;; -*- Mode: Lisp;  show-trailing-whitespace: t; Base: 10; indent-tabs: nil; Syntax: ANSI-Common-Lisp; Package: AUDIO-STREAMS; -*-
;;; Copyright (c) 2013, Mark VandenBrink. All rights reserved.

(in-package #:audio-streams)

;;;; Generic stream support
(deftype octet () '(unsigned-byte 8))
(deftype octets () '(simple-array octet (*)))
(defmacro make-octets (len) `(make-array ,len :element-type 'octet))

(defun make-audio-stream (arg)
  "Creates a stream for ARG"
  (declare #.utils:*standard-optimize-settings*)
  (labels ((make-file-stream (name)
             (let ((fd (open name :direction :input :element-type 'octet)))
               (if fd
                   (flex:make-flexi-stream fd :element-type 'octet)
                   nil))))
    (etypecase arg
      (string (make-file-stream arg))
      (pathname (make-file-stream arg))
      (octets (flex:make-in-memory-input-stream arg)))))

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
              (octets (flex:with-output-to-sequence (out)
                        (dotimes (i size)
                          (setf byte (stream-read-u8 stream))
                          (if last-byte-was-FF
                              (if (not (zerop byte))
                                  (write-byte byte out))
                              (write-byte byte out))
                          (setf last-byte-was-FF (= byte #xFF))))))
         (values octets size)))))

;;;; Strings: decoders

;;; Decode octets as an iso-8859-1 string (encoding == 0)
(defun stream-decode-iso-string (octets &key (start 0) (end (length octets)))
  (declare #.utils:*standard-optimize-settings*)
  (flex:octets-to-string octets :start start
                                         :end end :external-format :iso-8859-1))

;;;
;;; XXX: Coded this way because I can't seem to get a simple :external-format
;;; :ucs-2 to work correctly AND some taggers encode a UCS-2 empty string w/o
;;; a byte-order mark (i.e. null strings are sometimes encoded as #(00 00))
(defun stream-decode-ucs-string (octets &key (start 0) (end (length octets)))
  "Decode octets as a UCS string with a BOM (encoding == 1)"
  (declare #.utils:*standard-optimize-settings*)
  (labels ((get-byte-order-mark (octets)
             (let ((retval 0))
               (setf (ldb (byte 8 0) retval) (aref octets 1)
                     (ldb (byte 8 8) retval) (aref octets 0))
               (when (not (or (= #xfffe retval) (= #xfeff retval)))
                 (error
                  "Got invalid byte-order mark of ~x in STREAM-DECODE-UCS-STRING"
                  retval))
               retval)))

    ;; special case: empty (and mis-coded) string
    (cond ((zerop (length octets))
           (make-string 0))
          (t
           ;;
           ;; else, we have a (hopefully) properly encoded string
           (when (oddp end)
             (warn-user
              "Malformed UCS string, length (~d) is odd---decrementing by 1"
              end)
             (setf end (1- end)))

           (let ((bom (get-byte-order-mark octets)))
             (ecase (the fixnum bom)
               (#xfffe (flex:octets-to-string octets
                                              :start (+ 2 start)
                                              :end end
                                              :external-format :ucs-2le))
               (#xfeff (flex:octets-to-string octets
                                              :start (+ 2 start)
                                              :end end
                                              :external-format :ucs-2be))
               (0      (make-string 0))))))))

(defun stream-decode-ucs-be-string (octets &key (start 0) (end (length octets)))
  "Decode octets as a UCS-BE string (encoding == 2)"
  (declare #.utils:*standard-optimize-settings*)
  (flex:octets-to-string octets :start start
                                :end end :external-format :ucs-2be))

(defun stream-decode-utf-8-string (octets &key (start 0) (end (length octets)))
  "Decode octets as a utf-8 string"
  (declare #.utils:*standard-optimize-settings*)
  (flex:octets-to-string octets :start start :end end :external-format :utf-8))

(defun stream-decode-string (octets &key (start 0)
                                         (end (length octets))
                                         (encoding 0))
  "Decode octets depending on encoding"
  (declare #.utils:*standard-optimize-settings*)
  (ecase encoding
    (0 (stream-decode-iso-string octets    :start start :end end))
    (1 (stream-decode-ucs-string octets    :start start :end end))
    (2 (stream-decode-ucs-be-string octets :start start :end end))
    (3 (stream-decode-utf-8-string octets  :start start :end end))))

;;;; Strings: readers
(defun stream-read-iso-string-with-len (instream len)
  "Read an iso-8859-1 string of length 'len' (encoding = 0)"
  (declare #.utils:*standard-optimize-settings*)
  (stream-decode-iso-string (stream-read-sequence instream len)))

(defun stream-read-ucs-string-with-len (instream len)
  "Read an ucs-2 string of length 'len' (encoding = 1)"
  (declare #.utils:*standard-optimize-settings*)
  (stream-decode-ucs-string (stream-read-sequence instream len)))

(defun stream-read-ucs-be-string-with-len (instream len)
  "Read an ucs-2-be string of length 'len' (encoding = 2)"
  (declare #.utils:*standard-optimize-settings*)
  (stream-decode-ucs-be-string (stream-read-sequence instream len)))

(defun stream-read-utf-8-string-with-len (instream len)
  "Read an utf-8 string of length 'len' (encoding = 3)"
  (declare #.utils:*standard-optimize-settings*)
  (stream-decode-utf-8-string  (stream-read-sequence instream len)))

(defun stream-read-string-with-len (instream len &key (encoding 0))
  "Read in a string of a given encoding of length 'len'"
  (declare #.utils:*standard-optimize-settings*)
  (ecase encoding
    (0 (stream-read-iso-string-with-len instream len))
    (1 (stream-read-ucs-string-with-len instream len))
    (2 (stream-read-ucs-be-string-with-len instream len))
    (3 (stream-read-utf-8-string-with-len instream len))))

(defun stream-read-iso-string (instream)
  "Read in a null-terminated iso-8859-1 string"
  (declare #.utils:*standard-optimize-settings*)
  (let ((octets (flex:with-output-to-sequence (out)
                  (do ((b (stream-read-u8 instream) (stream-read-u8 instream)))
                      (nil)
                    (when (zerop b)
                      (return))         ; leave loop w/o writing
                    (write-byte b out)))))
    (stream-decode-iso-string octets)))

(defun stream-read-ucs-string (instream)
  "Read in a null-terminated UCS string."
  (declare #.utils:*standard-optimize-settings*)
  (let ((octets (flex:with-output-to-sequence (out)
                  (do* ((b0 (stream-read-u8 instream)
                            (stream-read-u8 instream))
                        (b1 (stream-read-u8 instream)
                            (stream-read-u8 instream)))
                       (nil)
                    (when (and (zerop b0) (zerop b1))
                      (return))
                    (write-byte b0 out)
                    (write-byte b1 out)))))
    (stream-decode-ucs-string octets)))

(defun stream-read-ucs-be-string (instream)
  "Read in a null-terminated UCS-BE string."
  (declare #.utils:*standard-optimize-settings*)
  (let ((octets (flex:with-output-to-sequence (out)
                  (do* ((b0 (stream-read-u8 instream)
                            (stream-read-u8 instream))
                        (b1 (stream-read-u8 instream)
                            (stream-read-u8 instream)))
                       (nil)
                    (when (and (zerop b0) (zerop b1))
                      (return))
                    (write-byte b0 out)
                    (write-byte b1 out)))))
    (stream-decode-ucs-be-string octets)))

(defun stream-read-utf-8-string (instream)
  "Read in a null-terminated utf-8 string (encoding == 3)"
  (declare #.utils:*standard-optimize-settings*)
  (let ((octets (flex:with-output-to-sequence (out)
                  (do ((b (stream-read-u8 instream)
                          (stream-read-u8 instream)))
                      (nil)
                    (when (zerop b)
                      (return))
                    (write-byte b out)))))
    (stream-decode-utf-8-string octets)))

(defun stream-read-string (instream &key (encoding 0))
  "Read in a null-terminated string of a given encoding."
  (declare #.utils:*standard-optimize-settings*)
  (ecase encoding
    (0 (stream-read-iso-string    instream))
    (1 (stream-read-ucs-string    instream))
    (2 (stream-read-ucs-be-string instream))
    (3 (stream-read-utf-8-string  instream))))

;;;; Files
(defvar *get-audio-info* t
  "controls whether the parsing functions parse audio info like bit-rate, etc")

(defun open-audio-file (filename &optional (get-audio-info *get-audio-info*))
  "Open and parse FILENAME for tag and optionally audio info"
  (declare #.utils:*standard-optimize-settings*)
  (let ((stream)
        (info))

    (unwind-protect
         (progn
           (setf stream (make-audio-stream filename))
           (when stream
             (setf info
                   (cond ((id3-frame:is-valid-mp3-file stream)
                          (id3-frame:parse-audio-file stream get-audio-info))
                         ((mp4-atom:is-valid-m4-file stream)
                          (mp4-atom:parse-audio-file stream get-audio-info))
                         ((flac-frame:is-valid-flac-file stream)
                          (flac-frame:parse-audio-file stream get-audio-info))
                         (t nil)))))
      (when stream
        (close stream)))
      info))
