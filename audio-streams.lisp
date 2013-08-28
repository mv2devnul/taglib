;;; -*- Mode: Lisp;  show-trailing-whitespace: t; Base: 10; indent-tabs: nil; Syntax: ANSI-Common-Lisp; Package: AUDIO-STREAMS; -*-
;;; Copyright (c) 2013, Mark VandenBrink. All rights reserved.

(in-package #:audio-streams)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defconstant +optimize-fastest+ '(optimize (speed 3) (safety 0) (debug 0)))
  (defmacro fastest (&body body)
    `(locally (declare ,+optimize-fastest+)
       ,@body)))

(log5:defcategory cat-log-stream)
(defmacro log-stream (&rest log-stuff) `(log5:log-for (cat-log-stream) ,@log-stuff))

(define-condition audio-stream-condition ()
  ((location :initarg :location :reader location :initform nil)
   (object   :initarg :object   :reader object   :initform nil)
   (messsage :initarg :message  :reader message  :initform "Undefined Condition"))
  (:report (lambda (condition stream)
             (format stream "audio-stream condition at location: <~a> with object: <~a>: message: <~a>"
                     (location condition) (object condition) (message condition)))))

(defmethod print-object ((me audio-stream-condition) stream)
  (format stream "location: <~a>, object: <~a>, message: <~a>" (location me) (object me) (message me)))

(deftype octet () '(unsigned-byte 8))
(defmacro make-octets (len) `(make-array ,len :element-type 'octet))

(defclass mem-stream ()
   ((stream-filename :accessor stream-filename :initform nil :initarg :stream-filename)
    (index           :accessor index           :initform 0)
    (stream-size     :accessor stream-size     :initform 0)
    (vect            :accessor vect            :initform nil :initarg :vect))
   (:documentation "A thin-wrapper class over mmaped-files and/or vectors"))

 (defmacro with-mem-stream-slots ((instance) &body body)
   `(with-slots (stream-filename index stream-size vect) ,instance
      (declare (integer index stream-size)
               (type (array (unsigned-byte 8) 1) vect))
      ,@body))

 (defun make-mem-stream (v) (make-instance 'mem-stream :vect v))
 (defun make-mmap-stream (f) (make-instance 'mem-stream :stream-filename f))

 (defmethod initialize-instance :after ((stream mem-stream) &key)
   (with-mem-stream-slots (stream)
     (when stream-filename
       (setf vect (ccl:map-file-to-octet-vector stream-filename)))
     (setf stream-size (length vect))))

 (defmethod stream-close ((stream mem-stream))
   (with-mem-stream-slots (stream)
     (when stream-filename
       (ccl:unmap-octet-vector vect))
     (setf vect nil)))

 (defmethod stream-seek ((stream mem-stream) &optional (offset 0) (from :current))
   (with-mem-stream-slots (stream)
     (ecase from
       (:start (setf index offset))
       (:current
        (if (zerop offset)
            index
            (incf index offset)))
        (:end (setf index (- stream-size offset))))))

(defun read-n-bytes (stream n-bytes &key (bits-per-byte 8))
  (fastest
    (with-mem-stream-slots (stream)
      (when (<= (+ index n-bytes) stream-size)
        (loop with value = 0
              for low-bit downfrom (* bits-per-byte (1- n-bytes)) to 0 by bits-per-byte do
                (setf (ldb (byte bits-per-byte low-bit) value) (aref vect index))
                (incf index)
              finally (return-from read-n-bytes value))))
    nil))

 (declaim (inline read-n-bytes))

 (defmethod stream-read-u8  ((stream mem-stream) &key (bits-per-byte 8)) (read-n-bytes stream 1 :bits-per-byte bits-per-byte))
 (defmethod stream-read-u16 ((stream mem-stream) &key (bits-per-byte 8)) (read-n-bytes stream 2 :bits-per-byte bits-per-byte))
 (defmethod stream-read-u24 ((stream mem-stream) &key (bits-per-byte 8)) (read-n-bytes stream 3 :bits-per-byte bits-per-byte))
 (defmethod stream-read-u32 ((stream mem-stream) &key (bits-per-byte 8)) (read-n-bytes stream 4 :bits-per-byte bits-per-byte))
 (defmethod stream-read-u64 ((stream mem-stream) &key (bits-per-byte 8)) (read-n-bytes stream 8 :bits-per-byte bits-per-byte))

 (defmethod stream-read-sequence ((stream mem-stream) size &key (bits-per-byte 8))
   (fastest
     (with-mem-stream-slots (stream)
       (when (> (+ index size) stream-size)
         (setf size (- stream-size index)))
       (ecase bits-per-byte
         (8 (let ((octets (make-array size :element-type 'octet :displaced-to vect :displaced-index-offset index :adjustable nil)))
              (incf index size)
              (values octets size)))
         (7
          (let* ((last-byte-was-FF nil)
                 (byte nil)
                 (octets (ccl:with-output-to-vector (out)
                           (dotimes (i size)
                             (setf byte (stream-read-u8 stream))
                             (if last-byte-was-FF
                                 (if (not (zerop byte))
                                     (write-byte byte out))
                                 (write-byte byte out))
                             (setf last-byte-was-FF (= byte #xFF))))))
            (values octets size)))))))

 (defclass mp3-file-stream (mem-stream)
   ((id3-header :accessor id3-header :initform nil :documentation "holds all the ID3 info")
    (audio-info :accessor audio-info :initform nil :documentation "holds the bit-rate, etc info"))
   (:documentation "Stream for parsing MP3 files"))

 (defclass mp4-file-stream (mem-stream)
   ((mp4-atoms  :accessor mp4-atoms  :initform nil :documentation "holds tree of parsed MP4 atoms/boxes")
    (audio-info :accessor audio-info :initform nil :documentation "holds the bit-rate, etc info"))
   (:documentation "Stream for parsing MP4A files"))

 (defun make-file-stream (filename)
   "Convenience function for creating a file stream."
   (let* ((new-stream (make-mmap-stream filename))
          (ret-stream))
     (cond ((mp4-atom:is-valid-m4-file new-stream)
            (setf ret-stream (make-instance 'mp4-file-stream :vect (vect new-stream) :stream-filename (stream-filename new-stream))))
           ((id3-frame:is-valid-mp3-file new-stream)
            (setf ret-stream (make-instance 'mp3-file-stream :vect (vect new-stream) :stream-filename (stream-filename new-stream)))))
     (stream-close new-stream)
     ret-stream))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Strings ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;
;;; Decode octets as an iso-8859-1 string (encoding == 0)
(defun stream-decode-iso-string (octets &key (start 0) (end nil))
  (ccl:decode-string-from-octets octets :start start :end end :external-format :iso-8859-1))

;;;
;;; XXX: Coded this way because I can't seem to get a simple :external-format :ucs-2 to work correctly
;;; AND some taggers encode a UCS-2 empty string w/o a byte-order mark (i.e. null strings are
;;; sometimes encoded as #(00 00))
(defun stream-decode-ucs-string (octets &key (start 0) (end nil))
  "Decode octets as a UCS string with a BOM (encoding == 1)"
    (labels ((get-byte-order-mark (octets)
               (let ((retval 0))
                 (setf (ldb (byte 8 0) retval) (aref octets 1))
                 (setf (ldb (byte 8 8) retval) (aref octets 0))
                 (when (not (or (= #xfffe retval) (= #xfeff retval)))
                   (error 'audio-stream-condition
                          :location "stream-decode-ucs-string"
                          :object nil
                          :message (format nil "got an invalid byte-order mark of ~x" retval)))
                 retval)))

      ;; special case: empty (and mis-coded) string
      (cond ((zerop (length octets))
             (make-string 0))
            (t
             ;;
             ;; else, we have a (hopefully) properly encoded string
             (let ((bom (get-byte-order-mark octets)))
               (ecase (the fixnum bom)
                 (#xfffe (ccl:decode-string-from-octets octets :start (+ 2 start) :end end :external-format :ucs-2le))
                 (#xfeff (ccl:decode-string-from-octets octets :start (+ 2 start) :end end :external-format :ucs-2be))
                 (0      (make-string 0))))))))

(defun stream-decode-ucs-be-string (octets &key (start 0) (end nil))
  "Decode octets as a UCS-BE string (encoding == 2)"
  (ccl:decode-string-from-octets octets :start start :end end :external-format :ucs-2be))

(defun stream-decode-utf-8-string (octets &key (start 0) (end nil))
  "Decode octets as a utf-8 string"
  (ccl:decode-string-from-octets octets :start start :end end :external-format :utf-8))

(defun stream-decode-string (octets &key (start 0) (end nil) (encoding 0))
  "Decode octets depending on encoding"
  (ecase encoding
    (0 (stream-decode-iso-string octets    :start start :end end))
    (1 (stream-decode-ucs-string octets    :start start :end end))
    (2 (stream-decode-ucs-be-string octets :start start :end end))
    (3 (stream-decode-utf-8-string octets  :start start :end end))))

(defmethod stream-read-iso-string-with-len ((instream mem-stream) len)
  "Read an iso-8859-1 string of length 'len' (encoding = 0)"
  (let ((octets (stream-read-sequence instream len)))
    (stream-decode-iso-string octets)))

(defmethod stream-read-ucs-string-with-len ((instream mem-stream) len)
  "Read an ucs-2 string of length 'len' (encoding = 1)"
  (let ((octets (stream-read-sequence instream len)))
      (stream-decode-ucs-string octets)))

(defmethod stream-read-ucs-be-string-with-len ((instream mem-stream) len)
  "Read an ucs-2-be string of length 'len' (encoding = 2)"
  (let ((octets (stream-read-sequence instream len)))
    (stream-decode-ucs-be-string octets)))

(defmethod stream-read-utf-8-string-with-len ((instream mem-stream) len)
  "Read an utf-8 string of length 'len' (encoding = 3)"
  (let ((octets (stream-read-sequence instream len)))
    (stream-decode-utf-8-string octets)))

(defmethod stream-read-string-with-len ((instream mem-stream) len &key (encoding 0))
  "Read in a string of a given encoding of length 'len'"
  (ecase encoding
    (0 (stream-read-iso-string-with-len instream len))
    (1 (stream-read-ucs-string-with-len instream len))
    (2 (stream-read-ucs-be-string-with-len instream len))
    (3 (stream-read-utf-8-string-with-len instream len))))

(defmethod stream-read-iso-string ((instream mem-stream))
  "Read in a null terminated iso-8859-1 string"
  (let ((octets (ccl:with-output-to-vector (out)
                  (do ((b (stream-read-u8 instream) (stream-read-u8 instream)))
                      (nil)
                    (when (zerop b)
                      (return))         ; leave loop w/o writing
                    (write-byte b out)))))
    (stream-decode-iso-string octets)))

(defmethod stream-read-ucs-string ((instream mem-stream))
  "Read in a null terminated UCS string."
  (let ((octets (ccl:with-output-to-vector (out)
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

(defmethod stream-read-ucs-be-string ((instream mem-stream))
  "Read in a null terminated UCS-BE string."
  (let ((octets (ccl:with-output-to-vector (out)
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

(defmethod stream-read-utf-8-string ((instream mem-stream))
  "Read in a null terminated utf-8 string (encoding == 3)"
  (let ((octets (ccl:with-output-to-vector (out)
                  (do ((b (stream-read-u8 instream)
                          (stream-read-u8 instream)))
                      (nil)
                    (when (zerop b)
                      (return))
                    (write-byte b out)))))
    (stream-decode-utf-8-string octets)))

(defmethod stream-read-string ((instream mem-stream) &key (encoding 0))
  "Read in a null terminated string of a given encoding."
  (ecase encoding
    (0 (stream-read-iso-string    instream))
    (1 (stream-read-ucs-string    instream))
    (2 (stream-read-ucs-be-string instream))
    (3 (stream-read-utf-8-string  instream))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Files ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar *get-audio-info* t "controls whether the parsing functions also parse audio info like bit-rate, etc")

(defmethod parse-audio-file ((stream mp4-file-stream) &key (get-audio-info *get-audio-info*) &allow-other-keys)
  "Parse an MP4A file by reading it's ATOMS and decoding them."
  (handler-case
      (progn
        (mp4-atom:find-mp4-atoms stream)
        (when get-audio-info
          (setf (audio-info stream) (mp4-atom:get-mp4-audio-info stream))))
    (mp4-atom:mp4-atom-condition (c)
      (utils:warn-user "make-mp4-stream got condition: ~a" c))))


(defmethod parse-audio-file ((stream mp3-file-stream) &key (get-audio-info *get-audio-info*) &allow-other-keys)
  "Parse an MP3 file by reading it's FRAMES and decoding them."
  (handler-case
      (progn
        (id3-frame:find-id3-frames stream)
        (when get-audio-info
          (setf (audio-info stream) (mpeg:get-mpeg-audio-info stream))))
    (id3-frame:id3-frame-condition (c)
      (utils:warn-user "make-mp3-stream got condition: ~a" c))))
