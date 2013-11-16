;;; -*- Mode: Lisp;  show-trailing-whitespace: t; Base: 10; indent-tabs: nil; Syntax: ANSI-Common-Lisp; Package: AUDIO-STREAMS; -*-
;;; Copyright (c) 2013, Mark VandenBrink. All rights reserved.

(in-package #:audio-streams)

(log5:defcategory cat-log-stream)
(defmacro log-stream (&rest log-stuff) `(log5:log-for (cat-log-stream) ,@log-stuff))

(deftype octet () '(unsigned-byte 8))
(defmacro make-octets (len) `(make-array ,len :element-type 'octet))

(defclass mem-stream ()
   ((stream-filename :accessor stream-filename :initform nil :initarg :stream-filename :documentation "if set, then MMAP file")
    (index           :accessor index           :initform 0)
    (stream-size     :accessor stream-size     :initform 0)
    (vect            :accessor vect            :initform nil :initarg :vect :documentation "if set, then the vector we want STREAM-ize"))
   (:documentation "A thin-wrapper class over mmaped-files and/or vectors."))

(defmacro with-mem-stream-slots ((instance) &body body)
  `(with-slots (stream-filename index stream-size vect) ,instance
     (declare (fixnum index stream-size)
              (type (or (array (unsigned-byte 8) 1) null) vect))
     ,@body))

(defun make-mem-stream (v) (make-instance 'mem-stream :vect v))
(defun make-mmap-stream (f) (make-instance 'mem-stream :stream-filename f))

(defmethod initialize-instance :after ((stream mem-stream) &key)
  "Stream initializer. If STREAM-FILENAME is set, MMAP a the file. Else, we assume VECT was set."
  (with-mem-stream-slots (stream)
    (when stream-filename
      #+CCL (setf vect (ccl:map-file-to-octet-vector stream-filename))
      #-CCL (error "Not Yet!")
      )
    (setf stream-size (length vect))))

(defmethod stream-close ((stream mem-stream))
  "Close a stream, making the underlying object (file or vector) inaccessible."
  (declare #.utils:*standard-optimize-settings*)
  (with-mem-stream-slots (stream)
    (when stream-filename
      #+CCL (ccl:unmap-octet-vector vect)
      #-CCL (error "Not Yet")
      )
    (setf vect nil)))

;;; finding out current file position is so common, we also
;;; provide a macro
(defmacro stream-here (stream) `(index ,stream))

(defmethod stream-seek ((stream mem-stream) &optional (offset 0) (from :current))
  "Set INDEX to requested value.  No error checking done here, but subsequent reads will fail if INDEX is out-of-bounds.
As a convenience, OFFSET and FROM are optional, so (STREAM-SEEK stream) returns the current read-offset in stream."
  (declare #.utils:*standard-optimize-settings*)
  (declare (fixnum offset))
(with-mem-stream-slots (stream)
    (ecase from
      (:start                  ; INDEX set to OFFSET from start of stream
       (setf index offset))
      (:current                ; INDEX set relative to current INDEX, by OFFSET bytes
       (if (zerop offset)
           index
           (incf index offset)))
      (:end                    ; INDEX set to OFFSET from end of stream
       (setf index (- stream-size offset))))))

(declaim (inline read-n-bytes))

(defun read-n-bytes (stream n-bytes &key (bits-per-byte 8) (endian :little-endian))
  "Returns a FIXNUM constructed by reading N-BYTES.  BITS-PER-BYTE contols how many bits should be used from each read byte."
  (declare #.utils:*standard-optimize-settings*)
  (declare (fixnum n-bytes))
  (with-mem-stream-slots (stream)
    (when (<= (+ index n-bytes) stream-size)
      (ecase endian
        (:little-endian
         (loop with value = 0
               for low-bit downfrom (* bits-per-byte (1- n-bytes)) to 0 by bits-per-byte do
                 (setf (ldb (byte bits-per-byte low-bit) value) (aref vect index))
                 (incf index)
               finally (return-from read-n-bytes value)))
        (:big-endian
         (loop with value = 0
               for low-bit upfrom 0 to (* bits-per-byte (1- n-bytes)) by bits-per-byte do
                 (setf (ldb (byte bits-per-byte low-bit) value) (aref vect index))
                 (incf index)
               finally (return-from read-n-bytes value))))))
    nil)

(defun stream-read-u8 (stream)
  (declare #.utils:*standard-optimize-settings*)
  (with-mem-stream-slots (stream)
    (if (<= (+ index 1) stream-size)
        (let ((val (aref vect index)))
          (incf index)
          val)
        nil)))

(defun stream-read-u16  (stream &key (bits-per-byte 8) (endian :little-endian)) (read-n-bytes stream 2  :bits-per-byte bits-per-byte :endian endian))
(defun stream-read-u24  (stream &key (bits-per-byte 8) (endian :little-endian)) (read-n-bytes stream 3  :bits-per-byte bits-per-byte :endian endian))
(defun stream-read-u32  (stream &key (bits-per-byte 8) (endian :little-endian)) (read-n-bytes stream 4  :bits-per-byte bits-per-byte :endian endian))
(defun stream-read-u64  (stream &key (bits-per-byte 8) (endian :little-endian)) (read-n-bytes stream 8  :bits-per-byte bits-per-byte :endian endian))
(defun stream-read-u128 (stream &key (bits-per-byte 8) (endian :little-endian)) (read-n-bytes stream 16 :bits-per-byte bits-per-byte :endian endian))

(defmethod stream-read-sequence ((stream mem-stream) size &key (bits-per-byte 8))
  "Read in a sequence of octets at BITS-PER-BYTE.  If BITS-PER-BYTE == 8, then simply return
a displaced array from STREAMs underlying vector.  If it is == 7, then we have to create a new vector and read into that."
  (declare #.utils:*standard-optimize-settings*)
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
                (octets
                  #-CCL (error "Not yet")
                  #+CCL  (ccl:with-output-to-vector (out)
                           (dotimes (i size)
                             (setf byte (stream-read-u8 stream))
                             (if last-byte-was-FF
                                 (if (not (zerop byte))
                                     (write-byte byte out))
                                 (write-byte byte out))
                             (setf last-byte-was-FF (= byte #xFF))))
                  ))
           (values octets size))))))

(defclass mp3-file-stream (mem-stream)
  ((id3-header :accessor id3-header :initform nil :documentation "holds all the ID3 info")
   (audio-info :accessor audio-info :initform nil :documentation "holds the bit-rate, etc info"))
  (:documentation "Stream for parsing MP3 files"))

(defclass mp4-file-stream (mem-stream)
  ((mp4-atoms  :accessor mp4-atoms  :initform nil :documentation "holds tree of parsed MP4 atoms/boxes")
   (audio-info :accessor audio-info :initform nil :documentation "holds the bit-rate, etc info"))
  (:documentation "Stream for parsing MP4 audio files"))

(defclass flac-file-stream (mem-stream)
  ((flac-headers :accessor flac-headers :initform nil :documentation "holds all the flac headers in file")
   (audio-info   :accessor audio-info   :initform nil :documentation "parsed audio info")
   (flac-tags    :accessor flac-tags    :initform nil :documentation "parsed comment tags."))
  (:documentation "Stream for parsing flac files"))

(defun make-file-stream (filename)
  "Convenience function for creating a file stream. Detects file type and returns proper type stream."
  (declare #.utils:*standard-optimize-settings*)
  (log5:with-context "make-file-stream"
    (let* ((new-stream (make-mmap-stream filename))
           (ret-stream))

      (log-stream "Looking at ~a" filename)
      ;; detect file type and make RET-STREAM.  if we don't recognize stream, RET-STREAM will be NULL
      (cond ((mp4-atom:is-valid-m4-file new-stream)
             (log-stream "~a is an MP4 file" filename)
             (setf ret-stream (make-instance 'mp4-file-stream :vect (vect new-stream) :stream-filename (stream-filename new-stream))))
            ((flac-frame:is-valid-flac-file new-stream)
             (log-stream "~a is a FLAC file" filename)
             (setf ret-stream (make-instance 'flac-file-stream :vect (vect new-stream) :stream-filename (stream-filename new-stream))))
            ((id3-frame:is-valid-mp3-file new-stream)
             (log-stream "~a is an ID3 file" filename)
             (setf ret-stream (make-instance 'mp3-file-stream :vect (vect new-stream) :stream-filename (stream-filename new-stream))))
            (t
             (log-stream "Unkown file type")))
      (stream-close new-stream)
      ret-stream)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Strings ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Decode octets as an iso-8859-1 string (encoding == 0)
(defun stream-decode-iso-string (octets &key (start 0) (end nil))
  (declare #.utils:*standard-optimize-settings*)
  #+CCL (ccl:decode-string-from-octets octets :start start :end end :external-format :iso-8859-1)
  #-CCL (error "Not Yet")
  )

;;;
;;; XXX: Coded this way because I can't seem to get a simple :external-format :ucs-2 to work correctly
;;; AND some taggers encode a UCS-2 empty string w/o a byte-order mark (i.e. null strings are
;;; sometimes encoded as #(00 00))
(defun stream-decode-ucs-string (octets &key (start 0) (end nil))
  "Decode octets as a UCS string with a BOM (encoding == 1)"
  (declare #.utils:*standard-optimize-settings*)
  (labels ((get-byte-order-mark (octets)
             (let ((retval 0))
               (setf (ldb (byte 8 0) retval) (aref octets 1)
                     (ldb (byte 8 8) retval) (aref octets 0))
               (when (not (or (= #xfffe retval) (= #xfeff retval)))
                 (error "Got invalid byte-order mark of ~x in STREAM-DECODE-UCS-STRING" retval))
               retval)))

    ;; special case: empty (and mis-coded) string
    (cond ((zerop (length octets))
           (make-string 0))
          (t
           ;;
           ;; else, we have a (hopefully) properly encoded string
           (let ((bom (get-byte-order-mark octets)))
             (ecase (the fixnum bom)
               (#xfffe #+CCL (ccl:decode-string-from-octets octets :start (+ 2 start) :end end :external-format :ucs-2le)
                       #-CCL (error "Not Yet")
                       )
               (#xfeff #+CCL (ccl:decode-string-from-octets octets :start (+ 2 start) :end end :external-format :ucs-2be)
                       #-CCL (error "Not Yet")
                       )
               (0      (make-string 0))))))))

(defun stream-decode-ucs-be-string (octets &key (start 0) (end nil))
  "Decode octets as a UCS-BE string (encoding == 2)"
  (declare #.utils:*standard-optimize-settings*)
  #+CCL (ccl:decode-string-from-octets octets :start start :end end :external-format :ucs-2be)
  #-CCL (error "Not Yet")
  )

(defun stream-decode-utf-8-string (octets &key (start 0) (end nil))
  "Decode octets as a utf-8 string"
  (declare #.utils:*standard-optimize-settings*)
  #+CCL (ccl:decode-string-from-octets octets :start start :end end :external-format :utf-8)
  #-CCL (error "Not Yet")
  )

(defun stream-decode-string (octets &key (start 0) (end nil) (encoding 0))
  "Decode octets depending on encoding"
  (declare #.utils:*standard-optimize-settings*)
  (ecase encoding
    (0 (stream-decode-iso-string octets    :start start :end end))
    (1 (stream-decode-ucs-string octets    :start start :end end))
    (2 (stream-decode-ucs-be-string octets :start start :end end))
    (3 (stream-decode-utf-8-string octets  :start start :end end))))

(defmethod stream-read-iso-string-with-len ((instream mem-stream) len)
  "Read an iso-8859-1 string of length 'len' (encoding = 0)"
  (declare #.utils:*standard-optimize-settings*)
  (stream-decode-iso-string (stream-read-sequence instream len)))

(defmethod stream-read-ucs-string-with-len ((instream mem-stream) len)
  "Read an ucs-2 string of length 'len' (encoding = 1)"
  (declare #.utils:*standard-optimize-settings*)
  (stream-decode-ucs-string (stream-read-sequence instream len)))

(defmethod stream-read-ucs-be-string-with-len ((instream mem-stream) len)
  "Read an ucs-2-be string of length 'len' (encoding = 2)"
  (declare #.utils:*standard-optimize-settings*)
  (stream-decode-ucs-be-string (stream-read-sequence instream len)))

(defmethod stream-read-utf-8-string-with-len ((instream mem-stream) len)
  "Read an utf-8 string of length 'len' (encoding = 3)"
  (declare #.utils:*standard-optimize-settings*)
  (stream-decode-utf-8-string  (stream-read-sequence instream len)))

(defmethod stream-read-string-with-len ((instream mem-stream) len &key (encoding 0))
  "Read in a string of a given encoding of length 'len'"
  (declare #.utils:*standard-optimize-settings*)
  (ecase encoding
    (0 (stream-read-iso-string-with-len instream len))
    (1 (stream-read-ucs-string-with-len instream len))
    (2 (stream-read-ucs-be-string-with-len instream len))
    (3 (stream-read-utf-8-string-with-len instream len))))

(defmethod stream-read-iso-string ((instream mem-stream))
  "Read in a null terminated iso-8859-1 string"
  (declare #.utils:*standard-optimize-settings*)
  (let ((octets #+CCL (ccl:with-output-to-vector (out)
                        (do ((b (stream-read-u8 instream) (stream-read-u8 instream)))
                            (nil)
                          (when (zerop b)
                            (return))   ; leave loop w/o writing
                          (write-byte b out)))
                #-CCL (error "Not Yet")
        ))
    (stream-decode-iso-string octets)))

(defmethod stream-read-ucs-string ((instream mem-stream))
  "Read in a null terminated UCS string."
  (declare #.utils:*standard-optimize-settings*)
  (let ((octets #+CCL (ccl:with-output-to-vector (out)
                        (do* ((b0 (stream-read-u8 instream)
                                  (stream-read-u8 instream))
                              (b1 (stream-read-u8 instream)
                                  (stream-read-u8 instream)))
                             (nil)
                          (when (and (zerop b0) (zerop b1))
                            (return))
                          (write-byte b0 out)
                          (write-byte b1 out)))
                #-CCL (error "Not Yet")
                ))
    (stream-decode-ucs-string octets)))

(defmethod stream-read-ucs-be-string ((instream mem-stream))
  "Read in a null terminated UCS-BE string."
  (declare #.utils:*standard-optimize-settings*)
  (let ((octets #+CCL (ccl:with-output-to-vector (out)
                        (do* ((b0 (stream-read-u8 instream)
                                  (stream-read-u8 instream))
                              (b1 (stream-read-u8 instream)
                                  (stream-read-u8 instream)))
                             (nil)
                          (when (and (zerop b0) (zerop b1))
                            (return))
                          (write-byte b0 out)
                          (write-byte b1 out)))
                #-CCL (error "Not Yet")
                ))
    (stream-decode-ucs-be-string octets)))

(defmethod stream-read-utf-8-string ((instream mem-stream))
  "Read in a null terminated utf-8 string (encoding == 3)"
  (declare #.utils:*standard-optimize-settings*)
  (let ((octets #+CCL (ccl:with-output-to-vector (out)
                        (do ((b (stream-read-u8 instream)
                                (stream-read-u8 instream)))
                            (nil)
                          (when (zerop b)
                            (return))
                          (write-byte b out)))
                #-CCL (error "Not Yet")
                ))
    (stream-decode-utf-8-string octets)))

(defmethod stream-read-string ((instream mem-stream) &key (encoding 0))
  "Read in a null terminated string of a given encoding."
  (declare #.utils:*standard-optimize-settings*)
  (ecase encoding
    (0 (stream-read-iso-string    instream))
    (1 (stream-read-ucs-string    instream))
    (2 (stream-read-ucs-be-string instream))
    (3 (stream-read-utf-8-string  instream))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Files ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar *get-audio-info* t "controls whether the parsing functions also parse audio info like bit-rate, etc")

(defmethod parse-audio-file ((stream mp4-file-stream) &key (get-audio-info *get-audio-info*) &allow-other-keys)
  "Parse an MP4A file by reading its ATOMS and decoding them."
  (declare #.utils:*standard-optimize-settings*)
  (handler-case
      (progn
        (mp4-atom:find-mp4-atoms stream)
        (when get-audio-info
          (setf (audio-info stream) (mp4-atom:get-mp4-audio-info stream))))
    (condition (c)
      (utils:warn-user "make-mp4-stream got condition: ~a" c))))

(defmethod parse-audio-file ((stream flac-file-stream) &key (get-audio-info *get-audio-info*) &allow-other-keys)
  "Parse a flac file by reading its headers and decoding them."
  (declare #.utils:*standard-optimize-settings*)
  (declare (ignore get-audio-info)) ; audio info comes for "free" by parsing headers
  (handler-case
      (flac-frame:find-flac-frames stream)
    (condition (c)
      (utils:warn-user "make-flac-stream got condition: ~a" c))))

(defmethod parse-audio-file ((stream mp3-file-stream) &key (get-audio-info *get-audio-info*) &allow-other-keys)
  "Parse an MP3 file by reading its FRAMES and decoding them."
  (declare #.utils:*standard-optimize-settings*)
  (handler-case
      (progn
        (id3-frame:find-id3-frames stream)
        (when get-audio-info
          (setf (audio-info stream) (mpeg:get-mpeg-audio-info stream))))
    (condition (c)
      (utils:warn-user "make-mp3-stream got condition: ~a" c))))
