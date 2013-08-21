;;; -*- Mode: Lisp;  show-trailing-whitespace: t; Base: 10; indent-tabs: nil; Syntax: ANSI-Common-Lisp; Package: STREAMS; -*-
;;; Copyright (c) 2013, Mark VandenBrink. All rights reserved.

(in-package #:audio-streams)

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

;;;
;;; A simple stream interface for parsing audio files.  Currently, we have two basic stream types:
;;; file-based and in-memory based, both of which implement the stream protocol of read, seek, etc.
;;;

;;; Not prefixing this with #+USE-MMAP so as to make stream seek easier
(defclass mmap-stream-mixin ()
  ((orig-vector :accessor orig-vector))
  (:documentation "Use CCLs MMAP facility to get a stream."))

(defclass base-stream ()
  ((stream :accessor stream))
  (:documentation "Base class for audio-stream implementation"))

(defclass base-file-stream #-USE-MMAP (base-stream) #+USE-MMAP (base-stream mmap-stream-mixin)
          ((stream-filename :accessor stream-filename)
           (orig-size   :accessor orig-size :documentation "ccl::stream-position let's you seek beyond EOF"))
          (:documentation "File-based audio stream"))

(defclass mp3-file-stream (base-file-stream)
  ((id3-header :accessor id3-header :initform nil :documentation "holds all the ID3 info")
   (audio-info :accessor audio-info :initform nil :documentation "holds the bit-rate, etc info"))
  (:documentation "Stream for parsing MP3 files"))

(defclass mp4-file-stream (base-file-stream)
  ((mp4-atoms  :accessor mp4-atoms  :initform nil :documentation "holds tree of parsed MP4 atoms/boxes")
   (audio-info :accessor audio-info :initform nil :documentation "holds the bit-rate, etc info"))
  (:documentation "Stream for parsing MP4A files"))

(defun make-file-stream (class-name filename &key (read-only t))
  "Convenience function for creating a file stream."
  (let ((new-stream (make-instance (find-class class-name))))

    #-USE-MMAP (progn
                 (setf (stream new-stream) (if read-only
                                               (open filename :direction :input :element-type 'octet)
                                               (open filename :direction :io :if-exists :overwrite :element-type 'octet)))
                 (setf (orig-size new-stream) (file-length (stream new-stream))))
    #+USE-MMAP (progn
                 (assert read-only () "Can not do read/write with MMAP files.")
                 (setf (orig-vector new-stream) (ccl:map-file-to-octet-vector filename))
                 (setf (orig-size new-stream) (length (orig-vector new-stream))) ; ccl::stream-position let's you seek beyond EOF
                 (setf (stream new-stream) (ccl:make-vector-input-stream (orig-vector new-stream))))

    (setf (stream-filename new-stream) filename)
    new-stream))

(defclass base-mem-stream (base-stream)
  ()
  (:documentation "In-memory stream"))

(defun make-mem-stream (vector)
  "Convenience function to turn a vector into a stream."
  (let ((new-stream (make-instance 'base-mem-stream)))
    (setf (stream new-stream) (ccl:make-vector-input-stream vector))
    new-stream))


(defmethod stream-close ((in-stream base-file-stream))
  "Close the underlying file."
  (with-slots (stream) in-stream
    (when stream
      #-USE-MMAP (close stream)
      #+USE-MMAP (ccl:unmap-octet-vector (orig-vector in-stream))
      (setf stream nil))))

(defmethod stream-close ((in-stream base-mem-stream))
  "'Close' a memory stream by setting it to nil"
  (with-slots (stream) in-stream
    (setf stream nil)))

(defmethod stream-size ((in-stream base-stream))
  "Returns the length of the underlying stream"
  (ccl::stream-length (stream in-stream)))

;;;
;;; I'm using ccl::stream-position, which I really shouldn't here...
(defmethod stream-seek ((in-stream base-stream) &optional (offset 0) (from :current))
  "C-like stream positioner.  Takes an offset and a location (one of :start, :end, :current).
If offset is not passed, then assume 0.  If from is not passed, assume from current location.
Thus (stream-seek in) == (stream-seek in 0 :current)"
  (with-slots (stream) in-stream
    (ecase from
      (:start
       (when (or (typep in-stream 'mmap-stream-mixin) (typep in-stream 'base-file-stream))
         (if (> offset (orig-size in-stream))
             (error 'audio-stream-condition :location "stream-seek" :object in-stream :message "Seeking beyond end of file")))
       (ccl::stream-position stream offset))
      (:current
       (if (zerop offset)
           (ccl::stream-position stream)
           (progn
             (when (or (typep in-stream 'mmap-stream-mixin) (typep in-stream 'base-file-stream))
               (if (> (+ (ccl::stream-position stream) offset) (orig-size in-stream))
                   (error 'audio-stream-condition :location "stream-seek" :object in-stream :message "Seeking beyond end of file")))
             (ccl::stream-position stream (+ (ccl::stream-position stream) offset)))))
       (:end
        (when (or (typep in-stream 'mmap-stream-mixin) (typep in-stream 'base-file-stream))
          (if (> (- (ccl::stream-length stream) offset) (orig-size in-stream))
              (error 'audio-stream-condition :location "stream-seek" :object in-stream :message "Seeking beyond end of file")))
        (ccl::stream-position stream (- (ccl::stream-length stream) offset))))))

(defun stream-read-octets (instream bytes &key (bits-per-byte 8))
  "Used to slurp in octets for the stream-read-* methods"
  (loop with value = 0
        for low-bit downfrom (* bits-per-byte (1- bytes)) to 0 by bits-per-byte do
          (setf (ldb (byte bits-per-byte low-bit) value) (read-byte instream))
        finally (return value)))

(defmethod stream-read-u8 ((in-stream base-stream) &key (bits-per-byte 8))
  "Read 1 byte from file"
  (with-slots (stream) in-stream
    (stream-read-octets stream 1 :bits-per-byte bits-per-byte)))

(defmethod stream-read-u16 ((in-stream base-stream) &key (bits-per-byte 8))
  "Read 2 bytes from file"
  (with-slots (stream) in-stream
    (stream-read-octets stream 2 :bits-per-byte bits-per-byte)))

(defmethod stream-read-u24 ((in-stream base-stream) &key (bits-per-byte 8))
  "Read 3 bytes from file"
  (with-slots (stream) in-stream
    (stream-read-octets stream 3 :bits-per-byte bits-per-byte)))

(defmethod stream-read-u32 ((in-stream base-stream) &key (bits-per-byte 8))
  "Read 4 bytes from file"
  (with-slots (stream) in-stream
    (stream-read-octets stream 4 :bits-per-byte bits-per-byte)))

(defmethod stream-read-u64 ((in-stream base-stream) &key (bits-per-byte 8))
  "Read 8 bytes from file"
  (with-slots (stream) in-stream
    (stream-read-octets stream 8 :bits-per-byte bits-per-byte)))

(defmethod stream-read-sequence ((stream base-stream) size &key (bits-per-byte 8))
  "Read SIZE octets from input-file in BIT-PER-BYTE sizes"
  (log5:with-context "stream-read-sequence"
    (ecase bits-per-byte
      (8
       (let ((octets (make-octets size)))
         (read-sequence octets (slot-value stream 'stream))
         octets))
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
         octets)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; STRINGS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
                   (error "got an invalid byte-order mark of ~x" retval))
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

(defmethod stream-read-iso-string-with-len ((instream base-stream) len)
  "Read an iso-8859-1 string of length 'len' (encoding = 0)"
  (let ((octets (stream-read-sequence instream len)))
    (stream-decode-iso-string octets)))

(defmethod stream-read-ucs-string-with-len ((instream base-stream) len)
  "Read an ucs-2 string of length 'len' (encoding = 1)"
  (let ((octets (stream-read-sequence instream len)))
      (stream-decode-ucs-string octets)))

(defmethod stream-read-ucs-be-string-with-len ((instream base-stream) len)
  "Read an ucs-2-be string of length 'len' (encoding = 2)"
  (let ((octets (stream-read-sequence instream len)))
    (stream-decode-ucs-be-string octets)))

(defmethod stream-read-utf-8-string-with-len ((instream base-stream) len)
  "Read an utf-8 string of length 'len' (encoding = 3)"
  (let ((octets (stream-read-sequence instream len)))
    (stream-decode-utf-8-string octets)))

(defmethod stream-read-string-with-len ((instream base-stream) len &key (encoding 0))
  "Read in a string of a given encoding of length 'len'"
  (ecase encoding
    (0 (stream-read-iso-string-with-len instream len))
    (1 (stream-read-ucs-string-with-len instream len))
    (2 (stream-read-ucs-be-string-with-len instream len))
    (3 (stream-read-utf-8-string-with-len instream len))))

(defmethod stream-read-iso-string ((instream base-stream))
  "Read in a null terminated iso-8859-1 string"
  (let ((octets (ccl:with-output-to-vector (out)
                  (do ((b (stream-read-u8 instream) (stream-read-u8 instream)))
                      (nil)
                    (when (zerop b)
                      (return))         ; leave loop w/o writing
                    (write-byte b out)))))
    (stream-decode-iso-string octets)))

(defmethod stream-read-ucs-string ((instream base-stream))
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

(defmethod stream-read-ucs-be-string ((instream base-stream))
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

(defmethod stream-read-utf-8-string ((instream base-stream))
  "Read in a null terminated utf-8 string (encoding == 3)"
  (let ((octets (ccl:with-output-to-vector (out)
                  (do ((b (stream-read-u8 instream)
                          (stream-read-u8 instream)))
                      (nil)
                    (when (zerop b)
                      (return))
                    (write-byte b out)))))
    (stream-decode-utf-8-string octets)))

(defmethod stream-read-string ((instream base-stream) &key (encoding 0))
  "Read in a null terminated string of a given encoding."
  (ecase encoding
    (0 (stream-read-iso-string    instream))
    (1 (stream-read-ucs-string    instream))
    (2 (stream-read-ucs-be-string instream))
    (3 (stream-read-utf-8-string  instream))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; FILES ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar *get-audio-info* t "controls whether the parsing functions also parse audio info like bit-rate, etc")

(defun parse-mp4-file (filename &key (get-audio-info *get-audio-info*))
  "Parse an MP4A file by reading it's ATOMS and decoding them."
  (let (stream)
    (handler-case
        (progn
          (setf stream (make-file-stream 'mp4-file-stream filename))
          (mp4-atom:find-mp4-atoms stream)
          (when get-audio-info
            (setf (audio-info stream) (mp4-atom:get-mp4-audio-info stream))))
      (mp4-atom:mp4-atom-condition (c)
        (warn-user "make-mp4-stream got condition: ~a" c)
        (when stream (stream-close stream))
        (setf stream nil)))
    stream))

(defun parse-mp3-file (filename &key (get-audio-info *get-audio-info*))
  "Parse an MP3 file by reading it's FRAMES and decoding them."
  (let (stream)
      (handler-case
          (progn
            (setf stream (make-file-stream 'mp3-file-stream filename))
            (id3-frame:find-id3-frames stream)
            (when get-audio-info
              (setf (audio-info stream) (mpeg:get-mpeg-audio-info stream))))
        (id3-frame:id3-frame-condition (c)
          (warn-user "make-mp3-stream got condition: ~a" c)
          (when stream (stream-close stream))
          (setf stream nil)))
    stream))
