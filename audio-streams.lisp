;;; -*- Mode: Lisp;  show-trailing-whitespace: t; Base: 10; indent-tabs: nil; Syntax: ANSI-Common-Lisp; Package: STREAMS; -*-
;;; Copyright (c) 2013, Mark VandenBrink. All rights reserved.

(in-package #:audio-streams)

(log5:defcategory cat-log-stream)
(defmacro log-stream (&rest log-stuff) `(log5:log-for (cat-log-stream) ,@log-stuff))

(deftype octet () '(unsigned-byte 8))
(defmacro make-octets (len) `(make-array ,len :element-type 'octet))

(defclass base-stream ()
  ((stream :accessor stream)))

(defclass base-file-stream (base-stream)
  ((stream-filename :accessor stream-filename)))

(defclass mp3-file-stream (base-file-stream)
  ((id3-header  :accessor id3-header)
   (mpeg-info   :accessor mpeg-info :initform nil)))

(defclass mp4-file-stream (base-file-stream)
  ((mp4-atoms :accessor mp4-atoms :initform nil)))

(defun make-file-stream (class-name filename &key (read-only t))
  (let ((new-stream (make-instance (find-class class-name))))
	(setf (stream new-stream) (if read-only
								  (open filename :direction :input :element-type 'octet)
								  (open filename :direction :io :if-exists :overwrite :element-type 'octet)))
	(setf (stream-filename new-stream) filename)
	new-stream))

(defclass base-mem-stream (base-stream) ())

(defun make-mem-stream (vector)
  (let ((new-stream (make-instance 'base-mem-stream)))
	(setf (stream new-stream) (ccl:make-vector-input-stream vector))
	new-stream))

(defmethod stream-close ((in-stream base-file-stream))
  (with-slots (stream) in-stream
	(when stream
	  (close stream)
	  (setf stream nil))))

(defmethod stream-close ((in-stream base-mem-stream))
  (with-slots (stream) in-stream
	(setf stream nil)))

(defmethod stream-size ((in-stream base-stream))
  (ccl::stream-length (stream in-stream)))

(defmethod stream-seek ((in-stream base-stream) offset from)
  (with-slots (stream) in-stream
	(ecase from
	  (:start (ccl::stream-position stream offset))
	  (:current (if (zerop offset)
					(ccl::stream-position stream)
					(ccl::stream-position stream (+ (ccl::stream-position stream) offset))))
	  (:end (ccl::stream-position stream (- (ccl::stream-length stream) offset))))))

(defun stream-read-octets (instream bytes &key (bits-per-byte 8))
  (loop with value = 0
		for low-bit downfrom (* bits-per-byte (1- bytes)) to 0 by bits-per-byte do
		  (setf (ldb (byte bits-per-byte low-bit) value) (read-byte instream))
		finally (return value)))

(defmethod stream-read-u8 ((in-stream base-stream) &key (bits-per-byte 8))
  "read 1 byte from file"
  (with-slots (stream) in-stream
	(stream-read-octets stream 1 :bits-per-byte bits-per-byte)))

(defmethod stream-read-u16 ((in-stream base-stream) &key (bits-per-byte 8))
  "read 2 bytes from file"
  (with-slots (stream) in-stream
	(stream-read-octets stream 2 :bits-per-byte bits-per-byte)))

(defmethod stream-read-u24 ((in-stream base-stream) &key (bits-per-byte 8))
  "read 3 bytes from file"
  (with-slots (stream) in-stream
	(stream-read-octets stream 3 :bits-per-byte bits-per-byte)))

(defmethod stream-read-u32 ((in-stream base-stream) &key (bits-per-byte 8))
  "read 4 bytes from file"
  (with-slots (stream) in-stream
	(stream-read-octets stream 4 :bits-per-byte bits-per-byte)))

;; (defmethod stream-read-string ((stream base-stream) &key size (terminators nil))
;;   "Read normal string from stream. If size is provided, read exactly that many octets.
;; If terminators is supplied, it is a list of characters that can terminate a string (and hence stop read)"
;;   (with-output-to-string (s)
;; 	(with-slots (stream) stream
;; 	  (let ((terminated nil)
;; 			(count 0)
;; 			(byte))
;; 		(loop
;; 		  (when (if size (= count size) terminated) (return))
;; 		  (setf byte (read-byte stream))
;; 		  (incf count)
;; 		  (when (member byte terminators :test #'=)
;; 			(setf terminated t))
;; 		  (when (not terminated)
;; 			(write-char (code-char byte) s)))))))

(defmethod stream-read-sequence ((stream base-stream) size &key (bits-per-byte 8))
  "Read SIZE octets from input-file in BIT-PER-BYTE sizes"
  (log5:with-context "stream-read-sequence"
	(ecase bits-per-byte
	  (8
	   (log-stream "reading ~:d bytes as 8-bit sequence" size)
	   (let ((octets (make-octets size)))
		 (read-sequence octets (slot-value stream 'stream))
		 octets))
	  (7
	   (log-stream "reading ~:d bytes as 7-bit sequence" size)
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
		 (log-stream "file pos is now: ~:d" (stream-seek stream 0 :current))
		 (log-stream "~a" (id3-frame:printable-array octets))
		 octets)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; STRINGS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;
;; decode octets as an iso-8859-1 string (encoding == 0)
(defun stream-decode-iso-string (octets &key (start 0) (end nil))
  (ccl:decode-string-from-octets octets :start start :end end :external-format :iso-8859-1))

;;
;; decode octets as a ucs string (encoding == 1)
;; XXX: Coded this way because I can't seem to get a simple :external-format :ucs-2 to work correctly
;; AND some taggers encode a UCS-2 empty string w/o a byte-order mark (i.e. null strings are
;; sometimes encoded as #(00 00))
(defun stream-decode-ucs-string (octets &key (start 0) (end nil))
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

;;
;; decode octets as a ucs-be string (encoding == 2)
(defun stream-decode-ucs-be-string (octets &key (start 0) (end nil))
  (ccl:decode-string-from-octets octets :start start :end end :external-format :ucs-2be))

;;
;; decode octets as a utf-8 string
(defun stream-decode-utf-8-string (octets &key (start 0) (end nil))
  (ccl:decode-string-from-octets octets :start start :end end :external-format :utf-8))

;;
;; decode octets depending on encoding
(defun stream-decode-string (octets &key (start 0) (end nil) (encoding 0))
  (ecase encoding
	(0 (stream-decode-iso-string octets :start start :end end))
	(1 (stream-decode-ucs-string octets :start start :end end))
	(2 (stream-decode-ucs-be-string octets :start start :end end))
	(3 (stream-decode-utf-8-string octets :start start :end end))))

;;
;; read an iso-8859-1 string of length 'len' (encoding = 0)
(defmethod stream-read-iso-string-with-len ((instream base-stream) len)
  (let ((octets (stream-read-sequence instream len)))
	(stream-decode-iso-string octets)))

;;
;; read an ucs-2 string of length 'len' (encoding = 1)
(defmethod stream-read-ucs-string-with-len ((instream base-stream) len)
  (let ((octets (stream-read-sequence instream len)))
	  (stream-decode-ucs-string octets)))

;;
;; read an ucs-2-be string of length 'len' (encoding = 2)
(defmethod stream-read-ucs-be-string-with-len ((instream base-stream) len)
  (let ((octets (stream-read-sequence instream len)))
	(stream-decode-ucs-be-string octets)))

;;
;; read an utf-8 string of length 'len' (encoding = 3)
(defmethod stream-read-utf-8-string-with-len ((instream base-stream) len)
  (let ((octets (stream-read-sequence instream len)))
	(stream-decode-utf-8-string octets)))

;;
;; Read in a string of a given encoding of length 'len'
(defmethod stream-read-string-with-len ((instream base-stream) len &key (encoding 0))
  ;(format t "s-wth-len: ~a, ~d, ~d~%" instream len encoding)
  (ecase encoding
	(0 (stream-read-iso-string-with-len instream len))
	(1 (stream-read-ucs-string-with-len instream len))
	(2 (stream-read-ucs-be-string-with-len instream len))
	(3 (stream-read-utf-8-string-with-len instream len))))

;;
;; Read in a null terminated iso-8859-1 string
(defmethod stream-read-iso-string ((instream base-stream))
  (let ((octets (ccl:with-output-to-vector (out)
				  (do ((b (stream-read-u8 instream) (stream-read-u8 instream)))
					  (nil)
					(when (zerop b)
					  (return))			; leave loop w/o writing
					(write-byte b out)))))
	(stream-decode-iso-string octets)))

;;
;; Read in a null terminated ucs string 
(defmethod stream-read-ucs-string ((instream base-stream))
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

;;
;; Read in a null terminated ucs-be string
(defmethod stream-read-ucs-be-string ((instream base-stream))
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

;;
;; Read in a null terminated utf-8 string (encoding == 3)
(defmethod stream-read-utf-8-string ((instream base-stream))
  (let ((octets (ccl:with-output-to-vector (out)
				  (do ((b (stream-read-u8 instream)
						  (stream-read-u8 instream)))
					  (nil)
					(when (zerop b)
					  (return))
					(write-byte b out)))))
	(stream-decode-utf-8-string octets)))

;;
;; Read in a null terminated string of a given encoding
(defmethod stream-read-string ((instream base-stream) &key (encoding 0))
  (ecase encoding
	(0 (stream-read-iso-string instream))
	(1 (stream-read-ucs-string instream))
	(2 (stream-read-ucs-be-string instream))
	(3 (stream-read-utf-8-string instream))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; FILES ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun parse-mp4-file (filename)
  (let (stream)
	(handler-case
		(progn
		  (setf stream (make-file-stream 'mp4-file-stream filename))
		  (mp4-atom:find-mp4-atoms stream))
	  (mp4-atom:mp4-atom-condition (c)
		(warn-user "make-mp4-stream got condition: ~a" c)
		(when stream (stream-close stream))
		(setf stream nil)))
	stream))

(defvar *get-mpeg-info* nil)

(defun parse-mp3-file (filename &key (get-mpeg-info *get-mpeg-info*))
  (let (stream)
	  (handler-case
		  (progn
			(setf stream (make-file-stream 'mp3-file-stream filename))
			(id3-frame:find-id3-frames stream)
			(when get-mpeg-info
			  (setf (mpeg-info stream) (mpeg:get-mpeg-info stream))))
		(id3-frame:id3-frame-condition (c)
		  (warn-user "make-mp3-stream got condition: ~a" c)
		  (when stream (stream-close stream))
		  (setf stream nil)))
	stream))
