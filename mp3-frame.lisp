;;; -*- Mode: Lisp;  show-trailing-whitespace: t; Base: 10; indent-tabs: nil; Syntax: ANSI-Common-Lisp; Package: MP-FRAME; -*-
;;; Copyright (c) 2013, Mark VandenBrink. All rights reserved.

(in-package #:mp3-frame)

(log5:defcategory cat-log-mp3-frame)
(defmacro log-mp3-frame (&rest log-stuff) `(log5:log-for (cat-log-mp3-frame) ,@log-stuff))

(define-condition mp3-frame-condition ()
  ((location :initarg :location :reader location :initform nil)
   (object   :initarg :object   :reader object   :initform nil)
   (messsage :initarg :message  :reader message  :initform "Undefined Condition"))
  (:report (lambda (condition stream)
			 (format stream "mp3-frame condition at location: <~a> with object: <~a>: message: <~a>"
					 (location condition) (object condition) (message condition)))))

(defmethod print-object ((me mp3-frame-condition) stream)
  (format stream "location: <~a>, object: <~a>, message: <~a>" (location me) (object me) (message me)))

(defparameter *pprint-mp3-frame* nil
  "Controls whether we pretty print frame data")

(defclass mp3-id3-header ()
  ((version        :accessor version        :initarg :version        :initform 0)
   (revision       :accessor revision       :initarg :revision       :initform 0)
   (flags          :accessor flags          :initarg :flags          :initform 0)
   (size           :accessor size           :initarg :size           :initform 0)
   (ext-header     :accessor ext-header     :initarg :ext-header     :initform nil)
   (frames	       :accessor frames	        :initarg :frames		 :initform nil)
   (v21-tag-header :accessor v21-tag-header :initarg :v21-tag-header :initform nil))
  (:documentation "The ID3 header, found at start of file"))

(defmethod vpprint ((me mp3-id3-header) stream &key (indent 0))
  "Set *pprint-mp3-frame* to get pretty printing and call print-object via format"
  (let ((*pprint-mp3-frame* t))
	(format stream "~vt~a" (* indent 1) me)))

(defun is-valid-mp3-file (mp3-file)
  "Make sure this is an MP3 file. Look for frames at begining and/or end"
  (log5:with-context "is-valid-mp3-file"
	(seek mp3-file 0 :start)
	(let* ((id3 (read-string mp3-file :size 3))
		   (version (read-u8 mp3-file))
		   (tag))
	  (seek mp3-file 128 :end)
	  (setf tag (read-string mp3-file :size 3))
	  (seek mp3-file 0 :start)

	  (log-mp3-frame "id3 = ~a, version = ~d" id3 version)

	  (or (and (string= "ID3" id3)
			   (or (= 2 version) (= 3 version) (= 4 version)))
		  (string= tag "TAG")))))

(defclass v21-tag-header ()
  ((songname :accessor songname :initarg :songname :initform nil)
   (artist   :accessor artist   :initarg :artist   :initform nil)
   (album    :accessor album    :initarg :album    :initform nil)
   (year     :accessor year     :initarg :year     :initform nil)
   (comment  :accessor comment  :initarg :comment  :initform nil)
   (genre    :accessor genre    :initarg :genre    :initform nil))
  (:documentation "ID3 V2.1 old-style tag.  If present, found in last 128 bytes of file."))

(defmethod vpprint ((me v21-tag-header) stream &key (indent 0))
  "Set *pprint-mp3-frame* to get pretty printing and call print-object via format"
  (let ((*pprint-mp3-frame* t))
	(format stream "~vt~a" (* indent 1) me)))

(defmethod print-object ((me v21-tag-header) stream)
  (if (null *pprint-mp3-frame*)
	  (call-next-method)
	  (with-slots (songname artist album year comment genre) me
		(format stream "songname = <~a>, artist = <~a>, album = <~a>, year = <~a>, comment = <~a>, genre = ~d"
				songname artist album year comment genre))))

(defmethod initialize-instance ((me v21-tag-header) &key instream)
  "Read in a V2.1 tag.  Caller will have seek'ed file to correct location and ensured that TAG was present"
  (log5:with-context "v21-frame-initializer"
	(log-mp3-frame "reading v2.1 tag")
	(with-slots (songname artist album year comment genre) me
	  (setf songname (read-string instream :size 30 :terminators '(0)))
	  (setf artist   (read-string instream :size 30 :terminators '(0)))
	  (setf album    (read-string instream :size 30 :terminators '(0)))
	  (setf year     (read-string instream :size 4  :terminators '(0)))
	  (setf comment  (read-string instream :size 30 :terminators '(0)))
	  (setf genre    (read-u8 instream))
	  (log-mp3-frame "v21 tag: ~a" (vpprint me nil)))))

(defclass mp3-ext-header ()
  ((size    :accessor size    :initarg :size    :initform 0)
   (flags   :accessor flags   :initarg :flags   :initform 0)
   (padding :accessor padding :initarg :padding :initform 0)
   (crc	    :accessor crc     :initarg :crc     :initform nil))
  (:documentation "class representing a V2.3/4 extended header"))

(defmethod vpprint ((me mp3-ext-header) stream &key (indent 0))
  "Set *pprint-mp3-frame* to get pretty printing and call print-object via format"
  (let ((*pprint-mp3-frame* t))
	(format stream "~vt~a" (* indent 1) me)))

(defmacro ext-header-crc-p (flags)	 `(logbitp 15 ,flags))

(defmethod initialize-instance ((me mp3-ext-header) &key instream)
  "Read in the extended header.  Caller will have seek'ed to correct location in file."
  (with-slots (size flags padding crc) me
	(setf size (read-u32 instream))
	(setf flags (read-u16 instream))
	(setf padding (read-u32 instream))
	(when (ext-header-crc-p flags)
	  (setf crc (read-u32 instream)))))

(defmethod print-object ((me mp3-ext-header) stream)
  (if (null *pprint-mp3-frame*)
	  (call-next-method)
	  (with-slots (size flags padding crc) me
		(format stream "extended header: size: ~d, flags: ~x, padding ~:d, crc = ~x~%"
				size flags padding crc))))

(defmacro header-unsynchronized-p (flags) `(logbitp 7 ,flags))
(defmacro header-extended-p (flags)       `(logbitp 6 ,flags))
(defmacro header-experimental-p (flags)   `(logbitp 5 ,flags)) 
(defmacro header-footer-p (flags)		  `(logbitp 4 ,flags)) ;; N.B. *NOT* defined for 2.3 tags

(defmacro print-header-flags (stream flags)
  `(format ,stream "0x~2,'0x: ~:[0/~;unsynchronized-frames/~]~:[0/~;extended-header/~]~:[0/~;expermental-tag/~]~:[0~;footer-present~]"
		   ,flags
		   (header-unsynchronized-p ,flags)
		   (header-extended-p ,flags)
		   (header-experimental-p ,flags)
		   (header-footer-p ,flags)))

(defmethod print-object ((me mp3-id3-header) stream)
  (if (null *pprint-mp3-frame*)
	  (call-next-method)
	  (with-slots (version revision flags v21-tag-header size ext-header frames) me
		(format stream "Header: version/revision: ~d/~d, flags: ~a, size = ~:d bytes; ~a; ~a"
				version revision (print-header-flags nil flags) size
				(if (header-extended-p flags)
					(concatenate 'string "Extended header: " (vpprint ext-header nil))
					"No extended header")
				(if v21-tag-header
					(concatenate 'string "V21 tag: " (vpprint v21-tag-header nil))
					"No v21 tag"))
		(when frames
		  (format stream "~4tFrames[~d]:~%~{~8t~a~^~%~}" (length frames) frames)))))

(defmethod initialize-instance :after ((me mp3-id3-header) &key instream &allow-other-keys)
  "Fill in an mp3-header from file."
  (log5:with-context "mp3-id3-header-initializer"
	(with-slots (version revision flags size ext-header frames v21-tag-header) me
	  (seek instream 128 :end)
	  (when (string= "TAG" (read-string instream :size 3))
		(log-mp3-frame "looking at last 128 bytes at ~:d to try to read id3v21 header" (seek instream 0 :current))
		(handler-case
			(setf v21-tag-header (make-instance 'v21-tag-header :instream instream))
		  (condition (c)
			(log-mp3-frame "reading v21 got condition: ~a" c))))

	  (seek instream 0 :start)
	  (when (string= "ID3" (read-string instream :size 3))
		(setf version (read-u8 instream))
		(setf revision (read-u8 instream))
		(setf flags (read-u8 instream))
		(setf size (mp3-file:read-sync-safe-u32 instream))
		(when (header-unsynchronized-p flags) (log-mp3-frame "unsync"))
		(assert (not (header-footer-p flags)) () "Can't decode ID3 footer's yet")
		(when (header-extended-p flags)
		  (setf ext-header (make-instance 'mp3-extended-header :instream instream))))

	  (log-mp3-frame "~a" (vpprint me nil)))))

(defclass id3-frame ()
  ((pos     :accessor pos     :initarg :pos)
   (version :accessor version :initarg :version)
   (id      :accessor id      :initarg :id)
   (len     :accessor len     :initarg :len)
   (flags   :accessor flags   :initarg :flags :initform nil))
  (:documentation   "Base class for an ID3 frame"))

(defmacro frame-23-altertag-p  (frame-flags) `(logbitp 15 ,frame-flags))
(defmacro frame-23-alterfile-p (frame-flags) `(logbitp 14 ,frame-flags))
(defmacro frame-23-readonly-p  (frame-flags) `(logbitp 13 ,frame-flags))
(defmacro frame-23-compress-p  (frame-flags) `(logbitp 7 ,frame-flags))
(defmacro frame-23-encrypt-p   (frame-flags) `(logbitp 6 ,frame-flags))
(defmacro frame-23-group-p     (frame-flags) `(logbitp 5 ,frame-flags))

(defmacro frame-24-altertag-p  (frame-flags) `(logbitp 14 ,frame-flags))
(defmacro frame-24-alterfile-p (frame-flags) `(logbitp 13 ,frame-flags))
(defmacro frame-24-readonly-p  (frame-flags) `(logbitp 12 ,frame-flags))
(defmacro frame-24-groupid-p   (frame-flags) `(logbitp 6 ,frame-flags))
(defmacro frame-24-compress-p  (frame-flags) `(logbitp 3 ,frame-flags))
(defmacro frame-24-encrypt-p   (frame-flags) `(logbitp 2 ,frame-flags))
(defmacro frame-24-unsynch-p   (frame-flags) `(logbitp 1 ,frame-flags))
(defmacro frame-24-datalen-p   (frame-flags) `(logbitp 0 ,frame-flags))

(defun valid-frame-flags (header-version frame-flags)
  (ecase header-version
	(3 (zerop (logand #b0001111100011111 frame-flags)))
	(4 (zerop (logand #b1000111110110000 frame-flags)))))

(defmethod print-object ((me id3-frame) stream)
  (if (null *pprint-mp3-frame*)
	  (call-next-method)
	  (with-slots (pos version valid-p id len flags) me
		(format stream "@offset: ~:d, version = ~d, id: ~s, len: ~:d "
				pos version id len)
		(if flags
			(ecase version
			  (3 (format stream "flags: 0x~4,'0x: ~:[0/~;tag-alter-preservation/~]~:[0/~;file-alter-preservation/~]~:[0/~;read-only/~]~:[0/~;compress/~]~:[0/~;encypt/~]~:[0~;group~], "
						 flags
						 (frame-23-altertag-p flags)
						 (frame-23-alterfile-p flags)
						 (frame-23-readonly-p flags)
						 (frame-23-compress-p flags)
						 (frame-23-encrypt-p flags)
						 (frame-23-group-p flags)))
			  (4 (format stream "flags: 0x~4,'0x: ~:[0/~;tag-alter-preservation/~]~:[0/~;file-alter-preservation/~]~:[0/~;read-only/~]~:[0/~;group-id/~]~:[0/~;compress/~]~:[0/~;encypt/~]~:[0/~;unsynch/~]~:[0~;datalen~], "
						 flags
						 (frame-24-altertag-p flags)
						 (frame-24-alterfile-p flags)
						 (frame-24-readonly-p flags)
						 (frame-24-groupid-p flags)
						 (frame-24-compress-p flags)
						 (frame-24-encrypt-p flags)
						 (frame-24-unsynch-p flags)
						 (frame-24-datalen-p flags))))))))

(defclass raw-frame (id3-frame)
  ((octets :accessor octets :initform nil))
  (:documentation "Frame class that slurps in frame contents"))

(defmethod initialize-instance :after ((me raw-frame) &key instream)
  (log5:with-context "raw-frame"
	(with-slots (len octets) me
	  (log-mp3-frame "reading ~:d bytes from position ~:d" len (seek instream 0 :current))
	  (setf octets (read-octets instream len)))))

(defmethod print-object :after ((me raw-frame) stream)
  (if (null *pprint-mp3-frame*)
	  (call-next-method)
	  (with-slots (octets) me
		(let* ((len (length (slot-value me 'octets)))
			   (print-len (min len 10))
			   (printable-array (make-array print-len :displaced-to (slot-value me 'octets))))
		  (format stream "[~:d of ~:d bytes] <~x>" print-len len printable-array)))))

(defun find-id3-frames (header instream)
  "Loop thru all the frames in INSTREAM based on information from HEADER"
  (labels ((read-and-de-sync (instream len)
			 "Used to undo sync-safe when the header says false syncs have been removed from the tags"
			 (let* ((last-byte-was-FF nil)
					(byte nil)
					(synced-frame-data (with-binary-output-to-vector (out)
										 (dotimes (i len)
										   (setf byte (read-byte instream))
										   (if last-byte-was-FF
											   (if (not (zerop byte))
												   (write-byte byte out))
											   (write-byte byte out))
										   (setf last-byte-was-FF (= byte #xFF))))))
			   synced-frame-data)))

	(log5:with-context "find-id3-frames"
	  nil)))

;; 	  (let ((mem-stream)
;; 			(first-byte))
;; 		(if (header-unsynchronized-p header)
;; 			(setf mem-stream (read-and-desync instream (size header)))
;; 	   (log-mp3-frame "Looking for frames: header = ~a, starting position = ~:d" (mp3-frame:vpprint header nil) (seek instream 0 :current))
;; 	 (loop
;; 	   (let ((first-byte (read-u8 instream)))
;; 		 (when (
;; ;	 (if (header-unsynchronized-p (flags header))
;; 	 (do* ((pos (seek instream 0 :current))
;; 		   (frame)
;; 		   (end (+ pos (size header))))
;; 		  ((>= pos end))

;; 	nil))

(defun find-mp3-frames (mp3-file)
  "With an open mp3-file, make sure it is in fact an MP3 file, then read it's header and frames, returning both"
  (log5:with-context "find-mp3-frames"
	(when (not (is-valid-mp3-file mp3-file))
	  (log-mp3-frame "~a is not an mp3 file" (filename mp3-file))
	  (error 'mp3-frame-condition :location "find-mp3-frames" :object (filename mp3-file) :message "is not an mp3 file"))

 	(log-mp3-frame "~a is a valid mp3 file" (filename mp3-file))

	(let* ((header (make-instance 'mp3-id3-header :instream mp3-file))
		   (frames (find-id3-frames header mp3-file)))
	  (log-mp3-frame "Header: ~a, frames = ~a" header frames)
	  (setf (slot-value header 'frames) frames))))

