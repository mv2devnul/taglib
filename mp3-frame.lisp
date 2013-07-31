;;; -*- Mode: Lisp;  show-trailing-whitespace: t; Base: 10; indent-tabs: nil; Syntax: ANSI-Common-Lisp; Package: MP3-FRAME; -*-
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

(defun is-valid-mp3-file (mp3-file)
  "Make sure this is an MP3 file. Look for frames at begining and/or end"
  (log5:with-context "is-valid-mp3-file"
	(stream-seek mp3-file 0 :start)
	(let* ((id3 (stream-read-string-with-len mp3-file 3))
		   (version (stream-read-u8 mp3-file))
		   (tag))
	  (stream-seek mp3-file 128 :end)
	  (setf tag (stream-read-string-with-len mp3-file 3))
	  (stream-seek mp3-file 0 :start)

	  (log-mp3-frame "id3 = ~a, version = ~d" id3 version)

	  (or (and (string= "ID3" id3)
			   (or (= 2 version) (= 3 version) (= 4 version)))
		  (string= tag "TAG")))))

(defclass v21-tag-header ()
  ((title :accessor title :initarg :title :initform nil)
   (artist   :accessor artist   :initarg :artist   :initform nil)
   (album    :accessor album    :initarg :album    :initform nil)
   (year     :accessor year     :initarg :year     :initform nil)
   (comment  :accessor comment  :initarg :comment  :initform nil)
   (genre    :accessor genre    :initarg :genre    :initform nil))
  (:documentation "ID3 V2.1 old-style tag.  If present, found in last 128 bytes of file."))

(defmethod vpprint ((me v21-tag-header) stream)
  (with-slots (title artist album year comment genre) me
	(format stream "title = <~a>, artist = <~a>, album = <~a>, year = <~a>, comment = <~a>, genre = ~d (~a)"
			title artist album year comment genre (mp3-tag::get-id3v1-genre genre))))

(defmethod initialize-instance ((me v21-tag-header) &key instream)
  "Read in a V2.1 tag.  Caller will have stream-seek'ed file to correct location and ensured that TAG was present"
  (log5:with-context "v21-frame-initializer"
	(log-mp3-frame "reading v2.1 tag")
	(with-slots (title artist album year comment genre) me
	  (setf title (trim-string (stream-read-string-with-len instream 30)))
	  (setf artist   (trim-string (stream-read-string-with-len instream 30)))
	  (setf album    (trim-string (stream-read-string-with-len instream 30)))
	  (setf year     (trim-string (stream-read-string-with-len instream 4)))
	  (setf comment  (trim-string (stream-read-string-with-len instream 30)))
	  (setf genre    (stream-read-u8 instream))
	  (log-mp3-frame "v21 tag: ~a" (vpprint me nil)))))

(defclass mp3-ext-header ()
  ((size    :accessor size    :initarg :size    :initform 0)
   (flags   :accessor flags   :initarg :flags   :initform 0)
   (padding :accessor padding :initarg :padding :initform 0)
   (crc	    :accessor crc     :initarg :crc     :initform nil))
  (:documentation "class representing a V2.3/4 extended header"))

(defmacro ext-header-crc-p (flags)	 `(logbitp 15 ,flags))

(defmethod initialize-instance ((me mp3-ext-header) &key instream)
  "Read in the extended header.  Caller will have stream-seek'ed to correct location in file.
Note: extended headers are subject to unsynchronization, so make sure that INSTREAM has been made sync-safe."
  (with-slots (size flags padding crc) me
	(setf size (stream-read-u32 instream)) ; this is sync-safe in 2.4?
	(setf flags (stream-read-u16 instream))
	(setf padding (stream-read-u32 instream)) ; this is sync-safe and 35 bits in 2.4?
	(when (ext-header-crc-p flags)
	  (setf crc (stream-read-u32 instream)))))

(defmethod vpprint ((me mp3-ext-header) stream)
  (with-slots (size flags padding crc) me
	(format stream "extended header: size: ~d, flags: ~x, padding ~:d, crc = ~x~%"
			size flags padding crc)))

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

(defmethod vpprint ((me mp3-id3-header) stream)
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
		  (format stream "~&~4tFrames[~d]:~%" (length frames))
		  (dolist (f frames)
			(format stream "~8t~a~%" (vpprint f nil))))))

(defmethod initialize-instance :after ((me mp3-id3-header) &key instream &allow-other-keys)
  "Fill in an mp3-header from INSTREAM."
  (log5:with-context "mp3-id3-header-initializer"
	(with-slots (version revision flags size ext-header frames v21-tag-header) me
	  (stream-seek instream 128 :end)
	  (when (string= "TAG" (stream-read-string-with-len instream 3))
		(log-mp3-frame "looking at last 128 bytes at ~:d to try to read id3v21 header" (stream-seek instream 0 :current))
		(handler-case
			(setf v21-tag-header (make-instance 'v21-tag-header :instream instream))
		  (mp3-frame-condition (c)
			(log-mp3-frame "reading v21 got condition: ~a" c))))

	  (stream-seek instream 0 :start)
	  (when (string= "ID3" (stream-read-string-with-len instream 3))
		(setf version (stream-read-u8 instream))
		(setf revision (stream-read-u8 instream))
		(setf flags (stream-read-u8 instream))
		(setf size (stream-read-u32 instream :bits-per-byte 7))
		(when (header-unsynchronized-p flags)
		  (log-mp3-frame "unsync"))
		(assert (not (header-footer-p flags)) () "Can't decode ID3 footer's yet"))

	  (log-mp3-frame "~a" (vpprint me nil)))))

(defclass id3-frame ()
  ((pos     :accessor pos     :initarg :pos)
   (id      :accessor id      :initarg :id)
   (len     :accessor len     :initarg :len)
   (version :accessor version :initarg :version)
   (flags   :accessor flags   :initarg :flags :initform nil)) ; unused in v2.2
  (:documentation "Base class for an ID3 frame"))

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

(defun vpprint-frame-header (id3-frame)
  (with-output-to-string (stream)
	(with-slots (pos version id len flags) id3-frame
	  (format stream "offset: ~:d, version = ~d, id: ~a, len: ~:d " pos version id len)
	  (if flags
		  (ecase version
			(3 (format stream "flags: 0x~4,'0x: ~:[0/~;tag-alter-preservation/~]~:[0/~;file-alter-preservation/~]~:[0/~;read-only/~]~:[0/~;compress/~]~:[0/~;encypt/~]~:[0~;group~]"
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

(defclass frame-raw (id3-frame)
  ((octets :accessor octets :initform nil))
  (:documentation "Frame class that slurps in frame contents"))

(defmethod initialize-instance :after ((me frame-raw) &key instream)
  (log5:with-context "frame-raw"
	(with-slots (pos len octets) me
	  (log-mp3-frame "reading ~:d bytes from position ~:d" len pos)
	  (setf octets (stream-read-sequence instream len))
	  (log-mp3-frame "frame: ~a" (vpprint me nil)))))

(defparameter *max-raw-bytes-print-len* 10)
(defun printable-array (array)
  (let* ((len (length array))
		 (print-len (min len *max-raw-bytes-print-len*))
		 (printable-array (make-array print-len :displaced-to array)))
	(format nil "[~:d of ~:d bytes] <~x>" print-len len printable-array)))

(defun upto-null (string)
  (subseq string 0 (position #\Null string)))

(defmethod vpprint ((me frame-raw) stream)
  (with-slots (octets) me
	(format stream "frame-raw: ~a, ~a" (vpprint-frame-header me) (printable-array octets))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; V22 frames
;;

;;; frame I haven't parsed (or don't need to parse)
(defclass frame-buf (frame-raw) ())
(defclass frame-cnt (frame-raw) ())
(defclass frame-cra (frame-raw) ())
(defclass frame-crm (frame-raw) ())
(defclass frame-equ (frame-raw) ())
(defclass frame-etc (frame-raw) ())
(defclass frame-geo (frame-raw) ())
(defclass frame-ipl (frame-raw) ())
(defclass frame-lnk (frame-raw) ())
(defclass frame-mci (frame-raw) ())
(defclass frame-mll (frame-raw) ())
(defclass frame-pop (frame-raw) ())
(defclass frame-rev (frame-raw) ())
(defclass frame-rva (frame-raw) ())
(defclass frame-slt (frame-raw) ())
(defclass frame-waf (frame-raw) ())
(defclass frame-war (frame-raw) ())
(defclass frame-was (frame-raw) ())
(defclass frame-wcm (frame-raw) ())
(defclass frame-wcp (frame-raw) ())
(defclass frame-wpb (frame-raw) ())
(defclass frame-wxx (frame-raw) ())
(defclass frame-stc (frame-raw) ())

;; COM frames
;; Comment                   "COM"
;; Frame size                $xx xx xx
;; Text encoding             $xx
;; Language                  $xx xx xx
;; Short content description <textstring> $00 (00)
;; The actual text           <textstring>
(defclass frame-com (id3-frame)
  ((encoding :accessor encoding)
   (lang     :accessor lang)
   (desc	 :accessor desc)
   (val	     :accessor val)))

(defmethod initialize-instance :after ((me frame-com) &key instream)
  (log5:with-context "frame-com"
	(with-slots (len encoding lang desc val) me
	  (setf encoding (stream-read-u8 instream))
	  (setf lang (stream-read-iso-string-with-len instream 3))
	  (multiple-value-bind (n v) (get-name-value-pair instream (- len 1 3) encoding encoding)
		(setf desc n)
		(setf val v))
	  (log-mp3-frame "encoding = ~d, lang = <~a>, desc = <~a>, text = <~a>" encoding lang desc val))))

(defmethod vpprint ((me frame-com) stream)
  (with-slots (len encoding lang desc val) me
	(format stream "frame-com: ~a,  encoding = ~d, lang = <~a>, desc = <~a>, val = <~a>" (vpprint-frame-header me) encoding lang desc val)))

;; v22 PIC
;; Attached picture   "PIC"
;; Frame size         $xx xx xx
;; Text encoding      $xx
;; Image format       $xx xx xx
;; Picture type       $xx
;; Description        <textstring> $00 (00)
;; Picture data       <binary data>
(defclass frame-pic (id3-frame)
  ((encoding   :accessor encoding)
   (img-format :accessor img-format)
   (type       :accessor type)
   (desc       :accessor desc)
   (data       :accessor data)))

(defmethod initialize-instance :after ((me frame-pic) &key instream)
  (log5:with-context "frame-pic"
	(with-slots (id len encoding img-format type desc data) me
	  (setf encoding (stream-read-u8 instream))
	  (setf img-format (stream-read-iso-string-with-len instream 3))
	  (setf type (stream-read-u8 instream))
	  (multiple-value-bind (n v) (get-name-value-pair instream (- len 5) encoding -1)
		(setf desc n)
		(setf data v)
		(log-mp3-frame "encoding: ~d, img-format = <~a>, type = ~d, desc = <~a>, value = ~a"
				   encoding img-format type desc (printable-array data))))))

(defmethod vpprint ((me frame-pic) stream)
  (with-slots (encoding img-format type desc data) me
	(format stream "frame-pic: ~a,  encoding ~d, img-format type: <~a>, picture type: ~d, description <~a>, data: ~a"
			(vpprint-frame-header me) encoding img-format type desc (printable-array data))))

;; Generic text-info frames
;; Text information identifier  "T00" - "TZZ" , excluding "TXX", or "T000 - TZZZ", excluding "TXXX"
;; Text encoding                $xx
;; Information                  <textstring>
(defclass frame-text-info (id3-frame)
  ((encoding :accessor encoding)
   (info     :accessor info)))

(defmethod initialize-instance :after ((me frame-text-info) &key instream)
  (log5:with-context "frame-text-info"
	(with-slots (len encoding info) me
	  (setf encoding (stream-read-u8 instream))
	  (setf info (stream-read-string-with-len instream (1- len) :encoding encoding))

	  ;; a null is ok, but according to the "spec", you're supposed to ignore anything after a 'Null'
	  (setf info (upto-null info))

	  (log-mp3-frame "encoding = ~d, info = <~a>" encoding info))))


(defmethod vpprint ((me frame-text-info) stream)
  (with-slots (len encoding info) me
	(format stream "frame-text-info: ~a, encoding = ~d, info = <~a>" (vpprint-frame-header me) encoding info)))

;; v22 User defined...   "TXX" frames
;; Frame size        $xx xx xx
;; Text encoding     $xx
;; Description       <textstring> $00 (00)
;; Value             <textstring>
(defclass frame-txx (id3-frame)
  ((encoding :accessor encoding)
   (desc     :accessor desc)
   (val      :accessor val)))

(defmethod initialize-instance :after ((me frame-txx) &key instream)
  (log5:with-context "frame-txx"
	(with-slots (len encoding desc val) me
	  (setf encoding (stream-read-u8 instream))
	  (multiple-value-bind (n v) (get-name-value-pair instream (1- len) encoding encoding)
		(setf desc n)
		(setf val v)
		(log-mp3-frame "encoding = ~d, desc = <~a>, val = <~a>" encoding desc val)))))

(defmethod vpprint ((me frame-txx) stream)
  (with-slots (len encoding desc val) me
	(format stream "frame-txx: ~a, encoding = ~d, desc = <~a>, val = <~a>" (vpprint-frame-header me) encoding desc val)))

(defclass frame-ufi (id3-frame)
  ((name  :accessor name)
   (value :accessor value)))

(defmethod initialize-instance :after ((me frame-ufi) &key instream)
  (log5:with-context "frame-ufi"
	(with-slots (id len name value) me
	  (multiple-value-bind (n v) (get-name-value-pair instream len 0 -1)
		(setf name n)
		(setf value v))
	  (log-mp3-frame "name = <~a>, value = ~a" name (printable-array value)))))

(defmethod vpprint ((me frame-ufi) stream)
  (with-slots (id len name value) me
	(format stream "frame-ufi: ~a, name: <~a>, value: ~a" (vpprint-frame-header me) name (printable-array value))))

(defclass frame-tal (frame-text-info) ())
(defclass frame-tbp (frame-text-info) ())
(defclass frame-tcm (frame-text-info) ())
(defclass frame-tco (frame-text-info) ())
(defclass frame-tcp (frame-text-info) ())
(defclass frame-tcr (frame-text-info) ())
(defclass frame-tda (frame-text-info) ())
(defclass frame-tdy (frame-text-info) ())
(defclass frame-ten (frame-text-info) ())
(defclass frame-tft (frame-text-info) ())
(defclass frame-tim (frame-text-info) ())
(defclass frame-tke (frame-text-info) ())
(defclass frame-tla (frame-text-info) ())
(defclass frame-tle (frame-text-info) ())
(defclass frame-tmt (frame-text-info) ())
(defclass frame-toa (frame-text-info) ())
(defclass frame-tof (frame-text-info) ())
(defclass frame-tol (frame-text-info) ())
(defclass frame-tor (frame-text-info) ())
(defclass frame-tot (frame-text-info) ())
(defclass frame-tp1 (frame-text-info) ())
(defclass frame-tp2 (frame-text-info) ())
(defclass frame-tp3 (frame-text-info) ())
(defclass frame-tp4 (frame-text-info) ())
(defclass frame-tpa (frame-text-info) ())
(defclass frame-tpb (frame-text-info) ())
(defclass frame-trc (frame-text-info) ())
(defclass frame-trd (frame-text-info) ())
(defclass frame-trk (frame-text-info) ())
(defclass frame-tsi (frame-text-info) ())
(defclass frame-tss (frame-text-info) ())
(defclass frame-tt1 (frame-text-info) ())
(defclass frame-tt2 (frame-text-info) ())
(defclass frame-tt3 (frame-text-info) ())
(defclass frame-txt (frame-text-info) ())
(defclass frame-tye (frame-text-info) ())

(defclass frame-ult (frame-com) ())


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; V2.3/4 frames

;;
;; <Header for 'Audio encryption', ID: "AENC"> 
;; Owner identifier        <text string> $00
;; Preview start           $xx xx
;; Preview length          $xx xx
;; Encryption info         <binary data>
(defclass frame-aenc (frame-raw) ())

(defclass frame-aspi (frame-raw) ())
(defclass frame-comr (frame-raw) ())
(defclass frame-encr (frame-raw) ())
(defclass frame-equ2 (frame-raw) ())
(defclass frame-equa (frame-raw) ())
(defclass frame-etco (frame-raw) ())
(defclass frame-geob (frame-raw) ())
(defclass frame-grid (frame-raw) ())
(defclass frame-ipls (frame-raw) ())
(defclass frame-link (frame-raw) ())
(defclass frame-mcdi (frame-raw) ())
(defclass frame-mllt (frame-raw) ())
(defclass frame-ncon (frame-raw) ())
(defclass frame-owne (frame-raw) ())
(defclass frame-popm (frame-raw) ())
(defclass frame-poss (frame-raw) ())
(defclass frame-rbuf (frame-raw) ())
(defclass frame-rva2 (frame-raw) ())
(defclass frame-rvad (frame-raw) ())
(defclass frame-rvrb (frame-raw) ())
(defclass frame-seek (frame-raw) ())
(defclass frame-sign (frame-raw) ())
(defclass frame-sylt (frame-raw) ())
(defclass frame-sytc (frame-raw) ())
(defclass frame-user (frame-raw) ())


;; APIC
;; <Header for 'Attached picture', ID: "APIC">
;; Text encoding   $xx
;; MIME type       <text string> $00
;; Picture type    $xx
;; Description     <text string according to encoding> $00 (00)
;; Picture data    <binary data>
(defclass frame-apic (id3-frame)
  ((encoding :accessor encoding)
   (mime     :accessor mime)
   (type     :accessor type)
   (desc     :accessor desc)
   (data     :accessor data)))

(defmethod initialize-instance :after ((me frame-apic) &key instream)
  (log5:with-context "frame-apic"
	(with-slots (id len encoding mime type desc data) me
	  (setf encoding (stream-read-u8 instream))
	  (setf mime (stream-read-iso-string instream))
	  (setf type (stream-read-u8 instream))
	  (multiple-value-bind (n v) (get-name-value-pair instream (- len 1 (length mime) 1 1) encoding -1)
		(setf desc n)
		(setf data v)
		(log-mp3-frame "enoding = ~d, mime = <~a>, type = ~d, descx = <~a>, data = ~a" encoding mime type desc (printable-array data))))))

(defmethod vpprint ((me frame-apic) stream)
  (with-slots (encoding mime type desc data) me
	(format stream "frame-apic: ~a, encoding ~d, mime type: ~a, picture type: ~d, description <~a>, data: ~a"
			(vpprint-frame-header me) encoding mime type desc (printable-array data))))

;; COMM frames
;; <Header for 'Comment', ID: "COMM">
;; Text encoding           $xx
;; Language                $xx xx xx
;; Short content descrip.  <text string according to encoding> $00 (00)
;; The actual text         <full text string according to encoding>
(defclass frame-comm (id3-frame)
  ((encoding :accessor encoding)
   (lang	 :accessor lang)
   (desc     :accessor desc)
   (val		 :accessor val)))

(defmethod initialize-instance :after ((me frame-comm) &key instream)
  (log5:with-context "frame-comm"
	(with-slots (encoding lang len desc val) me
	  (setf encoding (stream-read-u8 instream))
	  (setf lang (stream-read-iso-string-with-len instream 3))
	  (multiple-value-bind (n v) (get-name-value-pair instream (- len 1 3) encoding encoding)
		(setf desc n)
		(if (eq #\Null (aref v (1- (length v)))) ; iTunes broken-ness... maybe this should be done on rendering the comment instead of here?
			(setf val (make-array (1- (length v)) :displaced-to v))
			(setf val v)))
	  (log-mp3-frame "encoding = ~d, lang = <~a>, desc = <~a>, val = <~a>" encoding lang desc val))))

(defmethod vpprint ((me frame-comm) stream)
  (with-slots (encoding lang desc val) me
	(format stream "frame-comm: ~a,  encoding: ~d, lang: ~x, desc: ~a, val ~a"
			(vpprint-frame-header me) encoding lang desc val)))

(defclass frame-uslt (frame-comm) ())

;; PCNT frames
;; <Header for 'Play counter', ID: "PCNT">
;; Counter         $xx xx xx xx (xx ...)
(defclass frame-pcnt (id3-frame)
  ((play-count :accessor play-count)))

(defmethod initialize-instance :after ((me frame-pcnt) &key instream)
  (log5:with-context "frame-pcnt"
	(with-slots (play-count len) me
	  (assert (= 4 len) () "Ran into a play count with ~d bytes" len)
	  (setf play-count (stream-read-u32 instream)) ; probably safe---play count *can* be longer than 4 bytes, but...
	  (log-mp3-frame "play count = <~d>" play-count))))

(defmethod vpprint ((me frame-pcnt) stream)
  (with-slots (play-count) me
	(format stream "frame-pcnt: ~a, count = ~d" (vpprint-frame-header me) play-count)))

;; PRIV frames
;; <Header for 'Private frame', ID: "PRIV">
;; Owner identifier        <text string> $00
;; The private data        <binary data>
(defclass frame-priv (id3-frame)
  ((name  :accessor name)
   (value :accessor value)))

(defmethod initialize-instance :after ((me frame-priv) &key instream)
  (log5:with-context "frame-priv"
	(with-slots (id len name value) me
	  (multiple-value-bind (n v) (get-name-value-pair instream len 0 -1)
		(setf name n)
		(setf value v)
		(log-mp3-frame "name = <~a>, value = <~a>" name value)))))

(defmethod vpprint ((me frame-priv) stream)
  (with-slots (id len name value) me
	(format stream "frame-priv: ~a, name: <~a>, data: ~a" (vpprint-frame-header me) name (printable-array value))))

;; TXXX frames
;; <Header for 'User defined text information frame', ID: "TXXX">
;; Text encoding    $xx
;; Description      <text string according to encoding> $00 (00)
;; Value    	    <text string according to encoding>
(defclass frame-txxx (id3-frame)
  ((encoding :accessor encoding)
   (desc     :accessor desc)
   (val      :accessor val)))

(defmethod initialize-instance :after ((me frame-txxx) &key instream)
  (log5:with-context "frame-txxx"
	(with-slots (encoding len desc val) me
	  (setf encoding (stream-read-u8 instream))
	  (multiple-value-bind (n v) (get-name-value-pair instream
													  (- len 1)
													  encoding
													  encoding)
		(setf desc n)
		(setf val v))
	  (log-mp3-frame "encoding = ~d, desc = <~a>, value = <~a>" encoding desc val))))

(defmethod vpprint ((me frame-txxx) stream)
  (format stream "frame-txxx: ~a, <~a/~a>" (vpprint-frame-header me) (desc me) (val me)))

;; UFID frames
;; <Header for 'Unique file identifier', ID: "UFID">
;; Owner identifier    <text string> $00
;; Identifier    	   <up to 64 bytes binary data>
(defclass frame-ufid (id3-frame)
  ((name  :accessor name)
   (value :accessor value)))

(defmethod initialize-instance :after ((me frame-ufid) &key instream)
  (log5:with-context "frame-ufid"
	(with-slots (id len name value) me
	  (multiple-value-bind (n v) (get-name-value-pair instream len 0 -1)
		(setf name n)
		(setf value v))
	  (log-mp3-frame "name = <~a>, value = ~a" name (printable-array value)))))

(defmethod vpprint ((me frame-ufid) stream)
  (with-slots (id len name value) me
	(format stream "frame-ufid: ~a,  name: <~a>, value: ~a" (vpprint-frame-header me) name (printable-array value))))

;; URL frame
;; <Header for 'URL link frame', ID: "W000" - "WZZZ", excluding "WXXX" described in 4.3.2.>
;; URL <text string>
(defclass frame-url-link (id3-frame)
  ((url :accessor url)))

(defmethod initialize-instance :after ((me frame-url-link) &key instream)
  (with-slots (id len url) me
	(log5:with-context "url"
	  (setf url (stream-read-iso-string-with-len instream len))
	  (log-mp3-frame "url = <~a>" url))))

(defmethod vpprint ((me frame-url-link) stream)
  (with-slots (url) me
	(format stream "frame-url-link: ~a, url: ~a" (vpprint-frame-header me) url)))

(defclass frame-talb (frame-text-info) ())
(defclass frame-tbpm (frame-text-info) ())
(defclass frame-tcmp (frame-text-info) ())
(defclass frame-tcom (frame-text-info) ())
(defclass frame-tcon (frame-text-info) ())
(defclass frame-tcop (frame-text-info) ())
(defclass frame-tdat (frame-text-info) ())
(defclass frame-tden (frame-text-info) ())
(defclass frame-tdly (frame-text-info) ())
(defclass frame-tdor (frame-text-info) ())
(defclass frame-tdrc (frame-text-info) ())
(defclass frame-tdrl (frame-text-info) ())
(defclass frame-tdtg (frame-text-info) ())
(defclass frame-tenc (frame-text-info) ())
(defclass frame-text (frame-text-info) ())
(defclass frame-tflt (frame-text-info) ())
(defclass frame-time (frame-text-info) ())
(defclass frame-tipl (frame-text-info) ())
(defclass frame-tit1 (frame-text-info) ())
(defclass frame-tit2 (frame-text-info) ())
(defclass frame-tit3 (frame-text-info) ())
(defclass frame-tkey (frame-text-info) ())
(defclass frame-tlan (frame-text-info) ())
(defclass frame-tlen (frame-text-info) ())
(defclass frame-tmcl (frame-text-info) ())
(defclass frame-tmed (frame-text-info) ())
(defclass frame-tmoo (frame-text-info) ())
(defclass frame-toal (frame-text-info) ())
(defclass frame-tofn (frame-text-info) ())
(defclass frame-toly (frame-text-info) ())
(defclass frame-tope (frame-text-info) ())
(defclass frame-tory (frame-text-info) ())
(defclass frame-town (frame-text-info) ())
(defclass frame-tpe1 (frame-text-info) ())
(defclass frame-tpe2 (frame-text-info) ())
(defclass frame-tpe3 (frame-text-info) ())
(defclass frame-tpe4 (frame-text-info) ())
(defclass frame-tpos (frame-text-info) ())
(defclass frame-tpro (frame-text-info) ())
(defclass frame-tpub (frame-text-info) ())
(defclass frame-trda (frame-text-info) ())
(defclass frame-trsn (frame-text-info) ())
(defclass frame-trso (frame-text-info) ())
(defclass frame-tsoa (frame-text-info) ())
(defclass frame-tsop (frame-text-info) ())
(defclass frame-tsot (frame-text-info) ())
(defclass frame-tsst (frame-text-info) ())
(defclass frame-tsse (frame-text-info) ())
(defclass frame-tsrc (frame-text-info) ())
(defclass frame-tsiz (frame-text-info) ())
(defclass frame-tyer (frame-text-info) ())
(defclass frame-trck (frame-text-info) ())

(defclass frame-wcom (frame-url-link) ())
(defclass frame-wcop (frame-url-link) ())
(defclass frame-woaf (frame-url-link) ())
(defclass frame-woar (frame-url-link) ())
(defclass frame-woas (frame-url-link) ())
(defclass frame-wors (frame-url-link) ())
(defclass frame-wpay (frame-url-link) ())
(defclass frame-wpub (frame-url-link) ())
(defclass frame-wxxx (frame-url-link) ())

;;
;; many id3 tags are name/value pairs, with the name/value encoded in various ways
;; this routine assumes that the name is always a string with a "normal" encoding (i.e. 0, 1, 2, or 3).
;; a value, however, accepts any negative number, which means read
;; the bytes an raw octets.
(defun get-name-value-pair (instream len name-encoding value-encoding)
  (log5:with-context  "get-name-value-pair"
	(log-mp3-frame "reading from ~:d, len ~:d, name-encoding = ~d, value-encoding = ~d" (stream-seek instream 0 :current) len name-encoding value-encoding)
	(let* ((old-pos (stream-seek instream 0 :current))
		   (name (stream-read-string instream :encoding name-encoding))
		   (name-len (- (stream-seek instream 0 :current) old-pos))
		   (value))

	  (log-mp3-frame "name = <~a>, name-len = ~d" name name-len)
	  (setf value (if (>= value-encoding 0)
					  (stream-read-string-with-len instream (- len name-len) :encoding value-encoding)
					  (stream-read-sequence instream (- len name-len)))) ; if < 0, then just read as octets

	  (values name value))))

;;
;; test to see if a string is a potentially valid frame id
(defun possibly-valid-frame-id? (frame-id)
  (labels ((numeric-char-p (c)
			 (let ((code (char-code c)))
			   (and (>= code (char-code #\0))
					(<= code (char-code #\9))))))

	(dotimes (i (length frame-id))
	  (let ((c (aref frame-id i)))
		(when (not (or (numeric-char-p c)
					   (and (alpha-char-p c) (upper-case-p c))))
		  (return-from possibly-valid-frame-id? nil))))
	t))

;;; Search by frame-id for a class, returning a class that can be used as arg to
;;; make-instance.
(defun find-frame-class (id)
  (log5:with-context "find-frame-class"
	(log-mp3-frame "looking for class <~a>" id)
	(let ((found-class-symbol (find-symbol (string-upcase (concatenate 'string "frame-" id)) :MP3-FRAME))
		  found-class)
	  (when found-class-symbol
		(setf found-class (find-class found-class-symbol))
		(log-mp3-frame "found class: ~a" found-class)
		(return-from find-frame-class found-class))

	  (log-mp3-frame "didn't find class, checking general cases")

	  ;; if not a "normal" frame-id, look at general cases of
	  ;; starting with a 'T' or a 'W'
	  (setf found-class (case (aref id 0)
						  (#\T (log-mp3-frame "assuming text-info") (find-class (find-symbol "FRAME-TEXT-INFO" :MP3-FRAME)))
						  (#\W (log-mp3-frame "assuming url-link")  (find-class (find-symbol "FRAME-URL-LINK"  :MP3-FRAME)))
						  (t
						   ;; we don't recognize the frame name.  if it could possibly be a real frame name,
						   ;; then just read it raw
						   (when (possibly-valid-frame-id? id)
							 (log-mp3-frame "just reading raw")
							 (find-class (find-symbol "FRAME-RAW" :MP3-FRAME))))))

	  (log-mp3-frame "general case for id <~a> is ~a" id found-class)
	  found-class)))

(defun make-frame (version instream)
  "Create an appropriate mp3 frame by reading data from INSTREAM."
  (log5:with-context "find-mp3-frames"
	(let* ((pos (stream-seek instream 0 :current))
		   (byte (stream-read-u8 instream))
		   frame-name frame-len frame-flags frame-class)

	  (log-mp3-frame "reading from position ~:d (size of stream = ~:d" pos (stream-size instream))

	  (when (zerop byte)
		(log-mp3-frame "hit padding")
		(return-from make-frame nil))	; hit padding

	  (setf frame-name
			(concatenate 'string (string (code-char byte)) (stream-read-string-with-len instream (ecase version (2 2) (3 3) (4 3)))))
	  (setf frame-len (ecase version
						(2 (stream-read-u24 instream))
						(3 (stream-read-u32 instream))
						(4 (stream-read-u32 instream :bits-per-byte 7))))

	  (when (or (= version 3) (= version 4))
		(setf frame-flags (stream-read-u16 instream)))

	  (log-mp3-frame "making frame: id:~a, version: ~d, len: ~:d, flags: ~x" frame-name version frame-len frame-flags)
	  (setf frame-class (find-frame-class frame-name))
	  (when (or (> (+ (stream-seek instream 0 :current) frame-len) (stream-size instream))
				(null frame-class))
		(error 'mp3-frame-condition :message "bad frame found" :object frame-name :location pos))
	  (make-instance frame-class :pos pos :version version :id frame-name :len frame-len :flags frame-flags :instream instream))))

(defun find-mp3-frames (mp3-file)
  "With an open mp3-file, make sure it is in fact an MP3 file, then read it's header and frames"
  (labels ((read-loop (version stream)
			 (log-mp3-frame "Starting loop through ~:d bytes" (stream-size stream))
			 (let (frames this-frame)
			   (do ()
				   ((>= (stream-seek stream 0 :current) (stream-size stream)))
				 (handler-case
					 (progn
					   (setf this-frame (make-frame version stream))
					   (when (null this-frame)
						 (log-mp3-frame "hit padding: returning ~d frames" (length frames))
						 (return-from read-loop (values t (nreverse frames))))
					   (log-mp3-frame "bottom of read-loop: pos = ~:d, size = ~:d" (stream-seek stream 0 :current) (stream-size stream))
					   (push this-frame frames))
				   (condition (c)
					 (log-mp3-frame "got condition ~a when making frame" c)
					 (return-from read-loop (values nil (nreverse frames))))))

			   (log-mp3-frame "hit end: returning ~d frames" (length frames))
			   (values t (nreverse frames)))))

	(log5:with-context "find-mp3-frames"
	  (when (not (is-valid-mp3-file mp3-file))
		(log-mp3-frame "~a is not an mp3 file" (stream-filename mp3-file))
		(error 'mp3-frame-condition :location "find-mp3-frames" :object (stream-filename mp3-file) :message "is not an mp3 file"))

	  (log-mp3-frame "~a is a valid mp3 file" (stream-filename mp3-file))

	  (setf (mp3-header mp3-file) (make-instance 'mp3-id3-header :instream mp3-file))
	  (with-slots (size ext-header frames flags version) (mp3-header mp3-file)
		(when (not (zerop size))
		  (let ((mem-stream (make-mem-stream (stream-read-sequence mp3-file size
																   :bits-per-byte (if (header-unsynchronized-p flags) 7 8)))))

			;; must make extended header here since it is subject to unsynchronization.
			(when (header-extended-p flags)
			  (setf ext-header (make-instance 'mp3-extended-header :instream mem-stream)))
			(multiple-value-bind (_ok _frames) (read-loop version mem-stream)
			  (if (not _ok)
				  (warn "File ~a had errors finding mp3 frames. potentially missed frames!" (stream-filename mp3-file)))
			  (log-mp3-frame "ok = ~a, returning ~d frames" _ok (length _frames))
			  (setf frames _frames)
			  _ok)))))))

(defun get-frame-info (mp3-file frame-id)
  (with-slots (frames) (mp3-header mp3-file)
	(dolist (f frames)
	  (if (string= frame-id (id f))
		  (return-from get-frame-info f)))))

(defun mp3-map-frames (mp3-file &key (func (constantly t)))
  (mapcar func (frames (mp3-header mp3-file))))
