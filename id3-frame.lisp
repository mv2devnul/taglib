;;; -*- Mode: Lisp;  show-trailing-whitespace: t; Base: 10; indent-tabs: nil; Syntax: ANSI-Common-Lisp; Package: ID3-FRAME; -*-
;;; Copyright (c) 2013, Mark VandenBrink. All rights reserved.

(in-package #:id3-frame)

(log5:defcategory cat-log-id3-frame)
(defmacro log-id3-frame (&rest log-stuff) `(log5:log-for (cat-log-id3-frame) ,@log-stuff))

(define-condition id3-frame-condition ()
  ((location :initarg :location :reader location :initform nil)
   (object   :initarg :object   :reader object   :initform nil)
   (messsage :initarg :message  :reader message  :initform "Undefined Condition"))
  (:report (lambda (condition stream)
             (format stream "id3-frame condition at location: <~a> with object: <~a>: message: <~a>"
                     (location condition) (object condition) (message condition)))))

(defmethod print-object ((me id3-frame-condition) stream)
  (format stream "location: <~a>, object: <~a>, message: <~a>" (location me) (object me) (message me)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ID3 header/extended header/v2.1 header ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defclass id3-header ()
  ((version        :accessor version        :initarg :version        :initform 0   :documentation "ID3 version: 2, 3, or 4")
   (revision       :accessor revision       :initarg :revision       :initform 0   :documentation "ID3 revision---is this ever non-zero?")
   (flags          :accessor flags          :initarg :flags          :initform 0   :documentation "ID3 header flags")
   (size           :accessor size           :initarg :size           :initform 0   :documentation "size of ID3 info")
   (ext-header     :accessor ext-header     :initarg :ext-header     :initform nil :documentation "holds v2.3/4 extended header")
   (frames         :accessor frames         :initarg :frames         :initform nil :documentation "holds ID3 frames")
   (v21-tag-header :accessor v21-tag-header :initarg :v21-tag-header :initform nil :documentation "old-style v2.1 header (if present)"))
  (:documentation "The ID3 header, found at start of file"))

(defun is-valid-mp3-file (mp3-file)
  "Make sure this is an MP3 file. Look for ID3 header at begining (versions 2, 3, 4) and/or end (version 2.1)
Written in this fashion so as to be 'crash-proof' when passed an arbitrary file."

  (log5:with-context "is-valid-mp3-file"
    (let ((id3)
          (valid)
          (version)
          (tag))
      (unwind-protect
           (handler-case
               (progn
                 (stream-seek mp3-file 0 :start)
                 (setf id3 (stream-read-string-with-len mp3-file 3))
                 (setf version (stream-read-u8 mp3-file))
                 (stream-seek mp3-file 128 :end)
                 (setf tag (stream-read-string-with-len mp3-file 3))

                 (log-id3-frame "id3 = ~a, version = ~d" id3 version)

                 (setf valid (or (and (string= "ID3" id3)
                                      (or (= 2 version) (= 3 version) (= 4 version)))
                                 (string= tag "TAG"))))
             (condition (c)
               (declare (ignore c))))
        (stream-seek mp3-file 0 :start))
        valid)))

 (defclass v21-tag-header ()
   ((title    :accessor title    :initarg :title    :initform nil)
    (artist   :accessor artist   :initarg :artist   :initform nil)
    (album    :accessor album    :initarg :album    :initform nil)
    (year     :accessor year     :initarg :year     :initform nil)
    (comment  :accessor comment  :initarg :comment  :initform nil)
    (track    :accessor track    :initarg :track    :initform nil :documentation "some taggers allow the last 2 bytes of comment to be used as track number")
    (genre    :accessor genre    :initarg :genre    :initform nil))
  (:documentation "ID3 V2.1 old-style tag.  If present, found in last 128 bytes of file."))

(defmethod vpprint ((me v21-tag-header) stream)
  (with-slots (title artist album year comment track genre) me
    (format stream "title = <~a>, artist = <~a>, album = <~a>, year = <~a>, comment = <~a>, track = <~d>, genre = ~d (~a)"
            title artist album year comment track genre (abstract-tag:get-id3v1-genre genre))))

;;; NB: no ":after" here
(defmethod initialize-instance ((me v21-tag-header) &key instream)
  "Read in a V2.1 tag.  Caller will have stream-seek'ed file to correct location and ensured that TAG was present"
  (log5:with-context "v21-frame-initializer"
    (log-id3-frame "reading v2.1 tag from ~:d" (stream-seek instream 0))
    (with-slots (title artist album year comment genre track) me
      (setf title    (upto-null (stream-read-string-with-len instream 30)))
      (setf artist   (upto-null (stream-read-string-with-len instream 30)))
      (setf album    (upto-null (stream-read-string-with-len instream 30)))
      (setf year     (upto-null (stream-read-string-with-len instream 4)))

      ;; In V21, a comment can be split into comment and track #
      ;; find the first #\Null then check to see if that index < 28.  If so, the check the last two bytes being
      ;; non-zero---if so, then track can be set to integer value of last two bytes

        (let* ((c (stream-read-sequence instream 30))
               (first-null (find 0 c))
               (trck 0))
          (when (and first-null (<= first-null 28))
            (setf (ldb (byte 8 8) trck) (aref c 28))
            (setf (ldb (byte 8 0) trck) (aref c 29)))
          (setf comment (upto-null (map 'string #'code-char c)))
          (if (> trck 0)
              (setf track trck)
              (setf track nil)))

      (setf genre (stream-read-u8 instream))
      (log-id3-frame "v21 tag: ~a" (vpprint me nil)))))

(defclass id3-ext-header ()
  ((size         :accessor size         :initarg :size         :initform 0)
   (flags        :accessor flags        :initarg :flags        :initform 0)
   (padding      :accessor padding      :initarg :padding      :initform 0)
   (crc          :accessor crc          :initarg :crc          :initform nil)
   (is-update    :accessor is-update    :initarg :is-update    :initform nil)
   (restrictions :accessor restrictions :initarg :restrictions :initform 0))
  (:documentation "Class representing a V2.3/4 extended header"))

(defmethod initialize-instance :after ((me id3-ext-header) &key instream version)
  "Read in the extended header.  Caller will have stream-seek'ed to correct location in file.
Note: extended headers are subject to unsynchronization, so make sure that INSTREAM has been made sync-safe.
NB: 2.3 and 2.4 extended flags are different..."
  (with-slots (size flags padding crc is-update restrictions) me
    (setf size (stream-read-u32 instream))
    (setf flags (stream-read-u16 instream)) ; reading in flags fields, must discern below 2.3/2.4
    (log-id3-frame "making id3-ext-header: version = ~d, size = ~d, flags = ~x"
                   version size flags)
    (ecase version
      (3
       (setf padding (stream-read-u32 instream))
       (when (logand flags #x8000)
         (if (not (= size 10))
             (warn-user "CRC bit set in extended header, but not enough bytes to read")
             (setf crc (stream-read-u32 instream)))))
      (4
       (when (not (= (logand #xff00 flags) 1))
         (warn-user "v2.4 extended flags length is not 1"))
       (setf flags (logand flags #xff)) ; lop off type byte (the flags length)
       (let ((len 0))
         (when (logand #x3000 flags)
           (setf len (stream-read-u8 instream))
           (when (not (zerop len)) (warn-user "v2.4 extended header is-tag length is ~d" len))
           (setf is-update t))
         (when (logand #x2000 flags)
           (setf len (stream-read-u8 instream))
           (when (not (= 5 len)) (warn-user "v2.4 extended header crc length is ~d" len))
           (setf crc (stream-read-u32 instream :bits-per-byte 7)))
         (when (logand #x1000 flags)
           (setf len (stream-read-u8 instream))
           (when (not (= 5 1)) (warn-user "v2.4 extended header restrictions length is ~d" len))
           (setf restrictions (stream-read-u8 instream))))))))

(defun ext-header-restrictions-grok (r)
  "Return a string that shows what restrictions are in an ext-header"
  (if (zerop r)
      "No restrictions"
      (with-output-to-string (s)
        (format s "Tag size restictions: ~a/"
                (ecase (ash (logand #xc0 r) -6)
                  (0 "No more than 128 frames and 1 MB total tag size")
                  (1 "No more than 64 frames and 128 KB total tag size")
                  (2 "No more than 32 frames and 40 KB total tag size")
                  (3 "No more than 32 frames and 4 KB total tag size")))
        (format s "Tag encoding restictions: ~a/"
                (ecase (ash (logand #x20 r) -5)
                  (0 "No restrictions")
                  (1 "Strings are only encoded with ISO-8859-1 [ISO-8859-1] or UTF-8 [UTF-8]")))
        (format s "Tag field size restictions: ~a/"
                (ecase (ash (logand #x18 r) -3)
                  (0 "No restrictions")
                  (1 "No string is longer than 1024 characters")
                  (2 "No string is longer than 128 characters")
                  (3 "No string is longer than 30 characters")))
        (format s "Tag image encoding restrictions: ~a/"
                (ecase (ash (logand #x04 r) -2)
                  (0 "No restrictions")
                  (1 "Images are encoded only with PNG [PNG] or JPEG [JFIF]")))
        (format s "Tag image size restrictions: ~a"
                (ecase (logand #x04 r)
                  (0 "No restrictions")
                  (1 "All images are 256x256 pixels or smaller.")
                  (2 "All images are 64x64 pixels or smaller.")
                  (3 "All images are exactly 64x64 pixels, unless required otherwise."))))))

(defmethod vpprint ((me id3-ext-header) stream)
  (with-slots (size flags padding crc is-update restrictions) me
    (format stream "extended header: size: ~d, flags: ~x, padding ~:d, crc = ~x is-update ~a, restrictions = ~x/~a~%"
            size flags padding crc is-update restrictions (ext-header-restrictions-grok restrictions))))

;;; NB: v2.2 only really defines bit-7. It does document bit-6 as being the compression flag, but then states
;;; that if it is set, the software should "ignore the entire tag if this (bit-6) is set"
(defmacro header-unsynchronized-p (flags) `(logbitp 7 ,flags)) ; all share this flag
(defmacro header-extended-p (flags)       `(logbitp 6 ,flags)) ; 2.3/2.4
(defmacro header-experimental-p (flags)   `(logbitp 5 ,flags)) ; 2.3/2.4
(defmacro header-footer-p (flags)         `(logbitp 4 ,flags)) ; 2.4 only

(defmacro print-header-flags (stream flags)
  `(format ,stream "0x~2,'0x: ~:[0/~;unsynchronized-frames/~]~:[0/~;extended-header/~]~:[0/~;expermental-tag/~]~:[0~;footer-present~]"
           ,flags
           (header-unsynchronized-p ,flags)
           (header-extended-p ,flags)
           (header-experimental-p ,flags)
           (header-footer-p ,flags)))

(defmethod vpprint ((me id3-header) stream)
  (with-slots (version revision flags v21-tag-header size ext-header frames) me
    (format stream "~a"
            (with-output-to-string (s)
              (format s "Header: version/revision: ~d/~d, flags: ~a, size = ~:d bytes; ~a; ~a"
                      version revision (print-header-flags nil flags) size
                      (if (and (header-extended-p flags) ext-header)
                          (concatenate 'string "Extended header: " (vpprint ext-header nil))
                          "No extended header")
                      (if v21-tag-header
                          (concatenate 'string "V21 tag: " (vpprint v21-tag-header nil))
                          "No V21 tag"))
              (when frames
                (format s "~&~4tFrames[~d]:~%" (length frames))
                (dolist (f frames)
                  (format s "~8t~a~%" (vpprint f nil))))))))

(defmethod initialize-instance :after ((me id3-header) &key instream &allow-other-keys)
  "Fill in an mp3-header from INSTREAM."
  (log5:with-context "id3-header-initializer"
    (with-slots (version revision flags size ext-header frames v21-tag-header) me
      (stream-seek instream 128 :end)
      (when (string= "TAG" (stream-read-string-with-len instream 3))
        (log-id3-frame "looking at last 128 bytes at ~:d to try to read id3v21 header" (stream-seek instream))
        (handler-case
            (setf v21-tag-header (make-instance 'v21-tag-header :instream instream))
          (id3-frame-condition (c)
            (log-id3-frame "reading v21 got condition: ~a" c))))

      (stream-seek instream 0 :start)
      (when (string= "ID3" (stream-read-string-with-len instream 3))
        (setf version (stream-read-u8 instream))
        (setf revision (stream-read-u8 instream))
        (setf flags (stream-read-u8 instream))
        (setf size (stream-read-u32 instream :bits-per-byte 7))
        (when (header-unsynchronized-p flags)
          (log-id3-frame "header flags indicate unsync"))
        (assert (not (header-footer-p flags)) () "Can't decode ID3 footer's yet")
        (log-id3-frame "id3 header = ~a" (vpprint me nil))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; frames ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; General plan: for each frame type we are interested in, DEFCLASS a class with
;;; specfic naming convention: frame-xxx/frame-xxxx, where xxx is valid ID3V2.2 frame name
;;; and xxxx is a valid ID3V2.[34] frame name.  Upon finding a frame name in an MP3 file,
;;; we can then do a FIND-CLASS on the "frame-xxx", and a MAKE-INSTANCE on the found class
;;; to read in that class (each defined class is assumed to have an INITIALIZE-INSTANCE method
;;; that reads in data to build class.
;;;
;;; Each frame class assumes that the STREAM being passed has been made sync-safe.
;;;
;;; For any class we don't want to parse (eg, haven't gotten around to it yet, etc), we create
;;; a RAW-FRAME class that can be subclassed.  RAW-FRAME simply reads in the frame header, and then
;;; the frame "payload" as raw OCTETS.

;;;
;;; many ID3 tags are name/value pairs, with the name/value encoded in various ways
;;; this routine assumes that the "name" is always a string with a "normal" encoding (i.e. 0, 1, 2, or 3).
;;; The "value" field accepts "normal" encoding, but also accepts any negative number, which means read
;;; the bytes an raw octets.
(defun get-name-value-pair (instream len name-encoding value-encoding)
  (log5:with-context  "get-name-value-pair"
    (log-id3-frame "reading from ~:d, len ~:d, name-encoding = ~d, value-encoding = ~d" (stream-seek instream) len name-encoding value-encoding)
    (let* ((old-pos (stream-seek instream))
           (name (stream-read-string instream :encoding name-encoding))
           (name-len (- (stream-seek instream) old-pos))
           (value))

      (log-id3-frame "name = <~a>, name-len = ~d" name name-len)
      (setf value (if (>= value-encoding 0)
                      (stream-read-string-with-len instream (- len name-len) :encoding value-encoding)
                      (stream-read-sequence instream (- len name-len)))) ; if < 0, then just read as octets

      (values name value))))

(defclass id3-frame ()
  ((pos     :accessor pos     :initarg :pos                 :documentation "the offset in the buffer were this frame was found")
   (id      :accessor id      :initarg :id                  :documentation "the 3-4 character name of this frame")
   (len     :accessor len     :initarg :len                 :documentation "the length of this frame")
   (version :accessor version :initarg :version             :documentation "the ID3-HEADER version number stored here for convenience")
   (flags   :accessor flags   :initarg :flags :initform nil :documentation "the frame's flags"))
  (:documentation "Base class for an ID3 frame.  Used for versions 2.2, 2.3, and 2.4"))

;;; The frame flags are the same for V22/V23
(defmacro frame-23-altertag-p  (frame-flags) `(logbitp 15 ,frame-flags))
(defmacro frame-23-alterfile-p (frame-flags) `(logbitp 14 ,frame-flags))
(defmacro frame-23-readonly-p  (frame-flags) `(logbitp 13 ,frame-flags))
(defmacro frame-23-compress-p  (frame-flags) `(logbitp 7 ,frame-flags))
(defmacro frame-23-encrypt-p   (frame-flags) `(logbitp 6 ,frame-flags))
(defmacro frame-23-group-p     (frame-flags) `(logbitp 5 ,frame-flags))

;;; frame flags are different for 2.4.  Also note, that some flags indicate that additional data
;;; follows the frame header and these must be read in the order of the flags
(defmacro frame-24-altertag-p  (frame-flags) `(logbitp 14 ,frame-flags)) ; no additional data
(defmacro frame-24-alterfile-p (frame-flags) `(logbitp 13 ,frame-flags)) ; no additional data
(defmacro frame-24-readonly-p  (frame-flags) `(logbitp 12 ,frame-flags)) ; no additional data
(defmacro frame-24-groupid-p   (frame-flags) `(logbitp 6 ,frame-flags))  ; one byte added to frame
(defmacro frame-24-compress-p  (frame-flags) `(logbitp 3 ,frame-flags))  ; one byte added to frame
(defmacro frame-24-encrypt-p   (frame-flags) `(logbitp 2 ,frame-flags))  ; wonky case, may or may not be set, dependin on encryption type
(defmacro frame-24-unsynch-p   (frame-flags) `(logbitp 1 ,frame-flags))  ; *may* have a 4-byte field after header, iff datalen is set
(defmacro frame-24-datalen-p   (frame-flags) `(logbitp 0 ,frame-flags))  ; if unsynch is set and this too, 4-bytes are added to frame

;; NB version 2.2 does NOT have FLAGS field in a frame; hence, the ECASE
(defun valid-frame-flags (header-version frame-flags)
  (ecase header-version
    (3 (zerop (logand #b0001111100011111 frame-flags)))
    (4 (zerop (logand #b1000111110110000 frame-flags)))))

(defun print-frame-flags (version flags stream)
  (ecase version
    (2 (format stream "None, "))
    (3 (format stream
               "flags: 0x~4,'0x: ~:[0/~;tag-alter-preservation/~]~:[0/~;file-alter-preservation/~]~:[0/~;read-only/~]~:[0/~;compress/~]~:[0/~;encypt/~]~:[0~;group~], "
               flags
               (frame-23-altertag-p flags)
               (frame-23-alterfile-p flags)
               (frame-23-readonly-p flags)
               (frame-23-compress-p flags)
               (frame-23-encrypt-p flags)
               (frame-23-group-p flags)))
    (4 (format stream
               "flags: 0x~4,'0x: ~:[0/~;tag-alter-preservation/~]~:[0/~;file-alter-preservation/~]~:[0/~;read-only/~]~:[0/~;group-id/~]~:[0/~;compress/~]~:[0/~;encypt/~]~:[0/~;unsynch/~]~:[0~;datalen~], "
               flags
               (frame-24-altertag-p flags)
               (frame-24-alterfile-p flags)
               (frame-24-readonly-p flags)
               (frame-24-groupid-p flags)
               (frame-24-compress-p flags)
               (frame-24-encrypt-p flags)
               (frame-24-unsynch-p flags)
               (frame-24-datalen-p flags)))))

(defun vpprint-frame-header (id3-frame)
  (with-output-to-string (stream)
    (with-slots (pos version id len flags) id3-frame
      (format stream "offset: ~:d, version = ~d, id: ~a, len: ~:d, ~a" pos version id len
              (if flags
                  (print-frame-flags version flags stream)
                  "flags: none")))))

(defclass frame-raw (id3-frame)
  ((octets :accessor octets :initform nil))
  (:documentation "Frame class that slurps in frame contents w/no attempt to grok them"))

(defmethod initialize-instance :after ((me frame-raw) &key instream)
  (log5:with-context "frame-raw"
    (with-slots (pos len octets) me
      (log-id3-frame "reading ~:d bytes from position ~:d" len pos)
      (setf octets (stream-read-sequence instream len))
      (log-id3-frame "frame: ~a" (vpprint me nil)))))


(defmethod vpprint ((me frame-raw) stream)
  (with-slots (octets) me
    (format stream "frame-raw: ~a, ~a" (vpprint-frame-header me) (printable-array octets))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; V2.2 frames ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
(defclass frame-stc (frame-raw) ())

;;; V22 User defined... "WXX"
;;; Text encoding       $xx
;;; Description         <textstring> $00 (00)
;;; URL                 <textstring>
;;; Identical to TXX
(defclass frame-wxx (frame-txx) ())


;; V22 COM frames
;; Comment                   "COM"
;; Text encoding             $xx
;; Language                  $xx xx xx
;; Short content description <textstring> $00 (00)
;; The actual text           <textstring>
(defclass frame-com (id3-frame)
  ((encoding :accessor encoding)
   (lang     :accessor lang)
   (desc     :accessor desc)
   (val      :accessor val)))

(defmethod initialize-instance :after ((me frame-com) &key instream)
  (log5:with-context "frame-com"
    (with-slots (len encoding lang desc val) me
      (setf encoding (stream-read-u8 instream))
      (setf lang (stream-read-iso-string-with-len instream 3))
      (multiple-value-bind (n v) (get-name-value-pair instream (- len 1 3) encoding encoding)
        (setf desc n)

        ;; iTunes broken-ness... for frame-coms, there can be an additional null or two at the end
        (setf val (upto-null v)))
      (log-id3-frame "encoding = ~d, lang = <~a>, desc = <~a>, text = <~a>" encoding lang desc val))))

(defmethod vpprint ((me frame-com) stream)
  (with-slots (len encoding lang desc val) me
    (format stream "frame-com: ~a, encoding = ~d, lang = <~a> (~a), desc = <~a>, val = <~a>"
            (vpprint-frame-header me) encoding lang (get-iso-639-2-language lang) desc val)))

;;; ULT's are same format as COM's... XXX rewrite this as suggested in comment at bottom of this file
;;; V22 unsynced lyrics/text "ULT"
;;; Text encoding        $xx
;;; Language             $xx xx xx
;;; Content descriptor   <textstring> $00 (00)
;;; Lyrics/text          <textstring>
(defclass frame-ult (frame-com) ())

;; V22 PIC frames
;; Attached picture   "PIC"
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
        (log-id3-frame "encoding: ~d, img-format = <~a>, type = ~d (~a), desc = <~a>, value = ~a"
                   encoding img-format type (get-picture-type type) desc (printable-array data))))))

(defmethod vpprint ((me frame-pic) stream)
  (with-slots (encoding img-format type desc data) me
    (format stream "frame-pic: ~a,  encoding ~d, img-format type: <~a>, picture type: ~d (~a), description <~a>, data: ~a"
            (vpprint-frame-header me) encoding img-format type (get-picture-type type) desc (printable-array data))))

;; Version 2, 3, or 4 generic text-info frames
;; Text information identifier  "T00" - "TZZ", excluding "TXX", or "T000 - TZZZ", excluding "TXXX"
;; Text encoding                $xx
;; Information                  <textstring>
(defclass frame-text-info (id3-frame)
  ((encoding :accessor encoding)
   (info     :accessor info))
  (:documentation "V2/V3/V4 T00-TZZ and T000-TZZZ frames, but not TXX or TXXX"))

(defmethod initialize-instance :after ((me frame-text-info) &key instream)
  (log5:with-context "frame-text-info"
    (with-slots (version flags len encoding info) me
      (let ((read-len len))

        ;; In version 4 frames, each frame may also have an unsync flag.  since we have unsynced already
        ;; the only thing we need to do here is check for the optional DATALEN field.  If it is present
        ;; then it has the actual number of octets to read
        (when (and (= version 4) (frame-24-unsynch-p flags))
          (if (frame-24-datalen-p flags)
              (setf read-len (stream-read-u32 instream :bits-per-byte 7))))

        (setf encoding (stream-read-u8 instream))
        (setf info (stream-read-string-with-len instream (1- read-len) :encoding encoding)))

      ;; A null is ok, but according to the "spec", you're supposed to ignore anything after a 'Null'
      (log-id3-frame "made text-info-frame: ~a" (vpprint me nil))
      (setf info (upto-null info))

      (log-id3-frame "encoding = ~d, info = <~a>" encoding info))))

(defmethod vpprint ((me frame-text-info) stream)
  (with-slots (len encoding info) me
    (format stream "frame-text-info: ~a, encoding = ~d, info = <~a>" (vpprint-frame-header me) encoding info)))

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

;; V22 User defined "TXX" frames
;; Text encoding     $xx
;; Description       <textstring> $00 (00)
;; Value             <textstring>
(defclass frame-txx (id3-frame)
  ((encoding :accessor encoding)
   (desc     :accessor desc)
   (val      :accessor val))
  (:documentation "TXX is the only frame starting with a 'T' that has a different format"))

(defmethod initialize-instance :after ((me frame-txx) &key instream)
  (log5:with-context "frame-txx"
    (with-slots (len encoding desc val) me
      (setf encoding (stream-read-u8 instream))
      (multiple-value-bind (n v) (get-name-value-pair instream (1- len) encoding encoding)
        (setf desc n)
        (setf val v)
        (log-id3-frame "encoding = ~d, desc = <~a>, val = <~a>" encoding desc val)))))

(defmethod vpprint ((me frame-txx) stream)
  (with-slots (len encoding desc val) me
    (format stream "frame-txx: ~a, encoding = ~d, desc = <~a>, val = <~a>" (vpprint-frame-header me) encoding desc val)))

;;; V22 unique file identifier  "UFI"
;;; Owner identifier        <textstring> $00
;;; Identifier              <up to 64 bytes binary data>
(defclass frame-ufi (id3-frame)
  ((name  :accessor name)
   (value :accessor value))
  (:documentation "Unique File Identifier"))

(defmethod initialize-instance :after ((me frame-ufi) &key instream)
  (log5:with-context "frame-ufi"
    (with-slots (id len name value) me
      (multiple-value-bind (n v) (get-name-value-pair instream len 0 -1)
        (setf name n)
        (setf value v))
      (log-id3-frame "name = <~a>, value = ~a" name (printable-array value)))))

(defmethod vpprint ((me frame-ufi) stream)
  (with-slots (id len name value) me
    (format stream "frame-ufi: ~a, name: <~a>, value: ~a" (vpprint-frame-header me) name (printable-array value))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; V23/V24 frames ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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

;;; V23/V24 text-info frames
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

(defparameter *picture-type*
  '("Other"
    "32x32 pixels 'file icon' (PNG only)"
    "Other file icon"
    "Cover (front)"
    "Cover (back)"
    "Leaflet page"
    "Media (e.g. lable side of CD)"
    "Lead artist/lead performer/soloist"
    "Artist/performer"
    "Conductor"
    "Band/Orchestra"
    "Composer"
    "Lyricist/text writer"
    "Recording Location"
    "During recording"
    "During performance"
    "Movie/video screen capture"
    "A bright coloured fish"    ; how do you know the fish is intelligent? :)
    "Illustration"
    "Band/artist logotype"
    "Publisher/Studio logotype"))

(defun get-picture-type (n)
  "Function to return picture types for APIC frames"
  (if (and (>= n 0) (< n (length *picture-type*)))
      (nth n *picture-type*)
      "Unknown"))

;; V23/V24 APIC frames
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
   (data     :accessor data))
  (:documentation "Holds an attached picture (cover art)"))

(defmethod initialize-instance :after ((me frame-apic) &key instream)
  (log5:with-context "frame-apic"
    (with-slots (id len encoding mime type desc data) me
      (setf encoding (stream-read-u8 instream))
      (setf mime (stream-read-iso-string instream))
      (setf type (stream-read-u8 instream))
      (multiple-value-bind (n v) (get-name-value-pair instream (- len 1 (length mime) 1 1) encoding -1)
        (setf desc n)
        (setf data v)
        (log-id3-frame "enoding = ~d, mime = <~a>, type = ~d (~a), desc = <~a>, data = ~a" encoding mime type (get-picture-type type) desc (printable-array data))))))

(defmethod vpprint ((me frame-apic) stream)
  (with-slots (encoding mime type desc data) me
    (format stream "frame-apic: ~a, encoding ~d, mime type: ~a, picture type: ~d (~a), description <~a>, data: ~a"
            (vpprint-frame-header me) encoding mime type (get-picture-type type) desc (printable-array data))))

;;; V23/V24 COMM frames
;;; <Header for 'Comment', ID: "COMM">
;;; Text encoding           $xx
;;; Language                $xx xx xx
;;; Short content descrip.  <text string according to encoding> $00 (00)
;;; The actual text         <full text string according to encoding>
(defclass frame-comm (id3-frame)
  ((encoding :accessor encoding)
   (lang     :accessor lang)
   (desc     :accessor desc)
   (val      :accessor val))
  (:documentation "V23/4 Comment frame"))

(defmethod initialize-instance :after ((me frame-comm) &key instream)
  (log5:with-context "frame-comm"
    (with-slots (encoding lang len desc val) me
      (setf encoding (stream-read-u8 instream))
      (setf lang (stream-read-iso-string-with-len instream 3))
      (multiple-value-bind (n v) (get-name-value-pair instream (- len 1 3) encoding encoding)
        (setf desc n)

        ;; iTunes broken-ness... for frame-coms, there can be an additional null or two at the end
        (setf val (upto-null v)))
      (log-id3-frame "encoding = ~d, lang = <~a>, desc = <~a>, val = <~a>" encoding lang desc val))))

(defmethod vpprint ((me frame-comm) stream)
  (with-slots (encoding lang desc val) me
    (format stream "frame-comm: ~a,  encoding: ~d, lang: <~a> (~a), desc = <~a>, val = <~a>"
            (vpprint-frame-header me) encoding lang (get-iso-639-2-language lang) desc val)))

;;; Unsynchronized lyrics frames look very much like comment frames...
(defclass frame-uslt (frame-comm) ())

;;; V23/24 PCNT frames
;;; <Header for 'Play counter', ID: "PCNT">
;;; Counter         $xx xx xx xx (xx ...)
(defclass frame-pcnt (id3-frame)
  ((play-count :accessor play-count))
  (:documentation "Play count frame"))

(defmethod initialize-instance :after ((me frame-pcnt) &key instream)
  (log5:with-context "frame-pcnt"
    (with-slots (play-count len) me
      (assert (= 4 len) () "Ran into a play count with ~d bytes" len)
      (setf play-count (stream-read-u32 instream)) ; probably safe---play count *can* be longer than 4 bytes, but...
      (log-id3-frame "play count = <~d>" play-count))))

(defmethod vpprint ((me frame-pcnt) stream)
  (with-slots (play-count) me
    (format stream "frame-pcnt: ~a, count = ~d" (vpprint-frame-header me) play-count)))

;;; V23/V24 PRIV frames
;;; <Header for 'Private frame', ID: "PRIV">
;;; Owner identifier        <text string> $00
;;; The private data        <binary data>
(defclass frame-priv (id3-frame)
  ((name  :accessor name)
   (value :accessor value))
  (:documentation "Private frame"))

(defmethod initialize-instance :after ((me frame-priv) &key instream)
  (log5:with-context "frame-priv"
    (with-slots (id len name value) me
      (multiple-value-bind (n v) (get-name-value-pair instream len 0 -1)
        (setf name n)
        (setf value v)
        (log-id3-frame "name = <~a>, value = <~a>" name value)))))

(defmethod vpprint ((me frame-priv) stream)
  (with-slots (id len name value) me
    (format stream "frame-priv: ~a, name: <~a>, data: ~a" (vpprint-frame-header me) name (printable-array value))))

;; V23/V24 TXXX frames
;; <Header for 'User defined text information frame', ID: "TXXX">
;; Text encoding    $xx
;; Description      <text string according to encoding> $00 (00)
;; Value            <text string according to encoding>
(defclass frame-txxx (id3-frame)
  ((encoding :accessor encoding)
   (desc     :accessor desc)
   (val      :accessor val))
  (:documentation "TXXX frame"))

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
      (log-id3-frame "encoding = ~d, desc = <~a>, value = <~a>" encoding desc val))))

(defmethod vpprint ((me frame-txxx) stream)
  (format stream "frame-txxx: ~a, <~a/~a>" (vpprint-frame-header me) (desc me) (val me)))

;; V23/V24 UFID frames
;; <Header for 'Unique file identifier', ID: "UFID">
;; Owner identifier    <text string> $00
;; Identifier          <up to 64 bytes binary data>
(defclass frame-ufid (id3-frame)
  ((name  :accessor name)
   (value :accessor value))
  (:documentation "Unique file identifier frame"))

(defmethod initialize-instance :after ((me frame-ufid) &key instream)
  (log5:with-context "frame-ufid"
    (with-slots (id len name value) me
      (multiple-value-bind (n v) (get-name-value-pair instream len 0 -1)
        (setf name n)
        (setf value v))
      (log-id3-frame "name = <~a>, value = ~a" name (printable-array value)))))

(defmethod vpprint ((me frame-ufid) stream)
  (with-slots (id len name value) me
    (format stream "frame-ufid: ~a,  name: <~a>, value: ~a" (vpprint-frame-header me) name (printable-array value))))

;;; V23/V24 URL frame
;;; <Header for 'URL link frame', ID: "W000" - "WZZZ", excluding "WXXX" described in 4.3.2.>
;;; URL <text string>
(defclass frame-url-link (id3-frame)
  ((url :accessor url))
  (:documentation "URL link frame"))

(defmethod initialize-instance :after ((me frame-url-link) &key instream)
  (with-slots (id len url) me
    (log5:with-context "url"
      (setf url (stream-read-iso-string-with-len instream len))
      (log-id3-frame "url = <~a>" url))))

(defmethod vpprint ((me frame-url-link) stream)
  (with-slots (url) me
    (format stream "frame-url-link: ~a, url: ~a" (vpprint-frame-header me) url)))

;;; V23/V24 frames URL link frames
(defclass frame-wcom (frame-url-link) ())
(defclass frame-wcop (frame-url-link) ())
(defclass frame-woaf (frame-url-link) ())
(defclass frame-woar (frame-url-link) ())
(defclass frame-woas (frame-url-link) ())
(defclass frame-wors (frame-url-link) ())
(defclass frame-wpay (frame-url-link) ())
(defclass frame-wpub (frame-url-link) ())

;;; Identical to frame-txx
(defclass frame-wxxx (frame-txx) ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; frame finding/creation ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun possibly-valid-frame-id? (frame-id)
  "test to see if a string is a potentially valid frame id"
  (labels ((numeric-char-p (c)
             (let ((code (char-code c)))
               (and (>= code (char-code #\0))
                    (<= code (char-code #\9))))))

    ;; test each octet to see if it is alphanumeric
    (dotimes (i (length frame-id))
      (let ((c (aref frame-id i)))
        (when (not (or (numeric-char-p c)
                       (and (alpha-char-p c) (upper-case-p c))))
          (return-from possibly-valid-frame-id? nil))))
    t))

(defun find-frame-class (id)
  "Search by concatenating 'frame-' with ID and look for that symbol in this package"
  (log5:with-context "find-frame-class"
    (log-id3-frame "looking for class <~a>" id)
    (let ((found-class-symbol (find-symbol (string-upcase (concatenate 'string "frame-" id)) :ID3-FRAME))
          found-class)

      ;; if we found the class name, return the class (to be used for MAKE-INSTANCE)
      (when found-class-symbol
        (setf found-class (find-class found-class-symbol))
        (log-id3-frame "found class: ~a" found-class)
        (return-from find-frame-class found-class))

      (log-id3-frame "didn't find class, checking general cases")

      ;; if not a "normal" frame-id, look at general cases of
      ;; starting with a 'T' or a 'W'
      (setf found-class (case (aref id 0)
                          (#\T (log-id3-frame "assuming text-info") (find-class (find-symbol "FRAME-TEXT-INFO" :ID3-FRAME)))
                          (#\W (log-id3-frame "assuming url-link")  (find-class (find-symbol "FRAME-URL-LINK"  :ID3-FRAME)))
                          (t
                           ;; we don't recognize the frame name.  if it could possibly be a real frame name,
                           ;; then just read it raw
                           (when (possibly-valid-frame-id? id)
                             (log-id3-frame "just reading raw")
                             (find-class (find-symbol "FRAME-RAW" :ID3-FRAME))))))

      (log-id3-frame "general case for id <~a> is ~a" id found-class)
      found-class)))

(defun make-frame (version instream fn)
  "Create an appropriate mp3 frame by reading data from INSTREAM."
  (log5:with-context "make-frame"
    (let* ((pos (stream-seek instream))
           (byte (stream-read-u8 instream))
           frame-name frame-len frame-flags frame-class)

      (log-id3-frame "reading from position ~:d (size of stream = ~:d)" pos (stream-size instream))

      (when (zerop byte)                ; XXX should this be correlated to PADDING in the extended header???
        (log-id3-frame "hit padding of size ~:d while making a frame" (- (stream-size instream) pos))
        (return-from make-frame nil))   ; hit padding

      (setf frame-name
            (concatenate 'string (string (code-char byte)) (stream-read-string-with-len instream (ecase version (2 2) (3 3) (4 3)))))

      (setf frame-len (ecase version
                        (2 (stream-read-u24 instream))
                        (3 (stream-read-u32 instream))
                        (4 (stream-read-u32 instream :bits-per-byte 7))))

      (when (or (= version 3) (= version 4))
        (setf frame-flags (stream-read-u16 instream))
        (when (not (valid-frame-flags version frame-flags))
          (log-id3-frame "Invalid frame flags found ~a, will ignore" (print-frame-flags version frame-flags nil))
          (warn-user "Invalid frame flags found in ~a: ~a, will ignore" fn (print-frame-flags version frame-flags nil))))

      (log-id3-frame "making frame: id:~a, version: ~d, len: ~:d, flags: ~a"
                     frame-name version frame-len
                     (print-frame-flags version frame-flags nil))
      (setf frame-class (find-frame-class frame-name))

      ;; edge case where found a frame name, but it is not valid or where making this frame
      ;; would blow past the end of the file/buffer
      (when (or (> (+ (stream-seek instream) frame-len) (stream-size instream))
                (null frame-class))
        (error 'id3-frame-condition :message "bad frame found" :object frame-name :location pos))

      (make-instance frame-class :pos pos :version version :id frame-name :len frame-len :flags frame-flags :instream instream))))

(defmethod find-id3-frames ((mp3-file mp3-file-stream))
  "With an open mp3-file, make sure it is in fact an MP3 file, then read it's header and frames"

  (labels ((read-loop (version stream)
             (log5:with-context "read-loop-in-find-id3-frames"
               (log-id3-frame "Starting loop through ~:d bytes" (stream-size stream))
               (let (frames this-frame)
                 (do ()
                     ((>= (stream-seek stream) (stream-size stream)))
                   (handler-case
                       (progn
                         (setf this-frame (make-frame version stream (stream-filename mp3-file)))
                         (when (null this-frame)
                           (log-id3-frame "hit padding: returning ~d frames" (length frames))
                           (return-from read-loop (values t (nreverse frames))))

                         (log-id3-frame "bottom of read-loop: pos = ~:d, size = ~:d" (stream-seek stream) (stream-size stream))
                         (push this-frame frames))
                     (condition (c)
                       (log-id3-frame "got condition ~a when making frame" c)
                       (return-from read-loop (values nil (nreverse frames))))))

                 (log-id3-frame "Succesful read: returning ~d frames" (length frames))
                 (values t (nreverse frames)))))) ; reverse this so we have frames in "file order"

    (log5:with-context "find-id3-frames"

      (log-id3-frame "~a is a valid mp3 file" (stream-filename mp3-file))

      (setf (id3-header mp3-file) (make-instance 'id3-header :instream mp3-file))
      (with-slots (size ext-header frames flags version) (id3-header mp3-file)

        ;; At this point, we switch from reading the file stream and create a memory stream
        ;; rationale: it may need to be unsysnc'ed and it helps prevent run-away reads with
        ;; mis-formed frames
        (when (not (zerop size))
          (let ((mem-stream (make-mem-stream (stream-read-sequence mp3-file size
                                                                   :bits-per-byte (if (header-unsynchronized-p flags) 7 8)))))

            ;; Must make extended header here since it is subject to unsynchronization.
            (when (header-extended-p flags)
              (setf ext-header (make-instance 'id3-ext-header :instream mem-stream :version version)))
            (log-id3-frame "Complete header: ~a" (vpprint (id3-header mp3-file) nil))

            ;; Start reading frames from memory stream
            (multiple-value-bind (_ok _frames) (read-loop version mem-stream)
              (if (not _ok)
                  (warn-user "File ~a had errors finding mp3 frames. potentially missed frames!" (stream-filename mp3-file)))
              (log-id3-frame "ok = ~a, returning ~d frames" _ok (length _frames))
              (setf frames _frames)
              _ok)))))))

(defun map-id3-frames (mp3-file &key (func (constantly t)))
  "Iterates through the ID3 frames found in an MP3 file"
  (mapcar func (frames (id3-header mp3-file))))

#|
XXX
Random ideas for rewrite:
-might be simplest to read in frame payloads (sync'ed appropriately) for all frames and then move parsing into
 accessor methods? This might be easier to handle sync/compression/etc.

-probably should rewrite name/value pairs as a mixin class?  Or more broadly, there is a finite set of frame-encodings,
 so abstact to that, then subclass for frame-????
|#
