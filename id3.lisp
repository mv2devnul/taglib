;;; -*- Mode: Lisp;  show-trailing-whitespace: t; Base: 10; indent-tabs: nil; Syntax: ANSI-Common-Lisp; Package: ID3; -*-
;;; Copyright (c) 2013, Mark VandenBrink. All rights reserved.

(in-package #:id3)

;;;; ID3 string encoding support
(defun id3-read-string (instream &key (len nil) (encoding 0))
  "Read in a string of a given encoding of length 'len'. Encoding
is from the ID3 'spec'"
  (declare #.utils:*standard-optimize-settings*)

  (if (and len (<= len 0))
      nil
      (ecase encoding
        (0 (stream-read-iso-string instream len))
        (1 (stream-read-ucs-string instream :len len :kind :ucs-2))
        (2 (stream-read-ucs-string instream :len len :kind :ucs-2be))
        (3 (stream-read-utf-8-string instream len)))))

(defun id3-decode-string (octets &key (encoding 0)
                                      (start 0)
                                      (end (length octets)))
  "Decode a string of a given encoding of length 'len'. Encoding
is from the ID3 'spec'"
  (declare #.utils:*standard-optimize-settings*)

  (ecase encoding
    (0 (flex:octets-to-string octets :external-format :iso-8859-1 :start start :end end))
    (1 (flex:octets-to-string octets :external-format :ucs-2 :start start :end end))
    (2 (flex:octets-to-string octets :external-format :ucs-2be :start start :end end))
    (3 (flex:octets-to-string octets :external-format :utf-8 :start start :end end))))

;;;; V22 frame types:
(defparameter *v22-frame-names* '(("BUF" "Recommended buffer size")
                                  ("CNT" "Play counter")
                                  ("COM" "Comments")
                                  ("CRA" "Audio encryption")
                                  ("CRM" "Encrypted meta frame")
                                  ("EQU" "Equalization")
                                  ("ETC" "Event timing codes")
                                  ("GEO" "General encapsulated object")
                                  ("IPL" "Involved people list")
                                  ("LNK" "Linked information")
                                  ("MCI" "Music CD Identifier")
                                  ("MLL" "MPEG location lookup table")
                                  ("PIC" "Attached picture")
                                  ("POP" "Popularimeter")
                                  ("REV" "Reverb")
                                  ("RVA" "Relative volume adjustment")
                                  ("SLT" "Synchronized lyric/text")
                                  ("STC" "Synced tempo codes")
                                  ("TAL" "Album/Movie/Show title")
                                  ("TBP" "BPM (Beats Per Minute)")
                                  ("TCM" "Composer")
                                  ("TCO" "Content type")
                                  ("TCR" "Copyright message")
                                  ("TDA" "Date")
                                  ("TDY" "Playlist delay")
                                  ("TEN" "Encoded by")
                                  ("TFT" "File type")
                                  ("TIM" "Time")
                                  ("TKE" "Initial key")
                                  ("TLA" "Language(s)")
                                  ("TLE" "Length")
                                  ("TMT" "Media type")
                                  ("TOA" "Original artist(s)/performer(s)")
                                  ("TOF" "Original filename")
                                  ("TOL" "Original Lyricist(s)/text writer(s)")
                                  ("TOR" "Original release year")
                                  ("TOT" "Original album/Movie/Show title")
                                  ("TP1" "Lead artist(s)/Lead performer(s)/Soloist(s)/Performing group")
                                  ("TP2" "Band/Orchestra/Accompaniment")
                                  ("TP3" "Conductor/Performer refinement")
                                  ("TP4" "Interpreted, remixed, or otherwise modified by")
                                  ("TPA" "Part of a set")
                                  ("TPB" "Publisher")
                                  ("TRC" "ISRC (International Standard Recording Code)")
                                  ("TRD" "Recording dates")
                                  ("TRK" "Track number/Position in set")
                                  ("TSI" "Size")
                                  ("TSS" "Software/hardware and settings used for encoding")
                                  ("TT1" "Content group description")
                                  ("TT2" "Title/Songname/Content description")
                                  ("TT3" "Subtitle/Description refinement")
                                  ("TXT" "Lyricist/text writer")
                                  ("TXX" "User defined text information frame")
                                  ("TYE" "Year")
                                  ("UFI" "Unique file identifier")
                                  ("ULT" "Unsychronized lyric/text transcription")
                                  ("WAF" "Official audio file webpage")
                                  ("WAR" "Official artist/performer webpage")
                                  ("WAS" "Official audio source webpage")
                                  ("WCM" "Commercial information")
                                  ("WCP" "Copyright/Legal information")
                                  ("WPB" "Publishers official webpage")
                                  ("WXX" "User defined URL link frame")))
;;;; V23 frame names
(defparameter *v23-frame-names* '(("AENC" "Audio encryption")
                                  ("APIC" "Attached picture")
                                  ("COMM" "Comments")
                                  ("COMR" "Commercial frame")
                                  ("ENCR" "Encryption method registration")
                                  ("EQUA" "Equalization")
                                  ("ETCO" "Event timing codes")
                                  ("GEOB" "General encapsulated object")
                                  ("GRID" "Group identification registration")
                                  ("IPLS" "Involved people list")
                                  ("LINK" "Linked information")
                                  ("MCDI" "Music CD identifier")
                                  ("MLLT" "MPEG location lookup table")
                                  ("OWNE" "Ownership frame")
                                  ("PCNT" "Play counter")
                                  ("POPM" "Popularimeter")
                                  ("POSS" "Position synchronisation frame")
                                  ("PRIV" "Private frame")
                                  ("RBUF" "Recommended buffer size")
                                  ("RVAD" "Relative volume adjustment")
                                  ("RVRB" "Reverb")
                                  ("SYLT" "Synchronized lyric/text")
                                  ("SYTC" "Synchronized tempo codes")
                                  ("TALB" "Album/Movie/Show title")
                                  ("TBPM" "BPM (beats per minute)")
                                  ("TCOM" "Composer")
                                  ("TCON" "Content type")
                                  ("TCOP" "Copyright message")
                                  ("TDAT" "Date")
                                  ("TDLY" "Playlist delay")
                                  ("TENC" "Encoded by")
                                  ("TEXT" "Lyricist/Text writer")
                                  ("TFLT" "File type")
                                  ("TIME" "Time")
                                  ("TIT1" "Content group description")
                                  ("TIT2" "Title/songname/content description")
                                  ("TIT3" "Subtitle/Description refinement")
                                  ("TKEY" "Initial key")
                                  ("TLAN" "Language(s)")
                                  ("TLEN" "Length")
                                  ("TMED" "Media type")
                                  ("TOAL" "Original album/movie/show title")
                                  ("TOFN" "Original filename")
                                  ("TOLY" "Original lyricist(s)/text writer(s)")
                                  ("TOPE" "Original artist(s)/performer(s)")
                                  ("TORY" "Original release year")
                                  ("TOWN" "File owner/licensee")
                                  ("TPE1" "Lead performer(s)/Soloist(s)")
                                  ("TPE2" "Band/orchestra/accompaniment")
                                  ("TPE3" "Conductor/performer refinement")
                                  ("TPE4" "Interpreted, remixed, or otherwise modified by")
                                  ("TPOS" "Part of a set")
                                  ("TPUB" "Publisher")
                                  ("TRCK" "Track number/Position in set")
                                  ("TRDA" "Recording dates")
                                  ("TRSN" "Internet radio station name")
                                  ("TRSO" "Internet radio station owner")
                                  ("TSIZ" "Size")
                                  ("TSRC" "ISRC (international standard recording code)")
                                  ("TSSE" "Software/Hardware and settings used for encoding")
                                  ("TXXX" "User defined text information frame")
                                  ("TYER" "Year")
                                  ("UFID" "Unique file identifier")
                                  ("USER" "Terms of use")
                                  ("USLT" "Unsychronized lyric/text transcription")
                                  ("WCOM" "Commercial information")
                                  ("WCOP" "Copyright/Legal information")
                                  ("WOAF" "Official audio file webpage")
                                  ("WOAR" "Official artist/performer webpage")
                                  ("WOAS" "Official audio source webpage")
                                  ("WORS" "Official internet radio station homepage")
                                  ("WPAY" "Payment")
                                  ("WPUB" "Publishers official webpage")
                                  ("WXXX" "User defined URL link frame")))
(defparameter *v24-frame-names* '(("AENC" "Audio encryption")
                                  ("APIC" "Attached picture")
                                  ("ASPI" "Audio seek point index")
                                  ("COMM" "Comments")
                                  ("COMR" "Commercial frame")
                                  ("ENCR" "Encryption method registration")
                                  ("EQU2" "Equalisation (2)")
                                  ("ETCO" "Event timing codes")
                                  ("GEOB" "General encapsulated object")
                                  ("GRID" "Group identification registration")
                                  ("LINK" "Linked information")
                                  ("MCDI" "Music CD identifier")
                                  ("MLLT" "MPEG location lookup table")
                                  ("OWNE" "Ownership frame")
                                  ("PCNT" "Play counter")
                                  ("POPM" "Popularimeter")
                                  ("POSS" "Position synchronisation frame")
                                  ("PRIV" "Private frame")
                                  ("RBUF" "Recommended buffer size")
                                  ("RVA2" "Relative volume adjustment (2)")
                                  ("RVRB" "Reverb")
                                  ("SEEK" "Seek frame")
                                  ("SIGN" "Signature frame")
                                  ("SYLT" "Synchronised lyric/text")
                                  ("SYTC" "Synchronised tempo codes")
                                  ("TALB" "Album/Movie/Show title")
                                  ("TBPM" "BPM (beats per minute)")
                                  ("TCOM" "Composer")
                                  ("TCON" "Content type")
                                  ("TCOP" "Copyright message")
                                  ("TDEN" "Encoding time")
                                  ("TDLY" "Playlist delay")
                                  ("TDOR" "Original release time")
                                  ("TDRC" "Recording time")
                                  ("TDRL" "Release time")
                                  ("TDTG" "Tagging time")
                                  ("TENC" "Encoded by")
                                  ("TEXT" "Lyricist/Text writer")
                                  ("TFLT" "File type")
                                  ("TIPL" "Involved people list")
                                  ("TIT1" "Content group description")
                                  ("TIT2" "Title/songname/content description")
                                  ("TIT3" "Subtitle/Description refinement")
                                  ("TKEY" "Initial key")
                                  ("TLAN" "Language(s)")
                                  ("TLEN" "Length")
                                  ("TMCL" "Musician credits list")
                                  ("TMED" "Media type")
                                  ("TMOO" "Mood")
                                  ("TOAL" "Original album/movie/show title")
                                  ("TOFN" "Original filename")
                                  ("TOLY" "Original lyricist(s)/text writer(s)")
                                  ("TOPE" "Original artist(s)/performer(s)")
                                  ("TOWN" "File owner/licensee")
                                  ("TPE1" "Lead performer(s)/Soloist(s)")
                                  ("TPE2" "Band/orchestra/accompaniment")
                                  ("TPE3" "Conductor/performer refinement")
                                  ("TPE4" "Interpreted, remixed, or otherwise modified by")
                                  ("TPOS" "Part of a set")
                                  ("TPRO" "Produced notice")
                                  ("TPUB" "Publisher")
                                  ("TRCK" "Track number/Position in set")
                                  ("TRSN" "Internet radio station name")
                                  ("TRSO" "Internet radio station owner")
                                  ("TSOA" "Album sort order")
                                  ("TSOP" "Performer sort order")
                                  ("TSOT" "Title sort order")
                                  ("TSRC" "ISRC (international standard recording code)")
                                  ("TSSE" "Software/Hardware and settings used for encoding")
                                  ("TSST" "Set subtitle")
                                  ("TXXX" "User defined text information frame")
                                  ("UFID" "Unique file identifier")
                                  ("USER" "Terms of use")
                                  ("USLT" "Unsynchronised lyric/text transcription")
                                  ("WCOM" "Commercial information")
                                  ("WCOP" "Copyright/Legal information")
                                  ("WOAF" "Official audio file webpage")
                                  ("WOAR" "Official artist/performer webpage")
                                  ("WOAS" "Official audio source webpage")
                                  ("WORS" "Official Internet radio station homepage")
                                  ("WPAY" "Payment")
                                  ("WPUB" "Publishers official webpage")
                                  ("WXXX" "User defined URL link frame")))

(defstruct frame-db-entry
  text
  (is-v22 nil)
  (is-v23 nil)
  (is-v24 nil))

(defparameter *frame-db* nil)

(defmacro make-frame-db ()
  `(progn
     (setf *frame-db* (make-hash-table :test #'equalp))
     (dolist (s *v22-frame-names*)
       (setf (gethash (first s) *frame-db*)
             (make-frame-db-entry :text (second s) :is-v22 t)))
     (dolist (s *v23-frame-names*)
       (setf (gethash (first s) *frame-db*)
             (make-frame-db-entry :text (second s) :is-v23 t)))
     (dolist (s *v24-frame-names*)
       (multiple-value-bind (val found) (gethash (first s) *frame-db*)
         (setf (gethash (first s) *frame-db*)
               (if found
                   (progn
                     (setf (frame-db-entry-is-v24 val) t)
                     val)
                   (make-frame-db-entry :text (second s) :is-v24 t)))))))
(make-frame-db)

(defun get-frame-db-entry (id)
  "Given a frame id/name, return the associated FRAME-DB-ENTRY"
  (declare #.utils:*standard-optimize-settings*)

  (gethash id *frame-db*))

;;;; ID3 header/extended header/v2.1 header
(defclass id3-header ()
  ((version        :accessor version        :initarg :version        :initform 0   :documentation "ID3 version: 2, 3, or 4")
   (revision       :accessor revision       :initarg :revision       :initform 0   :documentation "ID3 revision---is this ever non-zero?")
   (flags          :accessor flags          :initarg :flags          :initform 0   :documentation "ID3 header flags")
   (size           :accessor size           :initarg :size           :initform 0   :documentation "size of ID3 info")
   (padding-size   :accessor padding-size   :initarg :padding-size   :initform 0   :documentation "padding size in tags")
   (ext-header     :accessor ext-header     :initarg :ext-header     :initform nil :documentation "holds v2.3/4 extended header")
   (frames         :accessor frames         :initarg :frames         :initform nil :documentation "holds ID3 frames")
   (v21-tag-header :accessor v21-tag-header :initarg :v21-tag-header :initform nil :documentation "old-style v2.1 header (if present)"))
  (:documentation "The ID3 header, found at start of file"))

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
  (declare #.utils:*standard-optimize-settings*)

  (with-slots (title artist album year comment genre track) me
    (setf title    (upto-null (stream-read-iso-string instream 30))
          artist   (upto-null (stream-read-iso-string instream 30))
          album    (upto-null (stream-read-iso-string instream 30))
          year     (upto-null (stream-read-iso-string instream 4)))

    ;; In V21, a comment can be split into comment and track #
    ;; find the first #\Null then check to see if that index < 28.  If so,
    ;; check the last two bytes being non-zero---if so, track can be set to
    ;; integer value of last two bytes
    (let* ((c (stream-read-sequence instream 30))
           (first-null (find 0 c))
           (trck 0))
      (when (and first-null (<= first-null 28))
        (setf (ldb (byte 8 8) trck) (aref c 28)
              (ldb (byte 8 0) trck) (aref c 29)))

      (setf comment (upto-null (map 'string #'code-char c)))
      (if (> trck 0)
          (setf track trck)
          (setf track nil)))

    (setf genre (stream-read-u8 instream))))

(defclass id3-ext-header ()
  ((size         :accessor size         :initarg :size         :initform 0)
   (flags        :accessor flags        :initarg :flags        :initform 0)
   (padding      :accessor padding      :initarg :padding      :initform 0)
   (crc          :accessor crc          :initarg :crc          :initform nil)
   (is-update    :accessor is-update    :initarg :is-update    :initform nil)
   (restrictions :accessor restrictions :initarg :restrictions :initform 0))
  (:documentation "Class representing a V2.3/4 extended header"))

(defmethod initialize-instance :after ((me id3-ext-header) &key instream version)
  "Read in the extended header.  Caller will have stream-seek'ed to correct
location in file. Note: extended headers are subject to unsynchronization, so
make sure that INSTREAM has been made sync-safe. NB: 2.3 and 2.4 extended flags
are different..."
  (declare #.utils:*standard-optimize-settings*)

  (with-slots (size flags padding crc is-update restrictions) me
    (setf size  (stream-read-u32 instream)
          flags (stream-read-u16 instream)) ; reading in flags fields, must discern below 2.3/2.4
    (ecase version
      (3
       (setf padding (stream-read-u32 instream))
       (when (logand flags #x8000)
         (if (not (= size 10))
             (warn-user "file ~a:~%CRC bit set in extended header, but not enough bytes to read"
                        audio-streams:*current-file*)
             (setf crc (stream-read-u32 instream)))))
      (4
       (when (not (= (logand #xff00 flags) 1))
         (warn-user "file ~a:~%v2.4 extended flags length is not 1"
                    audio-streams:*current-file*))
       (setf flags (logand flags #xff)) ; lop off type byte (the flags length)
       (let ((len 0))
         (when (logand #x3000 flags)
           (setf len (stream-read-u8 instream))
           (when (not (zerop len))
             (warn-user "file ~a:~%v2.4 extended header is-tag length is ~d"
                        audio-streams:*current-file* len))
           (setf is-update t))
         (when (logand #x2000 flags)
           (setf len (stream-read-u8 instream))
           (when (not (= 5 len))
             (warn-user "file ~a:~%v2.4 extended header crc length is ~d"
                        audio-streams:*current-file* len))
           (setf crc (stream-read-u32 instream :bits-per-byte 7)))
         (when (logand #x1000 flags)
           (setf len (stream-read-u8 instream))
           (when (not (= 5 1))
             (warn-user "file ~a:~%v2.4 extended header restrictions length is ~d"
                        audio-streams:*current-file* len))
           (setf restrictions (stream-read-u8 instream))))))))

(defun ext-header-restrictions-grok (r)
  "Return a string that shows what restrictions are in an ext-header"
  (declare #.utils:*standard-optimize-settings*)

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

;;; NB: v2.2 only really defines bit-7. It does document bit-6 as being the
;;; compression flag, but then states that if it is set, the software should
;;; "ignore the entire tag if this (bit-6) is set"
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
  (with-slots (version revision flags v21-tag-header padding-size size ext-header frames) me
    (format stream "~a"
            (with-output-to-string (s)
              (format s "Header: version/revision: ~d/~d, flags: ~a, size = ~:d bytes; padding: ~:d bytes; ~a; ~a"
                      version revision (print-header-flags nil flags) size padding-size
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
  (declare #.utils:*standard-optimize-settings*)

  (with-slots (version revision flags size ext-header frames v21-tag-header) me
    (stream-seek instream 128 :end)
    (when (string= "TAG" (stream-read-iso-string instream 3))
      (handler-case
          (setf v21-tag-header (make-instance 'v21-tag-header :instream instream))
        (condition (c)
          (warn-user "file ~a:~%Initialize id3-header got condition ~a"
                           audio-streams:*current-file* c))))

    (stream-seek instream 0 :start)
    (when (string= "ID3" (stream-read-iso-string instream 3))
      (setf version  (stream-read-u8 instream)
            revision (stream-read-u8 instream)
            flags    (stream-read-u8 instream)
            size     (stream-read-u32 instream :bits-per-byte 7))
      (assert (not (header-footer-p flags)) () "Can't decode ID3 footer's yet"))))

;;;; Frames
;;;
;;; General plan: for each frame type we are interested in, DEFCLASS a
;;; class with specfic naming convention: frame-xxx/frame-xxxx, where xxx
;;; is valid ID3V2.2 frame name and xxxx is a valid ID3V2.[34] frame name.
;;; Upon finding a frame name in an MP3 file, we can then do a FIND-CLASS
;;; on the "frame-xxx", and a MAKE-INSTANCE on the found class to read in
;;; that class (each defined class is assumed to have an
;;; INITIALIZE-INSTANCE method that reads in data to build class.
;;;
;;; Each frame class assumes that the STREAM being passed has been made
;;; sync-safe.
;;;
;;; For any class we don't want to parse (eg, haven't gotten around to it
;;; yet, etc), we create a RAW-FRAME class that can be subclassed.
;;; RAW-FRAME simply reads in the frame header, and then the frame
;;; "payload" as raw OCTETS.

;;; Many ID3 tags are name/value pairs, with the name/value encoded in
;;; various ways this routine assumes that the "name" is always a string
;;; with a "normal" encoding (i.e. 0, 1, 2, or 3).  The "value" field
;;; accepts "normal" encoding, but also accepts any negative number, which
;;; means read the bytes an raw octets.
(defun get-name-value-pair (instream len name-encoding value-encoding)
  (declare #.utils:*standard-optimize-settings*)

  (let* ((old-pos  (stream-seek instream))
         (name     (id3-read-string instream :encoding name-encoding))
         (name-len (- (stream-seek instream) old-pos))
         (value))

    (setf value (if (>= value-encoding 0)
                    (id3-read-string instream :len (- len name-len)
                                              :encoding value-encoding)
                    (stream-read-sequence instream (- len name-len)))) ; if < 0, then just read as octets

    (values name value)))

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
  (declare #.utils:*standard-optimize-settings*)

  (ecase header-version
    (3 (zerop (logand #b0001111100011111 frame-flags)))
    (4 (zerop (logand #b1000111110110000 frame-flags)))))

(defun print-frame-flags (version flags stream)
  (declare #.utils:*standard-optimize-settings*)

  (ecase version
    (2 (format stream "None"))
    (3 (format stream
               "0x~4,'0x: ~:[0/~;tag-alter-preservation/~]~:[0/~;file-alter-preservation/~]~:[0/~;read-only/~]~:[0/~;compress/~]~:[0/~;encypt/~]~:[0~;group~]"
               flags
               (frame-23-altertag-p flags)
               (frame-23-alterfile-p flags)
               (frame-23-readonly-p flags)
               (frame-23-compress-p flags)
               (frame-23-encrypt-p flags)
               (frame-23-group-p flags)))
    (4 (format stream
               "flags: 0x~4,'0x: ~:[0/~;tag-alter-preservation/~]~:[0/~;file-alter-preservation/~]~:[0/~;read-only/~]~:[0/~;group-id/~]~:[0/~;compress/~]~:[0/~;encypt/~]~:[0/~;unsynch/~]~:[0~;datalen~]"
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
      (format stream "offset: ~:d, version = ~d, id: ~a, description: ~a, len: ~:d, flags: ~a"
              pos version id
              (aif (get-frame-db-entry id)
                   (frame-db-entry-text it)
                   "Non-standard")
              len
              (if flags
                  (print-frame-flags version flags nil)
                  "flags: none")))))

(defclass frame-raw (id3-frame)
  ((octets :accessor octets :initform nil))
  (:documentation "Frame class that slurps in frame contents w/no attempt to grok them"))

(defmethod initialize-instance :after ((me frame-raw) &key instream)
  (declare #.utils:*standard-optimize-settings*)

  (with-slots (pos len octets) me
    (setf octets (stream-read-sequence instream len))))

(defmethod vpprint ((me frame-raw) stream)
  (with-slots (octets) me
    (format stream "frame-raw: ~a, ~a" (vpprint-frame-header me) (printable-array octets))))

;;;; V2.2 frames

;;; Frames we need to implement someday
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
  (declare #.utils:*standard-optimize-settings*)

  (with-slots (len encoding lang desc val) me
    (setf encoding (stream-read-u8 instream)
          lang     (stream-read-iso-string instream 3))
    (multiple-value-bind (n v) (get-name-value-pair instream (- len 1 3) encoding encoding)
      (setf desc n)

      ;; iTunes broken-ness... for frame-coms, there can be an additional null or two at the end
      (setf val (upto-null v)))))

(defmethod vpprint ((me frame-com) stream)
  (with-slots (len encoding lang desc val) me
    (format stream "frame-com: ~a, encoding = ~d, lang = <~a> (~a), desc = <~a>, val = <~a>"
            (vpprint-frame-header me) encoding lang (get-iso-639-2-language lang) desc val)))

;;; ULT's are same format as COM's...
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
   (ptype      :accessor ptype)
   (desc       :accessor desc)
   (data       :accessor data)))

(defmethod initialize-instance :after ((me frame-pic) &key instream)
  (declare #.utils:*standard-optimize-settings*)

  (with-slots (id len encoding img-format ptype desc data) me
    (setf encoding   (stream-read-u8 instream)
          img-format (stream-read-iso-string instream 3)
          ptype      (stream-read-u8 instream))
    (multiple-value-bind (n v) (get-name-value-pair instream (- len 5) encoding -1)
      (setf desc n
            data v))))

(defmethod vpprint ((me frame-pic) stream)
  (with-slots (encoding img-format ptype desc data) me
    (format stream "frame-pic: ~a,  encoding ~d, img-format type: <~a>, picture type: ~d (~a), description <~a>, data: ~a"
            (vpprint-frame-header me) encoding img-format ptype (get-picture-type ptype) desc (printable-array data))))

(defmethod picture-info ((me frame-pic))
  "Used by ABSTRACT-TAG interface to report data about V2.2 cover art"
  (with-slots (encoding img-format ptype desc data) me
    (format nil "Size: ~:d" (length data))))

;; Version 2, 3, or 4 generic text-info frames
;; Text information identifier  "T00" - "TZZ", excluding "TXX", or "T000 - TZZZ", excluding "TXXX"
;; Text encoding                $xx
;; Information                  <textstring>
(defclass frame-text-info (id3-frame)
  ((encoding :accessor encoding)
   (info     :accessor info))
  (:documentation "V2/V3/V4 T00-TZZ and T000-TZZZ frames, but not TXX or TXXX"))

(defmethod initialize-instance :after ((me frame-text-info) &key instream)
  (declare #.utils:*standard-optimize-settings*)

  (with-slots (version flags len encoding info) me
    (let ((read-len len))

      ;; In version 4 frames, each frame may also have an unsync flag.  since we
      ;; have unsynced already the only thing we need to do here is check for
      ;; the optional DATALEN field.  If it is present then it has the actual
      ;; number of octets to read
      (when (and (= version 4) (frame-24-unsynch-p flags))
        (if (frame-24-datalen-p flags)
            (setf read-len (stream-read-u32 instream :bits-per-byte 7))))

      (setf encoding (stream-read-u8 instream)
            info     (id3-read-string instream :len (1- read-len) :encoding encoding)))

    ;; A null is ok, but according to the "spec", you're supposed to
    ;; ignore anything after a 'Null'
    (setf info (upto-null info))))

(defmethod vpprint ((me frame-text-info) stream)
  (with-slots (len encoding info) me
    (format stream "frame-text-info: ~a, encoding = ~d, info = <~a>"
            (vpprint-frame-header me) encoding info)))

(defclass frame-tal (frame-text-info) ())
(defclass frame-tbp (frame-text-info) ())
(defclass frame-tcm (frame-text-info) ())
(defclass frame-tco (frame-text-info) ())
(defclass frame-tsa (frame-text-info) ())
(defclass frame-tsc (frame-text-info) ())
(defclass frame-tsp (frame-text-info) ())
(defclass frame-ts2 (frame-text-info) ())

(defclass frame-itunes-compilation (frame-raw)
  ((info :accessor info)))

(defclass frame-tcp (frame-itunes-compilation) ())

(defmethod initialize-instance :after ((me frame-itunes-compilation) &key &allow-other-keys)
  "iTunes compilation weirdness: I have seen this encoded soooo many ways..."
  (declare #.utils:*standard-optimize-settings*)

  (with-slots (len octets info) me
    (setf info
          (cond
            ((= 1 len) (if (= 0 (aref octets 0)) "0" "1"))
            ((= 2 len) (if (= #x30 (aref octets 1)) "0" "1"))
            ((= 3 len) (if (typep me 'frame-tcp)
                           (upto-null (id3-decode-string octets
                                                         :start 1
                                                         :encoding (aref octets 0)))
                           "0"))
            ((= 4 len) "0")
            (t (upto-null (id3-decode-string octets
                                             :start 1
                                             :encoding (aref octets 0))))))))

(defmethod vpprint ((me frame-itunes-compilation) stream)
  (with-slots (octets info) me
      (format stream "frame-itunes-compilation: ~a, octets:<~a>, info:~a"
              (vpprint-frame-header me) (printable-array octets) info)))

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
  (declare #.utils:*standard-optimize-settings*)

  (with-slots (len encoding desc val) me
    (setf encoding (stream-read-u8 instream))
    (multiple-value-bind (n v) (get-name-value-pair instream (1- len) encoding encoding)
      (setf desc n
            val v))))

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
  (declare #.utils:*standard-optimize-settings*)

  (with-slots (id len name value) me
    (multiple-value-bind (n v) (get-name-value-pair instream len 0 -1)
      (setf name n
            value v))))

(defmethod vpprint ((me frame-ufi) stream)
  (with-slots (id len name value) me
    (format stream "frame-ufi: ~a, name: <~a>, value: ~a" (vpprint-frame-header me) name (printable-array value))))

;;;; V23/V24 frames

;;; Frames we need to implement someday
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
(defclass frame-rgad (frame-raw) ())
(defclass frame-rva2 (frame-raw) ())
(defclass frame-rvad (frame-raw) ())
(defclass frame-rvrb (frame-raw) ())
(defclass frame-seek (frame-raw) ())
(defclass frame-sign (frame-raw) ())
(defclass frame-sylt (frame-raw) ())
(defclass frame-sytc (frame-raw) ())
(defclass frame-user (frame-raw) ())
(defclass frame-xdor (frame-raw) ())
(defclass frame-xsop (frame-raw) ())

;;; V23/V24 text-info frames
(defclass frame-talb (frame-text-info) ())
(defclass frame-tbpm (frame-text-info) ())

(defclass frame-tcmp (frame-itunes-compilation) ())

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
  (declare #.utils:*standard-optimize-settings*)

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
   (ptype    :accessor ptype)
   (desc     :accessor desc)
   (data     :accessor data))
  (:documentation "Holds an attached picture (cover art)"))

(defmethod initialize-instance :after ((me frame-apic) &key instream)
  (declare #.utils:*standard-optimize-settings*)

  (with-slots (id len encoding mime ptype desc data) me
    (setf encoding (stream-read-u8 instream)
          mime     (stream-read-iso-string instream)
          ptype    (stream-read-u8 instream))
    (multiple-value-bind (n v) (get-name-value-pair instream (- len 1 (length mime) 1 1) encoding -1)
      (setf desc n
            data v))))

(defmethod vpprint ((me frame-apic) stream)
  (with-slots (encoding mime ptype desc data) me
    (format stream "frame-apic: ~a, encoding ~d, mime type: ~a, picture type: ~d (~a), description <~a>, data: ~a"
            (vpprint-frame-header me) encoding mime ptype (get-picture-type ptype) desc (printable-array data))))

(defmethod picture-info ((me frame-apic))
  "Used by ABSTRACT-TAG interface to report data about V2.3/4 cover art"
  (with-slots (encoding mime ptype desc data) me
    (format nil "Size: ~:d" (length data))))

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
  (declare #.utils:*standard-optimize-settings*)

  (with-slots (encoding lang len desc val) me
    (setf encoding (stream-read-u8 instream)
          lang     (stream-read-iso-string instream 3))
    (multiple-value-bind (n v) (get-name-value-pair instream (- len 1 3) encoding encoding)
      (setf desc n)

      ;; iTunes broken-ness... for frame-coms, there can be an additional null or two at the end
      (setf val (upto-null v)))))

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
  (declare #.utils:*standard-optimize-settings*)

  (with-slots (play-count len) me
    (assert (= 4 len) () "Ran into a play count with ~d bytes" len)
    (setf play-count (stream-read-u32 instream)))) ; probably safe---play count *can* be longer than 4 bytes, but...

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
  (declare #.utils:*standard-optimize-settings*)

  (with-slots (id len name value) me
    (multiple-value-bind (n v) (get-name-value-pair instream len 0 -1)
      (setf name n
            value v))))

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
  (declare #.utils:*standard-optimize-settings*)
  (with-slots (encoding len desc val) me
    (setf encoding (stream-read-u8 instream))
    (multiple-value-bind (n v) (get-name-value-pair instream
                                                    (- len 1)
                                                    encoding
                                                    encoding)
      (setf desc n
            val v))))

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
  (declare #.utils:*standard-optimize-settings*)

  (with-slots (id len name value) me
    (multiple-value-bind (n v) (get-name-value-pair instream len 0 -1)
      (setf name n
            value v))))

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
  (declare #.utils:*standard-optimize-settings*)

  (with-slots (id len url) me
    (setf url (stream-read-iso-string instream len))))

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

;;;; Frame finding/creation

(defun possibly-valid-frame-id? (frame-id)
  "test to see if a string is a potentially valid frame id"
  (declare #.utils:*standard-optimize-settings*)

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
(memoize 'possibly-valid-frame-id?)

(defun mk-frame-class-name (id)
  (declare #.utils:*standard-optimize-settings*)

  (string-upcase (concatenate 'string "frame-" id)))
(memoize 'mk-frame-class-name)

(defun find-frame-class (id)
  "Search by concatenating 'frame-' with ID and look for that symbol in this package"
  (declare #.utils:*standard-optimize-settings*)

  (let ((found-class-symbol (find-symbol (mk-frame-class-name id) :ID3))
        found-class)

    ;; if we found the class name, return the class (to be used for MAKE-INSTANCE)
    (when found-class-symbol
      (setf found-class (find-class found-class-symbol))
      (return-from find-frame-class found-class))

    ;; if not a "normal" frame-id, look at general cases of
    ;; starting with a 'T' or a 'W'
    (setf found-class (case (aref id 0)
                        (#\T (find-class (find-symbol "FRAME-TEXT-INFO" :ID3)))
                        (#\W (find-class (find-symbol "FRAME-URL-LINK"  :ID3)))
                        (t
                         ;; we don't recognize the frame name.  if it could
                         ;; possibly be a real frame name, then just read
                         ;; it raw
                         (when (possibly-valid-frame-id? id)
                           (warn-user "file ~a~%Unknown frame type <~a> encountered~%"
                                      audio-streams:*current-file* id)
                           (find-class (find-symbol "FRAME-RAW" :ID3))))))
    found-class))

(memoize 'find-frame-class)

(defun make-frame (version instream fn)
  "Create an appropriate mp3 frame by reading data from INSTREAM."
  (declare #.utils:*standard-optimize-settings*)

  (let* ((pos  (stream-seek instream))
         (byte (stream-read-u8 instream))
         frame-name frame-len frame-flags frame-class)

    (when (zerop byte) ; XXX should this be correlated to PADDING in the extended header???
      (return-from make-frame nil))     ; hit padding

    ;; I have seen 3-char frame names where 4-chars were supposed to be...
    (setf frame-name
          (string-right-trim '(#\Space #\Null)
                             (concatenate 'string (string (code-char byte))
                                          (id3-read-string instream
                                                           :len (ecase version
                                                                  (2 2)
                                                                  (3 3)
                                                                  (4 3))))))
    (setf frame-len (ecase version
                      (2 (stream-read-u24 instream))
                      (3 (stream-read-u32 instream))
                      (4 (stream-read-u32 instream :bits-per-byte 7))))

    (when (or (= version 3) (= version 4))
      (setf frame-flags (stream-read-u16 instream))
      (when (not (valid-frame-flags version frame-flags))
        (warn-user "file: ~a~%Invalid frame flags found: ~a; will ignore"
                   fn
                   (print-frame-flags version frame-flags nil))))
    (setf frame-class (find-frame-class frame-name))

    ;; edge case where found a frame name, but it is not valid or where
    ;; making this frame would blow past the end of the file/buffer
    (when (or (> (+ (stream-seek instream) frame-len) (stream-size instream))
              (null frame-class))
      (error "bad frame at position ~d found: ~a" pos frame-name))

    (make-instance frame-class :pos pos :version version :id frame-name :len frame-len :flags frame-flags :instream instream)))

(defun is-valid-mp3-file (instream)
  "Make sure this is an MP3 file. Look for ID3 header at begining (versions 2,
 3, 4) and/or end (version 2.1) Written in this fashion so as to be
 'crash-proof' when passed an arbitrary file."
  (declare #.utils:*standard-optimize-settings*)

  (let ((id3)
        (valid nil)
        (version)
        (tag))

    (when (> (stream-size instream) 4)
      (stream-seek instream 0 :start)
      (setf id3     (stream-read-iso-string instream 3)
            version (stream-read-u8 instream))
      (when (> (stream-size instream) 128)
        (stream-seek instream 128 :end)
        (setf tag (stream-read-iso-string instream 3)))

      (setf valid (or (and (string= "ID3" id3)
                           (or (= 2 version) (= 3 version) (= 4 version)))
                      (string= tag "TAG"))))
    (stream-seek instream 0 :start)
    valid))

(defclass mp3-file ()
  ((filename   :accessor filename :initform nil :initarg :filename
               :documentation "filename that was parsed")
   (id3-header :accessor id3-header :initform nil
               :documentation "holds all the ID3 info")
   (audio-info :accessor audio-info :initform nil
               :documentation "holds the bit-rate, etc info"))
  (:documentation "Output of parsing MP3 files"))

(defun parse-audio-file (instream &optional get-audio-info)
  "Parse an MP3 file"
  (declare #.utils:*standard-optimize-settings*)

  (let ((parsed-info))
    (labels ((read-loop (version stream)
               (let (frames this-frame)
                 (do ()
                     ((>= (stream-seek stream) (stream-size stream)))
                   (handler-case
                       (progn
                         (setf this-frame (make-frame version stream
                                                      (stream-filename instream)))
                         (when (null this-frame)
                           (setf (padding-size (id3-header parsed-info))
                                 (- (stream-size stream)
                                    (stream-seek stream)))
                           (return-from read-loop (values t (nreverse frames))))

                         (push this-frame frames))
                     (condition (c)
                       (warn-user "file ~a:~%Id3 parse-audio-file got condition ~a"
                                  audio-streams:*current-file* c)
                       (return-from read-loop (values nil (nreverse frames))))))

                 (values t (nreverse frames))))) ; frames in "file order"

      (setf parsed-info (make-instance 'mp3-file
                                       :filename (stream-filename instream)))
      (setf (id3-header parsed-info) (make-instance 'id3-header :instream instream))
      (with-slots (size ext-header frames flags version) (id3-header parsed-info)

        ;; At this point, we switch from reading the file stream and create a
        ;; memory stream rationale: it may need to be unsysnc'ed and it helps
        ;; prevent run-away reads with mis-formed frames
        (when (not (zerop size))
          (let ((mem-stream
                  (make-audio-stream (stream-read-sequence
                                      instream size
                                      :bits-per-byte
                                      (if (header-unsynchronized-p flags) 7 8)))))

            ;; Make extended header here since it is subject to unsynchronization.
            (when (header-extended-p flags)
              (setf ext-header (make-instance 'id3-ext-header
                                              :instream mem-stream
                                              :version version)))

            ;; Start reading frames from memory stream
            (multiple-value-bind (_ok _frames) (read-loop version mem-stream)
              (if (not _ok)
                  (warn-user
                   "file ~a:~%Had errors finding mp3 frames. potentially missed frames!"
                   (stream-filename instream)))
              (setf frames _frames))))
        (when get-audio-info
          (mpeg:get-mpeg-audio-info instream parsed-info))
        parsed-info))))

(defun map-id3-frames (mp3 &key (func (constantly t)))
  "Iterates through the ID3 frames found in an MP3 file"
  (declare #.utils:*standard-optimize-settings*)

  (mapcar func (frames (id3-header mp3))))

(defun get-frames (mp3 names)
  "Given a MP3 file's info, search its frames for NAMES.
Return file-order list of matching frames"
  (declare #.utils:*standard-optimize-settings*)

  (let (found-frames)
    (map-id3-frames mp3
                    :func (lambda (f)
                            (when (member (id f) names :test #'string=)
                              (push f found-frames))))
    (nreverse found-frames)))
