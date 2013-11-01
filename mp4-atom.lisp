;;; -*- Mode: Lisp;  show-trailing-whitespace: t; Base: 10; indent-tabs: nil; Syntax: ANSI-Common-Lisp; Package: MP4-ATOM; -*-
;;; Copyright (c) 2013, Mark VandenBrink. All rights reserved.
(in-package #:mp4-atom)

(log5:defcategory cat-log-mp4-atom)
(defmacro log-mp4-atom (&rest log-stuff) `(log5:log-for (cat-log-mp4-atom) ,@log-stuff))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ATOMS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; A word about atoms (aka "boxes").  There are three kinds of atoms: ones that are containers, ones
;;; that are data, and ones that are both. A lot of the source code for taggers out there mostly ignore
;;; the third class and treat "container atoms that also have data" as a big blob of data that they
;;; rummage around in via indices.  Seems sort of broken, IMHO, so we'll try to handle all three if
;;; at all possible.
;;;

(defun as-int (str)
  "Given a 4-byte string, return an integer type equivalent.
(eg (as-int \"hdlr\" == +audioprop-hdlr+))"
  (declare #.utils:*standard-optimize-settings*)
  (declare (type (simple-array character 1) str))

  (let ((int 0))
    (declare (fixnum int))
    (setf (ldb (byte 8 24) int) (char-code (aref str 0))
          (ldb (byte 8 16) int) (char-code (aref str 1))
          (ldb (byte 8 8) int)  (char-code (aref str 2))
          (ldb (byte 8 0) int)  (char-code (aref str 3)))

    int))

(defun as-string (atom-type)
  (declare #.utils:*standard-optimize-settings*)
  (declare (fixnum atom-type))
  (with-output-to-string (s nil)
    (write-char (code-char (ldb (byte 8 24) atom-type)) s)
    (write-char (code-char (ldb (byte 8 16) atom-type)) s)
    (write-char (code-char (ldb (byte 8 8)  atom-type)) s)
    (write-char (code-char (ldb (byte 8 0)  atom-type)) s)))
(utils:memoize 'as-string)

(defun mk-atom-class-name (name)
  (string-upcase (concatenate 'string "atom-" (as-string name))))
(utils:memoize 'mk-atom-class-name)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun as-octet (c)
    "Used below so that we can create atom 'types' from char/ints"
    (declare #.utils:*standard-optimize-settings*)
    (cond ((typep c 'standard-char) (coerce (char-code c) '(unsigned-byte 8)))
          ((typep c 'integer) (coerce c '(unsigned-byte 8)))
          (t (error "can only handle characters and integers"))))

  (defmacro mk-mp4-atom-type (l1 l2 l3 l4)
    "Given 4 chars/ints, create a 32-bit word representing an atom 'type' (aka name)"
    `(let ((retval 0))
       (setf (ldb (byte 8 24) retval) ,(as-octet l1)
             (ldb (byte 8 16) retval) ,(as-octet l2)
             (ldb (byte 8 8) retval)  ,(as-octet l3)
             (ldb (byte 8 0) retval)  ,(as-octet l4))
       retval)))

(defconstant +itunes-album+          (mk-mp4-atom-type #xa9 #\a #\l #\b) "text: album name")
(defconstant +itunes-album-artist+   (mk-mp4-atom-type #\a  #\A #\R #\T) "text: album artist")
(defconstant +itunes-artist+         (mk-mp4-atom-type #xa9 #\A #\R #\T) "text: artist name")
(defconstant +itunes-comment+        (mk-mp4-atom-type #xa9 #\c #\m #\t) "text: comment, commonly used by iTunes for sound info, etc")
(defconstant +itunes-compilation+    (mk-mp4-atom-type #\c  #\p #\i #\l) "byte/boolean: is this file part of a compilation?")
(defconstant +itunes-composer+       (mk-mp4-atom-type #xa9 #\c #\o #\m) "text: composer name")
(defconstant +itunes-copyright+      (mk-mp4-atom-type #\c  #\p #\r #\t) "text: copyright info")
(defconstant +itunes-cover-art+      (mk-mp4-atom-type #\c  #\o #\v #\r) "octets: cover art, PNG, etc")
(defconstant +itunes-disk+           (mk-mp4-atom-type #\d  #\i #\s #\k) "octets: disk number, can be n of N")
(defconstant +itunes-encoder+        (mk-mp4-atom-type #xa9 #\e #\n #\c) "text: who encoded")
(defconstant +itunes-genre+          (mk-mp4-atom-type #\g  #\n #\r #\e) "octets: genre of file")
(defconstant +itunes-genre-x+        (mk-mp4-atom-type #xa9 #\n #\r #\e) "text: yet another genre atom")
(defconstant +itunes-groups+         (mk-mp4-atom-type #xa9 #\g #\r #\p) "text: ???")
(defconstant +itunes-lyrics+         (mk-mp4-atom-type #xa9 #\l #\y #\r) "text: lyrics tag")
(defconstant +itunes-purchased-date+ (mk-mp4-atom-type #\p  #\u #\r #\d) "text: when song was purchased")
(defconstant +itunes-tempo+          (mk-mp4-atom-type #\t  #\m #\p #\o) "octet: tempo of song")
(defconstant +itunes-title+          (mk-mp4-atom-type #xa9 #\n #\a #\m) "text: title of song")
(defconstant +itunes-tool+           (mk-mp4-atom-type #xa9 #\t #\o #\o) "text: what tool encoded this file")
(defconstant +itunes-track+          (mk-mp4-atom-type #xa9 #\t #\r #\k) "octet: track number")
(defconstant +itunes-track-n+        (mk-mp4-atom-type #\t  #\r #\k #\n) "octet: yet another track number")
(defconstant +itunes-writer+         (mk-mp4-atom-type #xa9 #\w #\r #\t) "text: who wrote the song")
(defconstant +itunes-year+           (mk-mp4-atom-type #xa9 #\d #\a #\y) "text: year album was released")
(defconstant +itunes-ilst-data+      (mk-mp4-atom-type #\d #\a #\t #\a) "Carries the actual data under an ilst atom")

(defconstant +m4-ftyp+               (mk-mp4-atom-type #\f #\t #\y #\p) "This should be the first atom type found in file")

(defconstant +audioprop-hdlr+        (mk-mp4-atom-type #\h #\d #\l #\r) "Found under trak.mdia and tells what kind of handler it is")
(defconstant +audioprop-mdhd+        (mk-mp4-atom-type #\m #\d #\h #\d) "Found under trak.mdia and holds data to calculate length of audio")
(defconstant +audioprop-stsd+        (mk-mp4-atom-type #\s #\t #\s #\d) "Container atom: found under trak.mdia.minf.stbl and holds bit-rate, etc")
(defconstant +audioprop-mp4a+        (mk-mp4-atom-type #\m #\p #\4 #\a) "Found under trak.mdia.minf.stbl")
(defconstant +audioprop-esds+        (mk-mp4-atom-type #\e #\s #\d #\s) "Found under trak.mdia.minf.stbl.mp4a")

(defconstant +mp4-atom-ilst+         (mk-mp4-atom-type #\i #\l #\s #\t))
(defconstant +mp4-atom-mdia+         (mk-mp4-atom-type #\m #\d #\i #\a))
(defconstant +mp4-atom-meta+         (mk-mp4-atom-type #\m #\e #\t #\a))
(defconstant +mp4-atom-minf+         (mk-mp4-atom-type #\m #\i #\n #\f))
(defconstant +mp4-atom-moov+         (mk-mp4-atom-type #\m #\o #\o #\v))
(defconstant +mp4-atom-stbl+         (mk-mp4-atom-type #\s #\t #\b #\l))
(defconstant +mp4-atom-trak+         (mk-mp4-atom-type #\t #\r #\a #\k))
(defconstant +mp4-atom-udta+         (mk-mp4-atom-type #\u #\d #\t #\a))


(defclass mp4-atom ()
  ((atom-file-position :accessor atom-file-position :initarg :atom-file-position)
   (atom-size          :accessor atom-size          :initarg :atom-size)
   (atom-type          :accessor atom-type          :initarg :atom-type)
   (atom-children      :accessor atom-children      :initform nil))
  (:documentation "The minimal mp4-atom.  Note: not all atoms have children, but we put them here anyway to make things 'simple'"))

(defmethod addc ((me mp4-atom) value)
  "Want to add children atoms to end of ATOM-CHILDREN to preserve in-file order."
  (declare #.utils:*standard-optimize-settings*)
  (with-slots (atom-children) me
    (if (null atom-children)
        (setf atom-children (list value))
        (nconc atom-children (list value)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Concreate atoms ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defclass atom-skip (mp4-atom) ())
(defmethod initialize-instance :after ((me atom-skip) &key (mp4-file nil) &allow-other-keys)
  "The 'skip' atom.  Used when we want to capture the header of atom, but don't want/need
to read the payload of an atom."
  (declare #.utils:*standard-optimize-settings*)
  (with-slots (atom-size atom-type) me
    (stream-seek mp4-file (- atom-size 8) :current)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ILST ATOMS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defclass atom-ilst (mp4-atom) ())

(defmethod initialize-instance :after ((me atom-ilst) &key (mp4-file nil) &allow-other-keys)
  "Construct an ilst atom.  ILST atoms are containers that hold data elements related to tagging.
Loop through this container and construct constituent atoms"
  (declare #.utils:*standard-optimize-settings*)
  (log5:with-context "atom-ilst-initializer"
    (with-slots (atom-size atom-type atom-children) me
      (log-mp4-atom "atom-ilst-init: found ilst atom <~a> @ ~:d, looping for ~:d bytes"
                    (as-string atom-type) (stream-here mp4-file) (- atom-size 8))

      (let ((end (+ (stream-here mp4-file) (- atom-size 8))))
        (loop for current = (stream-here mp4-file) then (stream-here mp4-file)
            while (< current end) do
              (log-mp4-atom "at ~:d:~:d~%" current end)
              (addc me (make-mp4-atom mp4-file atom-type)))))))

(defclass atom-©alb (atom-ilst) ())
(defclass atom-aART (atom-ilst) ())
(defclass atom-©art (atom-ilst) ())
(defclass atom-©cmt (atom-ilst) ())
(defclass atom-cpil (atom-ilst) ())
(defclass atom-©com (atom-ilst) ())
(defclass atom-cprt (atom-ilst) ())
(defclass atom-covr (atom-ilst) ())
(defclass atom-disk (atom-ilst) ())
(defclass atom-©enc (atom-ilst) ())
(defclass atom-gnre (atom-ilst) ())
(defclass atom-©nre (atom-ilst) ())
(defclass atom-©grp (atom-ilst) ())
(defclass atom-©lyr (atom-ilst) ())
(defclass atom-purd (atom-ilst) ())
(defclass atom-tmpo (atom-ilst) ())
(defclass atom-©nam (atom-ilst) ())
(defclass atom-©too (atom-ilst) ())
(defclass atom-©trk (atom-ilst) ())
(defclass atom-trkn (atom-ilst) ())
(defclass atom-©wrt (atom-ilst) ())
(defclass atom-©day (atom-ilst) ())

(defclass atom-data (mp4-atom)
   ((atom-version      :accessor atom-version     :initarg :atom-version)
    (atom-flags        :accessor atom-flags       :initarg :atom-flags)
    (atom-value        :accessor atom-value       :initarg :atom-value)
    (atom-parent-type  :accessor atom-parent-type :initarg :atom-parent-type :initform nil))
   (:documentation "Represents the 'data' portion of ilst data atom"))

 (defmethod initialize-instance :after ((me atom-data) &key mp4-file &allow-other-keys)
  (declare #.utils:*standard-optimize-settings*)
   (log5:with-context "atom-data-init"
     (with-slots (atom-size atom-type atom-version atom-flags atom-value atom-parent-type) me
       (setf atom-version (stream-read-u8 mp4-file)
             atom-flags (stream-read-u24 mp4-file))
       (assert (= 0 (stream-read-u32 mp4-file)) () "a data atom lacks the required null field") ; XXX is this true?
       (log-mp4-atom "atom-data-init: size = ~:d, name = ~a, version = ~d, flags = ~x" atom-size (as-string atom-type) atom-version atom-flags)
       (setf atom-value (decode-ilst-data-atom atom-type me atom-parent-type mp4-file))
       (log-mp4-atom "atom-data-init: made an ilst atom-data: ~a" (vpprint me nil)))))

;;; the ILST atom decoders.  First, a lot of the decoders do the same thing, so we define a macros
;;; and use those for the relevants atoms.
(defgeneric decode-ilst-data-atom (type atom atom-parent-type mp4-file))

;;; Quicktime spec says strings are stored as UTF-8...
(defmacro simple-text-decode (type)
  `(defmethod decode-ilst-data-atom ((type (eql +itunes-ilst-data+)) atom (atom-parent-type (eql ,type)) mp4-file)
     (stream-read-utf-8-string-with-len mp4-file (- (atom-size atom) 16))))

(simple-text-decode +itunes-album+)
(simple-text-decode +itunes-album-artist+)
(simple-text-decode +itunes-artist+)
(simple-text-decode +itunes-comment+)
(simple-text-decode +itunes-composer+)
(simple-text-decode +itunes-copyright+)
(simple-text-decode +itunes-year+)
(simple-text-decode +itunes-encoder+)
(simple-text-decode +itunes-groups+)
(simple-text-decode +itunes-genre-x+)
(simple-text-decode +itunes-lyrics+)
(simple-text-decode +itunes-purchased-date+)
(simple-text-decode +itunes-title+)
(simple-text-decode +itunes-tool+)
(simple-text-decode +itunes-writer+)

;;; for reasons I'm not clear on, there may or may not be extra bytes after the data in these atoms
;;; hence, the seek at the end to get us by any unread bytes.
(defmacro simple-a-b-decode (type)
  `(defmethod decode-ilst-data-atom ((type (eql +itunes-ilst-data+)) atom (atom-parent-type (eql ,type)) mp4-file)
     (let ((tmp (stream-read-u16 mp4-file)))
       (declare (ignore tmp)))
       ;(format t "ilist decode, parent = ~a: ~x~%" (as-string atom-parent-type) tmp))
     (let ((a) (b))
       (setf a (stream-read-u16 mp4-file)
             b (stream-read-u16 mp4-file))
       (stream-seek mp4-file (- (atom-size atom) 16 6) :current) ; seek to end of atom: 16 == header; 4 is a, b, skip read above
       (list a b))))

(simple-a-b-decode +itunes-track+)
(simple-a-b-decode +itunes-track-n+)
(simple-a-b-decode +itunes-disk+)

(defmacro simple-u16-decode (type)
  `(defmethod decode-ilst-data-atom ((type (eql +itunes-ilst-data+)) atom (atom-parent-type (eql ,type)) mp4-file)
     (declare (ignore atom))
     (stream-read-u16 mp4-file)))

(simple-u16-decode +itunes-tempo+)
(simple-u16-decode +itunes-genre+)

(defmethod decode-ilst-data-atom ((type (eql +itunes-ilst-data+)) atom (atom-parent-type (eql +itunes-compilation+)) mp4-file)
  (declare (ignore atom))
  (stream-read-u8 mp4-file))

(defmethod decode-ilst-data-atom ((type (eql +itunes-ilst-data+)) atom (atom-parent-type (eql +itunes-cover-art+)) mp4-file)
  (stream-read-sequence mp4-file (- (atom-size atom) 16)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; AUDIO PROPERTY ATOMS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; the pure container classes we need recurse into
(defclass atom-trak (mp4-atom) ())
(defmethod initialize-instance :after ((me atom-trak) &key (mp4-file nil) &allow-other-keys)
  (read-container-atoms mp4-file me))
(defclass atom-minf (mp4-atom) ())
(defmethod initialize-instance :after ((me atom-minf) &key (mp4-file nil) &allow-other-keys)
  (read-container-atoms mp4-file me))
(defclass atom-moov (mp4-atom) ())
(defmethod initialize-instance :after ((me atom-moov) &key (mp4-file nil) &allow-other-keys)
  (read-container-atoms mp4-file me))
(defclass atom-udta (mp4-atom) ())
(defmethod initialize-instance :after ((me atom-udta) &key (mp4-file nil) &allow-other-keys)
  (read-container-atoms mp4-file me))
(defclass atom-mdia (mp4-atom) ())
(defmethod initialize-instance :after ((me atom-mdia) &key (mp4-file nil) &allow-other-keys)
  (read-container-atoms mp4-file me))
(defclass atom-stbl (mp4-atom) ())
(defmethod initialize-instance :after ((me atom-stbl) &key (mp4-file nil) &allow-other-keys)
  (read-container-atoms mp4-file me))

(defclass atom-hdlr (mp4-atom)
  ((version :accessor version) ; 1 byte
   (flags   :accessor flags)   ; 3 bytes
   (qtype   :accessor qtype)   ; 4 bytes
   (mtype   :accessor mtype)   ; 4 bytes
   (resv    :accessor resv)    ; 4 bytes
   (rflag   :accessor rflag)   ; 4 bytes
   (rmask   :accessor rmask)   ; 4 bytes
   (mhdlr   :accessor mhdlr))) ; null-terminated string (but we're reading it as octets)

(defmethod initialize-instance :after ((me atom-hdlr) &key (mp4-file nil) &allow-other-keys)
  (with-slots (version flags qtype mtype resv rflag rmask mhdlr atom-size) me
    (setf version  (stream-read-u8 mp4-file)
          flags    (stream-read-u24 mp4-file)
          qtype    (stream-read-u32 mp4-file)
          mtype    (stream-read-u32 mp4-file)
          resv     (stream-read-u32 mp4-file)
          rflag    (stream-read-u32 mp4-file)
          rmask    (stream-read-u32 mp4-file)
          mhdlr    (stream-read-sequence mp4-file (- atom-size 32))))) ; 32 is 8-bytes of header plus fields above

(defclass atom-mdhd (mp4-atom)
  ((version  :accessor version)
   (flags    :accessor flags)
   (c-time   :accessor c-time)
   (m-time   :accessor m-time)
   (scale    :accessor scale)
   (duration :accessor duration)
   (lang     :accessor lang)
   (quality  :accessor quality)))

(defmethod initialize-instance :after ((me atom-mdhd) &key (mp4-file nil) &allow-other-keys)
  (declare #.utils:*standard-optimize-settings*)
  (with-slots (version flags c-time m-time scale duration lang quality) me
    (setf version  (stream-read-u8 mp4-file)
          flags    (stream-read-u24 mp4-file)
          c-time   (stream-read-u32 mp4-file)
          m-time   (stream-read-u32 mp4-file)
          scale    (stream-read-u32 mp4-file)
          duration (if (= 0 version) (stream-read-u32 mp4-file) (stream-read-u64 mp4-file))
          lang     (stream-read-u16 mp4-file)
          quality  (stream-read-u16 mp4-file))))

(defclass atom-esds (mp4-atom)
  ((version      :accessor version)       ; 1 byte
   (flags        :accessor flags)         ; 3 bytes
   (esid         :accessor esid)          ; 2 bytes
   (s-priority   :accessor s-priority)    ; 1 byte
   (obj-id       :accessor obj-id)        ; 1 byte
   (s-type       :accessor s-type)        ; 1 byte (1 bit up-stream, 1-but reservered, 6-bits stream type
   (buf-size     :accessor buf-size)      ; 3 bytes
   (max-bit-rate :accessor max-bit-rate)  ; 4 bytes
   (avg-bit-rate :accessor avg-bit-rate)) ; 4 bytes
  (:documentation "XXX-partial definition for Elementary Stream Descriptors (ESDs)"))

;;; 3 bytes extended descriptor type tag string = 3 * 8-bit hex value
;;; types are Start = 0x80 ; End = 0xFE
;;; then, one byte of length
;;; Note: start types are optional
(defun read-descriptor-len (instream)
  "Get the ES descriptor's length."
  (declare #.utils:*standard-optimize-settings*)
  (let* ((tmp (stream-read-u8 instream))
         (len (logand tmp #x7f)))
    (declare (type (unsigned-byte 8) tmp))
    (while (not (zerop (logand #x80 tmp)))
      (setf tmp (stream-read-u8 instream)
            len (logior (ash len 7) (logand tmp #x7f))))
    len))

;;; one-byte descriptor tags
(defconstant +mp4-odescrtag+               #x01)
(defconstant +mp4-iodescrtag+              #x02)
(defconstant +mp4-esdescrtag+              #x03)
(defconstant +mp4-decconfigdescrtag+       #x04)
(defconstant +mp4-decspecificdescrtag+     #x05)
(defconstant +mp4-slconfigdescrtag+        #x06)
(defconstant +mp4-contentiddescrtag+       #x07)
(defconstant +mp4-supplcontentiddescrtag+  #x08)
(defconstant +mp4-ipiptrdescrtag+          #x09)
(defconstant +mp4-ipmpptrdescrtag+         #x0a)
(defconstant +mp4-ipmpdescrtag+            #x0b)
(defconstant +mp4-registrationdescrtag+    #x0d)
(defconstant +mp4-esidincdescrtag+         #x0e)
(defconstant +mp4-esidrefdescrtag+         #x0f)
(defconstant +mp4-fileiodescrtag+          #x10)
(defconstant +mp4-fileodescrtag+           #x11)
(defconstant +mp4-extprofileleveldescrtag+ #x13)
(defconstant +mp4-extdescrtagsstart+       #x80)
(defconstant +mp4-extdescrtagsend+         #xfe)

(defmethod initialize-instance :after ((me atom-esds) &key (mp4-file nil) &allow-other-keys)
  (declare #.utils:*standard-optimize-settings*)
  (with-slots (version flags esid s-priority obj-id s-type buf-size max-bit-rate avg-bit-rate) me
    (setf version (stream-read-u8 mp4-file)
          flags (stream-read-u24 mp4-file))
    (assert (= +MP4-ESDescrTag+ (stream-read-u8 mp4-file)) () "Expected description tag of ESDescrTag")
    (let* ((len (read-descriptor-len mp4-file))
           (end-of-atom (+ (stream-here mp4-file) len)))
      (setf esid (stream-read-u16 mp4-file)
            s-priority (stream-read-u8 mp4-file))
      (assert (= +MP4-DecConfigDescrTag+ (stream-read-u8 mp4-file)) () "Expected tag type of DecConfigDescrTag")
      (setf len          (read-descriptor-len mp4-file)
            obj-id       (stream-read-u8 mp4-file)
            s-type       (stream-read-u8 mp4-file)
            buf-size     (stream-read-u24 mp4-file)
            max-bit-rate (stream-read-u32 mp4-file)
            avg-bit-rate (stream-read-u32 mp4-file))

      ;; Should do checking here and/or read rest of atom,
      ;; but for now, we have what we want, so just seek to end of atom
      (stream-seek mp4-file end-of-atom :start))))

(defclass atom-stsd (mp4-atom)
  ((flags       :accessor flags)
   (version     :accessor version)
   (num-entries :accessor num-entries)))

(defmethod initialize-instance :after ((me atom-stsd) &key (mp4-file nil) &allow-other-keys)
  (declare #.utils:*standard-optimize-settings*)
  (log5:with-context "atom-stsd"
    (with-slots (flags version num-entries) me
      (setf version (stream-read-u8 mp4-file)
            flags   (stream-read-u24 mp4-file)
            num-entries (stream-read-u32 mp4-file))
      (log-mp4-atom "atom-stsd: version = ~d, flags = ~x, num-fields = ~d" version flags num-entries))))

(defclass atom-mp4a (mp4-atom)
  ((reserved    :accessor reserved)    ; 6 bytes
   (d-ref-idx   :accessor d-ref-idx)   ; 2 bytes
   (version     :accessor version)     ; 2 bytes
   (revision    :accessor revision)    ; 2 bytes
   (vendor      :accessor vendor)      ; 4 bytes
   (num-chans   :accessor num-chans)   ; 2 bytes
   (samp-size   :accessor samp-size)   ; 2 bytes
   (comp-id     :accessor comp-id)     ; 2 bytes
   (packet-size :accessor packet-size) ; 2 bytes
   (samp-rate   :accessor samp-rate))) ; 4 bytes

(defmethod initialize-instance :after ((me atom-mp4a) &key (mp4-file nil) &allow-other-keys)
  (declare #.utils:*standard-optimize-settings*)
  (log5:with-context "atom-mp4a"
    (with-slots (reserved d-ref-idx version revision vendor num-chans samp-size comp-id packet-size samp-rate) me
      (setf reserved    (stream-read-sequence mp4-file 6)
            d-ref-idx   (stream-read-u16 mp4-file)
            version     (stream-read-u16 mp4-file)
            revision    (stream-read-u16 mp4-file)
            vendor      (stream-read-u32 mp4-file)
            num-chans   (stream-read-u16 mp4-file)
            samp-size   (stream-read-u16 mp4-file)
            comp-id     (stream-read-u16 mp4-file)
            packet-size (stream-read-u16 mp4-file)
            samp-rate   (stream-read-u32 mp4-file)) ; fixed 16.16 floating point number

      (read-container-atoms mp4-file me))))

(defun read-container-atoms (mp4-file parent-atom)
  "Loop through a container atom and add it's children to it"
  (declare #.utils:*standard-optimize-settings*)
  (with-slots (atom-children atom-file-position atom-of-interest atom-size atom-type atom-decoded) parent-atom
    (let ((end (+ atom-file-position atom-size)))
      (loop for current = (stream-here mp4-file) then (stream-here mp4-file)
            while (< current end) do
              (let ((child (make-mp4-atom mp4-file atom-type)))
                (log-mp4-atom "read-container-atoms: adding new child ~a" (vpprint child nil))
                (addc parent-atom child))))))

(defclass atom-meta (mp4-atom)
  ((version  :accessor version)
   (flags    :accessor flags)))
(defmethod initialize-instance :after ((me atom-meta) &key (mp4-file nil) &allow-other-keys)
  (declare #.utils:*standard-optimize-settings*)
   (with-slots (version flags) me
     (setf version  (stream-read-u8 mp4-file)
           flags    (stream-read-u24 mp4-file))
     (read-container-atoms mp4-file me)))

(defun find-atom-class (id)
  "Search by concatenating 'atom-' with ID and look for that symbol in this package"
  (declare #.utils:*standard-optimize-settings*)
  (log5:with-context "find-atom-class"
    (log-mp4-atom "find-atom-class: looking for class <~a>" (as-string id))
    (let ((found-class-symbol (find-symbol (mk-atom-class-name id) :MP4-ATOM))
          (found-class))

      ;; if we found the class name, return the class (to be used for MAKE-INSTANCE)
      (when found-class-symbol
        (setf found-class (find-class found-class-symbol))
        (log-mp4-atom "find-atom-class: found class: ~a" found-class)
        (return-from find-atom-class (find-class found-class-symbol))))

    ;; didn't find a class, so return ATOM-SKIP class
    (log-mp4-atom "find-atom-class: class not found")
    'atom-skip))
(utils:memoize 'find-atom-class)

(defun make-mp4-atom (mp4-file &optional parent-type)
  "Get current file position, read in size/type, then construct the correct atom."
  (declare #.utils:*standard-optimize-settings*)
  (log5:with-context "make-mp4-atom"
    (let* ((pos (stream-here mp4-file))
           (siz (stream-read-u32 mp4-file))
           (typ (stream-read-u32 mp4-file))
           (atom))
      (declare (type integer pos siz typ))

      (log-mp4-atom "make-mp4-atom: @ pos = ~:d of size = ~:d and type = ~a" pos siz (as-string typ))

      (when (= 0 siz)
        (error "trying to make an atom ~a with size of 0 at offset ~:d in file ~a"
               (as-string typ) pos (stream-filename mp4-file)))

      (setf atom (make-instance (find-atom-class typ) :atom-size siz :atom-type typ :atom-file-position pos :mp4-file mp4-file :atom-parent-type parent-type))
      (log-mp4-atom "make-mp4-atom: made ~a" (vpprint atom nil))
      atom)))

(defmethod vpprint ((me mp4-atom) stream)
  (format stream "~a"
          (with-output-to-string (s)
            (with-slots (atom-children atom-file-position atom-size atom-type) me
              (format s "ATOM: type: <~a> @ ~:d of size ~:d and child count of ~d"
                      (as-string atom-type) atom-file-position atom-size (length atom-children)))
            (if (typep me 'atom-data)
                (with-slots (atom-version atom-flags atom-value atom-type atom-parent-type) me
                  (format s " having ilst fields: atom-parent-type = ~a, verison = ~d, flags = ~x, data = ~x"
                          (as-string atom-parent-type) atom-version atom-flags
                          (if (typep atom-value 'array) (printable-array atom-value) atom-value)))))))

(defun is-valid-m4-file (mp4-file)
  "Make sure this is an MP4 file.  Quick check: is first atom (at file-offset 4) == FSTYP?
Written in this fashion so as to be 'crash-proof' when passed an arbitrary file."
  (declare #.utils:*standard-optimize-settings*)
  (let ((valid)
        (size)
        (header))
    (when (> (stream-size mp4-file) 8)
      (unwind-protect
           (handler-case
               (progn
                 (stream-seek mp4-file 0 :start)
                 (setf size   (stream-read-u32 mp4-file)
                       header (stream-read-u32 mp4-file)
                       valid  (and (<= size (stream-size mp4-file))
                                   (= header +m4-ftyp+))))
             (condition (c)
               (utils:warn-user "File:~a~%is-valid-mp4-file got condition ~a" (stream-filename mp4-file) c)))

        (stream-seek mp4-file 0 :start)))
    valid))

(defmethod find-mp4-atoms ((mp4-file mp4-file-stream))
  "Given a valid MP4 file MP4-FILE, look for the 'right' atoms and return them."
  (declare #.utils:*standard-optimize-settings*)
  (log5:with-context "find-mp4-atoms"

    (log-mp4-atom "find-mp4-atoms: ~a, before read-file loop, file-position = ~:d, end = ~:d"
                  (stream-filename mp4-file) (stream-here mp4-file) (stream-size mp4-file))

    (let ((atoms)
          (end (stream-size mp4-file)))
      (loop for current = (stream-here mp4-file) then (stream-here mp4-file)
            while (< current end) do
              (let ((new-atom (make-mp4-atom mp4-file)))
                (when new-atom
                  (push new-atom atoms))))
      (setf (mp4-atoms mp4-file) (nreverse atoms))) ; preserve in-file-order

    (log-mp4-atom "find-mp4-atoms: returning list of size ~d" (length (mp4-atoms mp4-file)))))

(defmethod map-mp4-atom ((atoms list) &key (func nil) (depth nil))
  "Given a list of atoms, call map-mp4-atom for each one"
  (declare #.utils:*standard-optimize-settings*)
  (log5:with-context "map-mp4-atom"
    (dolist (a atoms)
      (map-mp4-atom a :func func :depth depth))))

(defmethod map-mp4-atom ((me mp4-atom) &key (func nil) (depth nil))
  "Traverse all atoms under a given atom"
  (declare #.utils:*standard-optimize-settings*)
  (log5:with-context "map-mp4-atom(single)"
    (labels ((_indented-atom (atom depth)
               (format t "~vt~a~%"  (if (null depth) 0 depth) (vpprint atom nil))))
      (with-slots (atom-type atom-children) me
        (log-mp4-atom "map-mp4-atom: begining traversal with ~a, I have ~d children" (as-string atom-type) (length atom-children))
        (when (null func)
          (setf func #'_indented-atom))
        (funcall func me depth)
        (map-mp4-atom atom-children :func func :depth (if (null depth) nil (+ 1 depth)))))))

(defmethod traverse ((me mp4-atom) path)
  (declare #.utils:*standard-optimize-settings*)
  (traverse (atom-children me) path))

(defmethod traverse ((me list) path)
  "Used in finding nested atoms. Search MP4-ATOMS and if we find a match with first of path,
call traverse atom (unless length of path == 1, in which case, we've found our match)"
  (declare #.utils:*standard-optimize-settings*)
  (log5:with-context "traverse"
    (log-mp4-atom "traverse: entering with ~a ~a" me path)
    (dolist (sibling me)
      (with-slots (atom-type atom-children) sibling
        (log-mp4-atom "traverse: comparing ~a to ~a" (as-string atom-type) (as-string (first path)))
        (when (= atom-type (first path))
          (cond
            ((= 1 (length path))
             (log-mp4-atom "traverse: matched: ~a" sibling)
             (return-from traverse sibling))
            (t
             (log-mp4-atom "traverse: path matches, recursing")
             (let ((found (traverse atom-children (rest path))))
               (if found (return-from traverse found))))))))
    (log-mp4-atom "traverse: ~a not found" path)
    nil))

(defmethod tag-get-value (atoms node)
  "Helper function to extract text from ILST atom's data atom"
  (declare #.utils:*standard-optimize-settings*)
  (aif (traverse atoms
                 (list +mp4-atom-moov+ +mp4-atom-udta+ +mp4-atom-meta+ +mp4-atom-ilst+ node +itunes-ilst-data+))
       (atom-value it)
       nil))

(defconstant +path-to-ilst+ (list +mp4-atom-moov+ +mp4-atom-udta+ +mp4-atom-meta+ +mp4-atom-ilst+))

(defun mp4-show-raw-tag-atoms (mp4-file-stream out-stream)
  (declare #.utils:*standard-optimize-settings*)
  (map-mp4-atom (traverse (mp4-atoms mp4-file-stream) +path-to-ilst+)
                :depth 0
                :func (lambda (atom depth)
                        (when (= (atom-type atom) +itunes-ilst-data+)
                          (format out-stream "~vt~a~%" depth (vpprint atom nil))))))

(defun get-audio-properties-atoms (mp4-file)
  "Get the audio property atoms from MP4-FILE"
  (declare #.utils:*standard-optimize-settings*)
  (let* ((mdia       (traverse (mp4-atoms mp4-file) (list +mp4-atom-moov+ +mp4-atom-trak+ +mp4-atom-mdia+)))
         (mdhd       (traverse mdia (list +audioprop-mdhd+)))
         (audioprop1 (traverse mdia (list +mp4-atom-minf+ +mp4-atom-stbl+ +audioprop-mp4a+)))
         (audioprop2 (traverse audioprop1 (list +audioprop-esds+))))
    (if (and mdhd audioprop1 audioprop2)
        (values mdhd audioprop1 audioprop2)
        nil)))

(defclass audio-info ()
  ((seconds         :accessor seconds         :initform nil)
   (channels        :accessor channels        :initform nil)
   (bits-per-sample :accessor bits-per-sample :initform nil)
   (sample-rate     :accessor sample-rate     :initform nil)
   (max-bit-rate    :accessor max-bit-rate    :initform nil)
   (avg-bit-rate    :accessor avg-bit-rate    :initform nil))
  (:documentation "Holds extracted audio information about an MP4 file."))

(defmethod vpprint ((me audio-info) stream)
  (with-slots (seconds channels bits-per-sample sample-rate max-bit-rate avg-bit-rate) me
    (format stream "sample rate: ~:d Hz, # channels: ~d, bits-per-sample: ~:d, max bit-rate: ~:d Kbps, avg bit-rate: ~:d Kbps, duration: ~:d:~2,'0d"
            (if sample-rate sample-rate 0)
            (if channels channels 0)
            (if bits-per-sample bits-per-sample 0)
            (if max-bit-rate (round (/ max-bit-rate 1000)) 0)
            (if avg-bit-rate (round (/ avg-bit-rate 1000)) 0)
            (if seconds (floor (/ seconds 60)) 0)
            (if seconds (round (mod seconds 60)) 0))))

(defun get-mp4-audio-info (mp4-file)
  "MP4A audio info is held in under the trak.mdia.mdhd/trak.mdia.minf.stbl/trak.mdia.minf.stbl.mp4a atoms."
  (declare #.utils:*standard-optimize-settings*)
  (let ((info (make-instance 'audio-info)))
    (multiple-value-bind (mdhd mp4a esds) (get-audio-properties-atoms mp4-file)
      (with-slots (seconds channels bits-per-sample sample-rate max-bit-rate avg-bit-rate) info
        (when mdhd
          (setf seconds (/ (float (duration mdhd)) (float (scale mdhd)))))
        (when mp4a
          (setf channels (num-chans mp4a)
                bits-per-sample (samp-size mp4a))
          (let* ((upper (ash (samp-rate mp4a) -16))
                 (lower (logand (samp-rate mp4a) #xffff)))
            (setf sample-rate (+ (float upper) (/ (float lower) 1000))))
        (when esds
          (setf avg-bit-rate (avg-bit-rate esds)
                max-bit-rate (max-bit-rate esds))))))
    info))
