;;; -*- Mode: Lisp;  show-trailing-whitespace: t; Base: 10; indent-tabs: nil; Syntax: ANSI-Common-Lisp; Package: MP4-ATOM; -*-
;;; Copyright (c) 2013, Mark VandenBrink. All rights reserved.
(in-package #:mp4-atom)

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
(ie (as-int \"hdlr\" == +audioprop-hdlr+))"
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
  "The inverse of as-int: given an integer, return the
string representation"
  (declare #.utils:*standard-optimize-settings*)
  (declare (fixnum atom-type))
  (with-output-to-string (s nil)
    (write-char (code-char (ldb (byte 8 24) atom-type)) s)
    (write-char (code-char (ldb (byte 8 16) atom-type)) s)
    (write-char (code-char (ldb (byte 8 8)  atom-type)) s)
    (write-char (code-char (ldb (byte 8 0)  atom-type)) s)))
(utils:memoize 'as-string)

(defun mk-atom-class-name (name)
  "Create an atom class name by concatenating ATOM- with NAME"
(declare #.utils:*standard-optimize-settings*)
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

(defconstant +root+                  (mk-mp4-atom-type #\R #\O #\O #\T)  "fake root for tree")
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
(defconstant +itunes-genre-x+        (mk-mp4-atom-type #xa9 #\g #\e #\n) "text: yet another genre atom")
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
(defconstant +itunes-ilst-data+      (mk-mp4-atom-type #\d #\a #\t #\a)  "carries the actual data under an ilst atom")

(defconstant +m4-ftyp+               (mk-mp4-atom-type #\f #\t #\y #\p) "This should be the first atom type found in file")

(defconstant +mp4-atom-hdlr+         (mk-mp4-atom-type #\h #\d #\l #\r) "Found under trak.mdia and tells what kind of handler it is")

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

(defvar *in-progress* nil "the node currently being worked upon")
(defvar *tree*        nil "the root of the atom tree")

(defclass mp4-atom ()
  ((atom-file-pos :accessor atom-file-pos :initarg :atom-file-pos :initform nil)
   (atom-size     :accessor atom-size     :initarg :atom-size     :initform nil)
   (atom-type     :accessor atom-type     :initarg :atom-type     :initform nil))
  (:documentation "The minimal mp4-atom."))

(defmethod initialize-instance :around ((me mp4-atom) &key &allow-other-keys)
  (declare #.utils:*standard-optimize-settings*)
  (let* ((old *in-progress*)
         (*in-progress* (tree:make-node me)))
    (if old
        (tree:add-child old *in-progress*)
        (setf *tree* *in-progress*))
    (call-next-method)))

(defmacro with-mp4-atom-slots ((instance) &body body)
  `(with-slots (atom-file-pos atom-size atom-type) ,instance
     ,@body))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Concrete atoms ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defclass atom-skip (mp4-atom) ())

(defmethod initialize-instance :after ((me atom-skip) &key mp4-file &allow-other-keys)
  "The 'skip' atom.  Used when we want to capture the header of atom, but don't want/need
to read the payload of an atom."
  (declare #.utils:*standard-optimize-settings*)
  (with-mp4-atom-slots (me)
    (stream-seek mp4-file (- atom-size 8) :current)))

(defclass mp4-container-atom (mp4-atom) ())

(defmethod initialize-instance :after ((me mp4-container-atom) &key mp4-file &allow-other-keys)
  (declare #.utils:*standard-optimize-settings*)
  (with-mp4-atom-slots (me)
    (loop for end = (+ atom-file-pos atom-size)
          for current = (stream-here mp4-file) then (stream-here mp4-file)
          while (< current end) do
            (make-mp4-atom mp4-file me))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ILST ATOMS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defclass atom-ilst (mp4-container-atom) ())

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
(defclass atom-©gen (atom-ilst) ())
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
  ((atom-version :accessor atom-version :initarg :atom-version :initform nil)
   (atom-flags   :accessor atom-flags   :initarg :atom-flags   :initform nil)
   (atom-value   :accessor atom-value   :initarg :atom-value   :initform nil))
  (:documentation "Represents the 'data' portion of ilst data atom"))

(defmethod initialize-instance :after ((me atom-data) &key mp4-file parent &allow-other-keys)
  (declare #.utils:*standard-optimize-settings*)
  (with-slots (atom-size atom-type atom-version atom-flags atom-value) me
    (setf atom-version (stream-read-u8 mp4-file)
          atom-flags   (stream-read-u24 mp4-file))
    (assert (= 0 (stream-read-u32 mp4-file)) ()
            "a data atom lacks the required null field")
    (setf atom-value
          (cond ((member (atom-type parent)
                         (list +itunes-album+ +itunes-album-artist+ +itunes-artist+
                               +itunes-comment+ +itunes-composer+ +itunes-copyright+
                               +itunes-year+ +itunes-encoder+ +itunes-groups+
                               +itunes-genre-x+ +itunes-lyrics+ +itunes-purchased-date+
                               +itunes-title+ +itunes-tool+ +itunes-writer+))
                 (stream-read-utf-8-string-with-len mp4-file (- (atom-size me) 16)))

                ((member (atom-type parent)
                         (list +itunes-track+
                               +itunes-track-n+
                               +itunes-disk+))
                 (stream-read-u16 mp4-file) ; throw away
                 (let* ((a (stream-read-u16 mp4-file))
                        (b (stream-read-u16 mp4-file)))
                   (stream-seek mp4-file (- (atom-size me) 16 6) :current)
                   (list a b)))

                ((member (atom-type parent)
                         (list +itunes-tempo+ +itunes-genre+))
                 (stream-read-u16 mp4-file))

                ((= (atom-type parent) +itunes-compilation+)
                 (stream-read-u8 mp4-file))

                ((= (atom-type parent) +itunes-cover-art+)
                 (stream-read-sequence mp4-file (- (atom-size me) 16)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; AUDIO PROPERTY ATOMS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defclass atom-trak (mp4-container-atom) ())
(defclass atom-minf (mp4-container-atom) ())
(defclass atom-moov (mp4-container-atom) ())
(defclass atom-udta (mp4-container-atom) ())
(defclass atom-mdia (mp4-container-atom) ())
(defclass atom-stbl (mp4-container-atom) ())

(defclass atom-hdlr (mp4-atom)
  ((version :accessor version) ; 1 byte
   (flags   :accessor flags)   ; 3 bytes
   (qtype   :accessor qtype)   ; 4 bytes
   (mtype   :accessor mtype)   ; 4 bytes
   (resv    :accessor resv)    ; 4 bytes
   (rflag   :accessor rflag)   ; 4 bytes
   (rmask   :accessor rmask)   ; 4 bytes
   (mhdlr   :accessor mhdlr))) ; null-terminated string (but we're reading it as octets)

(defmethod initialize-instance :after ((me atom-hdlr) &key mp4-file &allow-other-keys)
  (declare #.utils:*standard-optimize-settings*)
  (with-slots (version flags qtype mtype resv rflag rmask mhdlr atom-size) me
    (setf version  (stream-read-u8 mp4-file)
          flags    (stream-read-u24 mp4-file)
          qtype    (stream-read-u32 mp4-file)
          mtype    (stream-read-u32 mp4-file)
          resv     (stream-read-u32 mp4-file)
          rflag    (stream-read-u32 mp4-file)
          rmask    (stream-read-u32 mp4-file)
          mhdlr    (stream-read-sequence mp4-file (- atom-size 32))) ; 32 is 8-bytes of header plus fields above
    ))

(defclass atom-mdhd (mp4-atom)
  ((version  :accessor version)
   (flags    :accessor flags)
   (c-time   :accessor c-time)
   (m-time   :accessor m-time)
   (scale    :accessor scale)
   (duration :accessor duration)
   (lang     :accessor lang)
   (quality  :accessor quality)))

(defmethod initialize-instance :after ((me atom-mdhd) &key mp4-file &allow-other-keys)
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

(defmethod initialize-instance :after ((me atom-esds) &key mp4-file &allow-other-keys)
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

(defmethod initialize-instance :after ((me atom-stsd) &key mp4-file &allow-other-keys)
  (declare #.utils:*standard-optimize-settings*)
  (with-slots (flags version num-entries) me
    (setf version     (stream-read-u8 mp4-file)
          flags       (stream-read-u24 mp4-file)
          num-entries (stream-read-u32 mp4-file))))

(defclass atom-mp4a (mp4-container-atom)
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

(defmethod initialize-instance :around ((me atom-mp4a) &key mp4-file &allow-other-keys)
  "Note: this MUST be an AROUND method so that the atom's data can be read in before
reading the container atoms"
  (declare #.utils:*standard-optimize-settings*)
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
          samp-rate   (stream-read-u32 mp4-file))) ; fixed 16.16 floating point number
  (call-next-method))

(defclass atom-meta (mp4-container-atom)
  ((version  :accessor version)
   (flags    :accessor flags)))

(defmethod initialize-instance :around ((me atom-meta) &key mp4-file &allow-other-keys)
  (declare #.utils:*standard-optimize-settings*)
   (with-slots (version flags) me
     (setf version (stream-read-u8 mp4-file)
           flags   (stream-read-u24 mp4-file)))
  (call-next-method))

(defun find-atom-class (id)
  "Search by concatenating 'atom-' with ID and look for that symbol in this package"
  (declare #.utils:*standard-optimize-settings*)
  (let ((found-class-symbol (find-symbol (mk-atom-class-name id) :MP4-ATOM)))

    ;; if we found the class name, return the class (to be used for MAKE-INSTANCE)
    (when found-class-symbol
      (return-from find-atom-class (find-class found-class-symbol)))

    ;; didn't find a class, so return ATOM-SKIP class
    'atom-skip))

(utils:memoize 'find-atom-class)

(defun make-mp4-atom (mp4-file parent)
  "Get current file position, read in size/type, then construct the correct atom."
  (declare #.utils:*standard-optimize-settings*)
  (let* ((pos (stream-here mp4-file))
         (siz (stream-read-u32 mp4-file))
         (typ (stream-read-u32 mp4-file))
         (atom))
    (declare (type fixnum pos siz typ))

    (when (= 0 siz)
      (error "trying to make an atom ~a with size of 0 at offset ~:d in file ~a"
             (as-string typ) pos (stream-filename mp4-file)))

    (setf atom (make-instance (find-atom-class typ) :atom-size siz
                                                    :atom-type typ
                                                    :atom-file-pos pos
                                                    :parent parent
                                                    :mp4-file mp4-file))
    atom))

(defmethod vpprint ((me mp4-atom) stream)
  (declare #.utils:*standard-optimize-settings*)
  (format stream "~a"
          (with-output-to-string (s)
            (with-mp4-atom-slots (me)
              (format s "ATOM: type: <~a> @ ~:d of size ~:d"
                      (as-string atom-type) atom-file-pos atom-size))
            (if (typep me 'atom-data)
                (with-slots (atom-version atom-flags atom-value atom-type) me
                  (format s " having ilst fields:verison = ~d, flags = ~x, data = ~x"
                          atom-version atom-flags
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
  (stream-seek mp4-file 0 :start)
  (setf *in-progress* nil)

  ;; Construct our fake "root" for our tree, which recursively reads all atoms
  (tree:make-node (make-instance 'mp4-container-atom
                                 :atom-type +root+
                                 :atom-file-pos 0
                                 :atom-size (stream-size mp4-file)
                                 :mp4-file mp4-file))
    (setf (mp4-atoms mp4-file) *tree*))

(defparameter *ilst-data* (list +root+ +mp4-atom-moov+ +mp4-atom-udta+
                                +mp4-atom-meta+ +mp4-atom-ilst+ nil
                                +itunes-ilst-data+)
  "iTunes artist/album/etc path. The 5th element should be set to
one of the +iTunes- constants")

(defmethod tag-get-value (atoms atom-type)
  "Helper function to extract text from ILST atom's data atom"
  (declare #.utils:*standard-optimize-settings*)
  (setf (nth 5 *ilst-data*) atom-type)
  (aif (tree:at-path atoms *ilst-data* (lambda (x y)
                                         (= (mp4-atom:atom-type (tree:data x)) y)))
       (atom-value (tree:data it))
       nil))

(defun mp4-show-raw-tag-atoms (mp4-file-stream out-stream)
  "Show all the iTunes data atoms"
  (declare #.utils:*standard-optimize-settings*)
  (let ((top-node (tree:at-path (mp4-atoms mp4-file-stream)
                                (list +root+ +mp4-atom-moov+ +mp4-atom-udta+ +mp4-atom-meta+ +mp4-atom-ilst+)
                                (lambda (x y) (= (mp4-atom:atom-type (tree:data x)) y)))))
    (loop for node = (tree:first-child top-node)
            then (tree:next-sibling node) until (null node) do
            (format out-stream "~2t~a~%" (vpprint (tree:data node) nil)))))

(defun get-audio-properties-atoms (mp4-file)
  "Get the audio property atoms from MP4-FILE.
MP4A audio info is held in under root.moov.trak.mdia.mdhd,
root.moov.trak.mdia.minf.stbl.mp4a, and root.moov.trak.mdia.minf.stbl.mp4a.esds"
  (declare #.utils:*standard-optimize-settings*)

  (let ((mdhd (tree:find-tree (mp4-atoms mp4-file) (lambda (x) (= (atom-type (tree:data x)) +audioprop-mdhd+))))
        (mp4a (tree:find-tree (mp4-atoms mp4-file) (lambda (x) (= (atom-type (tree:data x)) +audioprop-mp4a+))))
        (esds (tree:find-tree (mp4-atoms mp4-file) (lambda (x) (= (atom-type (tree:data x)) +audioprop-esds+)))))

    (if (and mdhd mp4a esds)
        (values (tree:data (first mdhd))
                (tree:data (first mp4a))
                (tree:data (first esds)))
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
  (declare #.utils:*standard-optimize-settings*)
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
  "Find and parse the audio information in MP4-FILE"
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
