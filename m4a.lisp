;;; -*- Mode: Lisp;  show-trailing-whitespace: t; Base: 10; indent-tabs: nil; Syntax: ANSI-Common-Lisp; Package: M4A; -*-
;;; Copyright (c) 2013, Mark VandenBrink. All rights reserved.
(in-package #:m4a)

;;;; ATOMS
;;;
;;; A word about atoms (aka "boxes").  There are three kinds of atoms: ones that
;;; are containers, ones that are data, and ones that are both. A lot of the
;;; taggers out there mostly ignore third class and treat "container atoms that
;;; also have data" as a big blob of data that they rummage around in via
;;; indices.  Seems sort of broken, IMHO, so we'll try to handle all three if
;;; at all possible.
;;;

(defun mk-atom-class-name (name)
  "Create an atom class name by concatenating ATOM- with NAME, preserving case"
  (declare #.utils:*standard-optimize-settings*)
  (mkstr "atom-" name))
(memoize 'mk-atom-class-name)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun as-octets (str)
    (declare #.utils:*standard-optimize-settings*)
    (let ((ret 0))
      (setf (ldb (byte 8 24) ret) (char-code (aref str 0)))
      (setf (ldb (byte 8 16) ret) (char-code (aref str 1)))
      (setf (ldb (byte 8  8) ret) (char-code (aref str 2)))
      (setf (ldb (byte 8  0) ret) (char-code (aref str 3)))
      ret))

  (defun as-string (atom-type)
    "Given an integer, return the string representation"
    (declare #.utils:*standard-optimize-settings*)

    (with-output-to-string (s nil)
      (write-char (code-char (ldb (byte 8 24) atom-type)) s)
      (write-char (code-char (ldb (byte 8 16) atom-type)) s)
      (write-char (code-char (ldb (byte 8 8)  atom-type)) s)
      (write-char (code-char (ldb (byte 8 0)  atom-type)) s)))

  (defun as-octet (c)
    "Used below so that we can create atom 'types' from char/ints"
    (declare #.utils:*standard-optimize-settings*)

    (cond ((typep c 'standard-char) (coerce (char-code c) '(unsigned-byte 8)))
          ((typep c 'integer) (coerce c '(unsigned-byte 8)))
          (t (error "can only handle characters and integers"))))

  (defmacro mk-mp4-atom-type (l1 l2 l3 l4)
    "Given 4 chars/ints, create a string for the name"
    `(with-output-to-string (s nil)
       (write-char (code-char ,(as-octet l1)) s)
       (write-char (code-char ,(as-octet l2)) s)
       (write-char (code-char ,(as-octet l3)) s)
       (write-char (code-char ,(as-octet l4)) s))))

(defparameter *in-progress* nil "the node currently being worked upon")
(defparameter *tree*        nil "the root of the atom tree being constructed")

;;;; Atoms
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
        (progn
          (tree:add-child old *in-progress*)
          (call-next-method))
        (let ((*tree* *in-progress*)) ; must use dynamic binding for multi-threading
          (call-next-method)
          ;; HACK ALERT: this is the top of the tree, so stuff the root
          ;; in the TREE slot of a container atom
          (setf (tree me) *tree*)))))

(defmacro with-mp4-atom-slots ((instance) &body body)
  `(with-slots (atom-file-pos atom-size atom-type) ,instance
     ,@body))

;;;; Concrete atoms
(defclass atom-skip (mp4-atom) ())

(defmethod initialize-instance :after ((me atom-skip) &key mp4-file &allow-other-keys)
  "The 'skip' atom.  Used when we want to capture the header of atom, but don't want/need
to read the payload of an atom."
  (declare #.utils:*standard-optimize-settings*)

  (with-mp4-atom-slots (me)
    (stream-seek mp4-file (- atom-size 8) :current)))

(defclass atom-container (mp4-atom)
  ((tree :accessor tree :documentation "Note: this is ONLY set for the ROOT atom"))
  (:documentation "An atom that 'contains' other atoms"))

(defmethod initialize-instance :after ((me atom-container) &key mp4-file &allow-other-keys)
  (declare #.utils:*standard-optimize-settings*)

  (with-mp4-atom-slots (me)
    (loop for end = (+ atom-file-pos atom-size)
          for current = (stream-seek mp4-file) then (stream-seek mp4-file)
          while (< current end) do
            (make-mp4-atom mp4-file me))))

;;;; ILST ATOMS (ie atoms related to tagging)
;(defclass |atom-ilst| (atom-container) ())

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun generate-it (name type super-class)
    (let ((class-name (if super-class
                          `(defclass ,(mksym nil "atom-" type) (,super-class) ())))
          (constant `(defconstant* ,(mksym t "+itunes-" name "+") ,type))
          (exported `(export ',(mksym t "+itunes-" name "+"))))
      `(progn ,constant ,exported ,class-name)))

  (defmacro ncls-atom-entry (a b) `(list ,a ,b nil))             ; no superclass
  (defmacro ilst-atom-entry (a b) `(list ,a ,b '|atom-ilst|))    ; itunes tagging
  (defmacro skip-atom-entry (a b) `(list ,a ,b 'atom-skip))      ; read and skip
  (defmacro cont-atom-entry (a b) `(list ,a ,b 'atom-container)) ; contains other atoms, but no data

  (defparameter *handled-atoms*
    (list (ncls-atom-entry "root"                         "ROOT") ; pseudo-atom
          (ncls-atom-entry "ilst-data"                    "data")
          (ncls-atom-entry "atom-handler"                 "hdlr")
          (ncls-atom-entry "media-header"                 "mdhd")
          (ncls-atom-entry "sample-table-desc"            "stsd")
          (ncls-atom-entry "mp4a-codec"                   "mp4a")
          (ncls-atom-entry "elementary-stream-descriptor" "esds")
          (ncls-atom-entry "item-list"                    "ilst")
          (ncls-atom-entry "metadata"                     "meta")

          (ilst-atom-entry "account-type"                 "akID")
          (ilst-atom-entry "album"                        "©alb")
          (ilst-atom-entry "album-artist"                 "aART")
          (ilst-atom-entry "artist"                       "©ART")
          (ilst-atom-entry "at-id"                        "atID")
          (ilst-atom-entry "cn-id"                        "cnID")
          (ilst-atom-entry "comment"                      "©cmt")
          (ilst-atom-entry "compilation"                  "cpil")
          (ilst-atom-entry "composer"                     "©com")
          (ilst-atom-entry "content-rating"               "rtng")
          (ilst-atom-entry "copyright"                    "cprt")
          (ilst-atom-entry "cover-art"                    "covr")
          (ilst-atom-entry "disk"                         "disk")
          (ilst-atom-entry "encoder"                      "©enc")
          (ilst-atom-entry "flavor"                       "flvr")
          (ilst-atom-entry "gapless-playback"             "pgap")
          (ilst-atom-entry "ge-id"                        "geID")
          (ilst-atom-entry "genre"                        "gnre")
          (ilst-atom-entry "genre-x"                      "©gen")
          (ilst-atom-entry "groups"                       "©grp")
          (ilst-atom-entry "lyrics"                       "©lyr")
          (ilst-atom-entry "media-type"                   "stik")
          (ilst-atom-entry "purchase-account"             "apID")
          (ilst-atom-entry "purchased-date"               "purd")
          (ilst-atom-entry "sort-album"                   "soal")
          (ilst-atom-entry "sort-album-artist"            "soaa")
          (ilst-atom-entry "sort-artist"                  "soar")
          (ilst-atom-entry "sort-composer"                "soco")
          (ilst-atom-entry "sort-name"                    "sonm")
          (ilst-atom-entry "store"                        "sfID")
          (ilst-atom-entry "tempo"                        "tmpo")
          (ilst-atom-entry "title"                        "©nam")
          (ilst-atom-entry "tool"                         "©too")
          (ilst-atom-entry "track"                        "©trk")
          (ilst-atom-entry "track-n"                      "trkn")
          (ilst-atom-entry "writer"                       "©wrt")
          (ilst-atom-entry "unique-id"                    "xid ") ; Note: space at the end
          (ilst-atom-entry "year"                         "©day")

          (skip-atom-entry "composer-id"                  "cmID")
          (skip-atom-entry "mean"                         "mean")
          (skip-atom-entry "data-ref-url"                 "url ")
          (skip-atom-entry "data-ref-alis"                "alis")
          (skip-atom-entry "data-ref-rsrc"                "rsrc")
          (skip-atom-entry "unk-drm"                      "drms")
          (skip-atom-entry "edit-atom"                    "edts")
          (skip-atom-entry "free"                         "free")
          (skip-atom-entry "file-type"                    "ftyp")
          (skip-atom-entry "unk-iods"                     "iods")
          (skip-atom-entry "media-data-atom"              "mdat")
          (skip-atom-entry "movie-header"                 "mvhd")
          (skip-atom-entry "name-atom"                    "name")
          (skip-atom-entry "unk-pinf"                     "pinf")
          (skip-atom-entry "play-list-id"                 "plID")
          (skip-atom-entry "unk-sbtd"                     "sbtd")
          (skip-atom-entry "sound-media-header"           "smhd")
          (skip-atom-entry "sample-table-chunk-offset"    "stco")
          (skip-atom-entry "sample-table-sample"          "stsc")
          (skip-atom-entry "sample-table-size"            "stsz")
          (skip-atom-entry "sample-table-time"            "stts")
          (skip-atom-entry "track-header"                 "tkhd")

          (cont-atom-entry "free-form"                    "----")
          (cont-atom-entry "data-info"                    "dinf")
          (cont-atom-entry "item-list"                    "ilst")
          (cont-atom-entry "media"                        "mdia")
          (cont-atom-entry "media-info"                   "minf")
          (cont-atom-entry "movie"                        "moov")
          (cont-atom-entry "sample-table"                 "stbl")
          (cont-atom-entry "trak-header"                  "trak")
          (cont-atom-entry "user-data"                    "udta"))
 "This is the list of atoms we 'handle', where 'handle' may well indeed mean to
skip or just define a constant. Each entry is constant-name/class-name.
From this data structure we auto-generate (optionally) class-names
and (always) defconstants.")

  (defmacro mk-handled-atoms-constants-and-classes ()
    `(progn
       ,@(loop for e in *handled-atoms*
               collect
               (generate-it (first e) (second e) (third e))))))

;;; generate the handled atoms/constants/exports
(mk-handled-atoms-constants-and-classes)

(defclass |atom-data| (mp4-atom)
  ((atom-version :accessor atom-version :initarg :atom-version :initform nil)
   (atom-flags   :accessor atom-flags   :initarg :atom-flags   :initform nil)
   (atom-locale  :accessor atom-locale  :initarg :atom-locale  :initform nil)
   (atom-value   :accessor atom-value   :initarg :atom-value   :initform nil))
  (:documentation "Represents the 'data' portion of ilst data atom"))

(defmethod initialize-instance :after ((me |atom-data|) &key mp4-file parent &allow-other-keys)
  (declare #.utils:*standard-optimize-settings*)

  (with-slots (atom-size atom-type atom-version atom-flags atom-value atom-locale) me
    (setf atom-version (stream-read-u8 mp4-file)
          atom-flags   (stream-read-u24 mp4-file)
          atom-locale  (stream-read-u32 mp4-file))

    ;(dbg 'atom-data-init atom-version atom-flags atom-locale)

    ;; Ideally, we would be able to read the atom's value by looking
    ;; solely at the atom-flags; however, when atom-flags == 0, then
    ;; things get crazy---I can NOT for the life of me figure out
    ;; the trck/trkn/disk integer format, hence the mess below.
    (setf atom-value
          (ecase atom-flags
            (1
             (stream-read-utf-8-string mp4-file (- atom-size 16)))

            ((13 14)
             (stream-read-sequence mp4-file (- atom-size 16)))

            ((0 21)
             (cond      ; messy!
               ((member (atom-type parent)
                        (list +itunes-track+ +itunes-track-n+ +itunes-disk+)
                        :test #'string=)
                (stream-read-u16 mp4-file) ; throw away
                (let* ((a (stream-read-u16 mp4-file))
                       (b (stream-read-u16 mp4-file)))
                  (stream-seek mp4-file (- atom-size 16 6) :current)
                  (list a b)))

               (t (case (- atom-size 16)
                    (4 (stream-read-u32 mp4-file))
                    (2 (stream-read-u16 mp4-file))
                    (1 (stream-read-u8 mp4-file))
                    (otherwise
                     ;;(warn-user "file: ~a~%unknown atom-flags: ~a, reading ~:d octets"
                     ;;  audio-streams:*current-file* atom-flags (- atom-size 16))
                     (stream-read-sequence mp4-file (- atom-size 16)))))))))))

;(defclass |atom-dinf| (atom-container) ())

(defclass |atom-dref| (atom-container)
  ((version     :accessor version)
   (flags       :accessor flags)
   (num-entries :accessor num-entries))
  (:documentation "data reference atom"))

(defmethod initialize-instance :around ((me |atom-dref|) &key mp4-file &allow-other-keys)
  (declare #.utils:*standard-optimize-settings*)

  (with-slots (version flags num-entries entries) me
    (setf version     (stream-read-u8 mp4-file)
          flags       (stream-read-u24 mp4-file)
          num-entries (stream-read-u32 mp4-file))
    (call-next-method)))

    ;; (loop for i from 0 to (1- num-entries) do
    ;;   (setf (aref entries i)
    ;;         (make-instance 'data-reference
    ;;                        :mp4-file mp4-file)))))

(defclass |atom-hdlr| (mp4-atom)
  ((version :accessor version) ; 1 byte
   (flags   :accessor flags)   ; 3 bytes
   (qtype   :accessor qtype)   ; 4 bytes
   (mtype   :accessor mtype)   ; 4 bytes
   (resv    :accessor resv)    ; 4 bytes
   (rflag   :accessor rflag)   ; 4 bytes
   (rmask   :accessor rmask)   ; 4 bytes
   (mhdlr   :accessor mhdlr))) ; null-terminated string (but we're reading it as octets)

(defmethod initialize-instance :after ((me |atom-hdlr|) &key mp4-file &allow-other-keys)
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

(defclass |atom-mdhd| (mp4-atom)
  ((version  :accessor version)
   (flags    :accessor flags)
   (c-time   :accessor c-time)
   (m-time   :accessor m-time)
   (scale    :accessor scale)
   (duration :accessor duration)
   (lang     :accessor lang)
   (quality  :accessor quality)))

(defmethod initialize-instance :after ((me |atom-mdhd|) &key mp4-file &allow-other-keys)
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

(defclass |atom-esds| (mp4-atom)
  ((version      :accessor version)       ; 1 byte
   (flags        :accessor flags)         ; 3 bytes
   (esid         :accessor esid)          ; 2 bytes
   (s-priority   :accessor s-priority)    ; 1 byte
   (obj-id       :accessor obj-id)        ; 1 byte
   (s-type       :accessor s-type)        ; 1 byte (1 bit up-stream, 1-but reserved, 6-bits stream type
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
(defconstant* +mp4-odescrtag+               #x01)
(defconstant* +mp4-iodescrtag+              #x02)
(defconstant* +mp4-esdescrtag+              #x03)
(defconstant* +mp4-decconfigdescrtag+       #x04)
(defconstant* +mp4-decspecificdescrtag+     #x05)
(defconstant* +mp4-slconfigdescrtag+        #x06)
(defconstant* +mp4-contentiddescrtag+       #x07)
(defconstant* +mp4-supplcontentiddescrtag+  #x08)
(defconstant* +mp4-ipiptrdescrtag+          #x09)
(defconstant* +mp4-ipmpptrdescrtag+         #x0a)
(defconstant* +mp4-ipmpdescrtag+            #x0b)
(defconstant* +mp4-registrationdescrtag+    #x0d)
(defconstant* +mp4-esidincdescrtag+         #x0e)
(defconstant* +mp4-esidrefdescrtag+         #x0f)
(defconstant* +mp4-fileiodescrtag+          #x10)
(defconstant* +mp4-fileodescrtag+           #x11)
(defconstant* +mp4-extprofileleveldescrtag+ #x13)
(defconstant* +mp4-extdescrtagsstart+       #x80)
(defconstant* +mp4-extdescrtagsend+         #xfe)

(defmethod initialize-instance :after ((me |atom-esds|) &key mp4-file &allow-other-keys)
  (declare #.utils:*standard-optimize-settings*)

  (with-slots (version flags esid s-priority obj-id s-type buf-size max-bit-rate avg-bit-rate) me
    (setf version (stream-read-u8 mp4-file)
          flags   (stream-read-u24 mp4-file))

    (assert (= +MP4-ESDescrTag+ (stream-read-u8 mp4-file)) () "Expected description tag of ESDescrTag")

    (let* ((len         (read-descriptor-len mp4-file))
           (end-of-atom (+ (stream-seek mp4-file) len)))
      (setf esid       (stream-read-u16 mp4-file)
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

(defclass |atom-stsd| (mp4-atom)
  ((flags       :accessor flags)
   (version     :accessor version)
   (num-entries :accessor num-entries)))

(defmethod initialize-instance :after ((me |atom-stsd|) &key mp4-file &allow-other-keys)
  (declare #.utils:*standard-optimize-settings*)

  (with-slots (flags version num-entries) me
    (setf version     (stream-read-u8 mp4-file)
          flags       (stream-read-u24 mp4-file)
          num-entries (stream-read-u32 mp4-file))))

(defclass |atom-mp4a| (atom-container)
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

(defmethod initialize-instance :around ((me |atom-mp4a|) &key mp4-file &allow-other-keys)
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

(defclass |atom-meta| (atom-container)
  ((version  :accessor version)
   (flags    :accessor flags)))

(defmethod initialize-instance :around ((me |atom-meta|) &key mp4-file &allow-other-keys)
  (declare #.utils:*standard-optimize-settings*)

   (with-slots (version flags) me
     (setf version (stream-read-u8 mp4-file)
           flags   (stream-read-u24 mp4-file)))
  (call-next-method))

(defun find-atom-class (id)
  "Search by concatenating 'atom-' with ID and look for that symbol in this package"
  (declare #.utils:*standard-optimize-settings*)

  (let ((found-class-symbol (find-symbol (mk-atom-class-name id) :M4A)))

    ;(dbg 'find-atom-class id found-class-symbol)

    ;; if we found the class name, return the class (to be used for MAKE-INSTANCE)
    (when found-class-symbol
      (return-from find-atom-class (find-class found-class-symbol)))

    ;; didn't find a class, so return ATOM-SKIP class
    (warn-user "file ~a~%Unknown atom type <~a> encountered~%"
               audio-streams:*current-file* id)
    'atom-skip))
(memoize 'find-atom-class)

(defparameter *stop-on-count* 0)

(defun make-mp4-atom (mp4-file parent)
  "Get current file position, read in size/type, then construct the correct atom."
  (declare #.utils:*standard-optimize-settings*)

  (let* ((pos (stream-seek mp4-file))
         (siz (stream-read-u32 mp4-file))
         (typ (as-string (stream-read-u32 mp4-file)))
         (atom))
    (declare (fixnum pos siz))

    (when (= 0 siz)
      (error "trying to make an atom ~a with size of 0 at offset ~:d in file ~a"
             typ pos (stream-filename mp4-file)))

    ;(dbg 'make-mp4-atom pos siz typ)
    ;(incf *stop-on-count*)
    ;(when (> *stop-on-count* 100)
      ;(break))

    (setf atom (make-instance (find-atom-class typ)
                              :atom-size siz
                              :atom-type typ
                              :atom-file-pos pos
                              :parent parent
                              :mp4-file mp4-file))
    ;(dbg 'make-mp4-atom (type-of atom) (vpprint atom nil))
    atom))

(defmethod vpprint ((me mp4-atom) stream)
  "Pretty print an atom"
  (declare #.utils:*standard-optimize-settings*)

  (format stream "~a"
          (with-output-to-string (s)
            (with-mp4-atom-slots (me)
              (format s "Atom ~a @ ~:d of size ~:d"
                      atom-type atom-file-pos atom-size))
            (typecase me
              (|atom-data|
               (with-slots (atom-version atom-flags atom-value atom-type) me
                 (format s ", ilst fields: verison = ~d, flags = ~x, data = ~a"
                         atom-version atom-flags
                         (typecase atom-value
                           (array
                             (printable-array atom-value))
                           (otherwise
                            atom-value)))))))))

(defun is-valid-m4-file (mp4-file)
  "Make sure this is an MP4 file.  Quick check: is first atom type
(at file-offset 4) == 'FSTYP'?"
  (declare #.utils:*standard-optimize-settings*)

  (let ((valid)
        (size)
        (header))

    (when (> (stream-size mp4-file) 8)
      (stream-seek mp4-file 0 :start)
      (setf size   (stream-read-u32 mp4-file)
            header (as-string (stream-read-u32 mp4-file))
            valid  (and (<= size (stream-size mp4-file))
                        (string= header +itunes-file-type+))))
    (stream-seek mp4-file 0 :start)
    valid))

(defclass mp4-file ()
  ((filename   :accessor filename :initform nil :initarg :filename
               :documentation "filename that was parsed")
   (mp4-atoms  :accessor mp4-atoms  :initform nil
               :documentation "holds tree of parsed MP4 atoms/boxes")
   (audio-info :accessor audio-info :initform nil
               :documentation "holds the bit-rate, etc info"))
  (:documentation "For parsing MP4 audio files"))

(defun parse-audio-file (instream &optional (get-audio-info nil))
  "Given a valid MP4 file, look for the 'right' atoms and return them."
  (declare #.utils:*standard-optimize-settings*)

  (stream-seek instream 0 :start)

  (let* ((*in-progress* nil)          ; dynamic binding for multi-threading
         (parsed-info (make-instance 'mp4-file
                                     :filename (stream-filename instream))))
    (setf (mp4-atoms parsed-info)
          (tree      (make-instance 'atom-container
                                    :atom-type +itunes-root+
                                    :atom-file-pos 0
                                    :atom-size (stream-size instream)
                                    :mp4-file instream)))
    (when get-audio-info
      (setf (audio-info parsed-info) (get-mp4-audio-info parsed-info)))

    parsed-info))

(defparameter *ilst-data* (list +itunes-root+ +itunes-movie+ +itunes-user-data+
                                +itunes-metadata+ +itunes-item-list+ nil)
  "iTunes artist/album/etc path. The 5th element should be set to
one of the +iTunes- constants")

(defun tag-get-value (mp4-file atom-type)
  "Helper function to extract text from ILST atom's data atom"
  (declare #.utils:*standard-optimize-settings*)

  (setf (nth 5 *ilst-data*) atom-type)
  ;(dbg 'tag-get-value *ilst-data*)

  (aif (tree:at-path (mp4-atoms mp4-file) *ilst-data*
                     (lambda (x y)
                       (string= (atom-type (tree:data x)) y)))

       (let ((ret))
         ;; NB: only the COVR atom can have more than one data atom,
         ;; and it can be "data", "name", or "itif"(???).
         (loop for e = (tree:first-child it)
                 then (tree:next-sibling e)
               until (null e) do
                 (when (typep (tree:data e) '|atom-data|)
                   (push (atom-value (tree:data e)) ret)))
         (nreverse ret))
       nil))

(defun mp4-show-raw-tag-atoms (mp4-file-stream out-stream)
  "Show all the iTunes data atoms"
  (declare #.utils:*standard-optimize-settings*)

  (let ((top-node
          (tree:at-path (mp4-atoms mp4-file-stream)
                        (list +itunes-root+ +itunes-movie+ +itunes-user-data+
                              +itunes-metadata+ +itunes-item-list+)
                        (lambda (x y)
                          (string= (atom-type (tree:data x)) y)))))

    (loop for node = (tree:first-child top-node)
            then (tree:next-sibling node) until (null node) do
              (format out-stream "~2t~a" (vpprint (tree:data node) nil))
              (aif (tree:first-child node)
                   (format out-stream ", Data ~a~%" (vpprint (tree:data it) nil))
                   (format out-stream "~%")))))

(defun get-audio-properties-atoms (mp4-file)
  "Get the audio property atoms from MP4-FILE.
MP4A audio info is held in under root.moov.trak.mdia.mdhd,
root.moov.trak.mdia.minf.stbl.mp4a, and root.moov.trak.mdia.minf.stbl.mp4a.esds"
  (declare #.utils:*standard-optimize-settings*)

  (let ((mdhd
          (tree:find-tree
           (mp4-atoms mp4-file)
           (lambda (x)
             (string= (atom-type (tree:data x)) +itunes-media-header+))))
        (mp4a (tree:find-tree
               (mp4-atoms mp4-file)
               (lambda (x)
                 (string= (atom-type (tree:data x)) +itunes-mp4a-codec+))))
        (esds (tree:find-tree
               (mp4-atoms mp4-file)
               (lambda (x)
                 (string= (atom-type (tree:data x)) +itunes-elementary-stream-descriptor+)))))

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
  "Pretty print audio information"
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
          (setf channels        (num-chans mp4a)
                bits-per-sample (samp-size mp4a))
          (let* ((upper (ash (samp-rate mp4a) -16))
                 (lower (logand (samp-rate mp4a) #xffff)))
            (setf sample-rate (+ (float upper) (/ (float lower) 1000))))
        (when esds
          (setf avg-bit-rate (avg-bit-rate esds)
                max-bit-rate (max-bit-rate esds))))))
    info))

(defun map-mp4-atoms (m4a &key (func nil))
  "Visit each atom we found in M4A"
  (declare #.utils:*standard-optimize-settings*)
  (let ((count 0))
    (labels ((_internal-print (atom depth)
               (format t "~vt~a~%" depth (vpprint atom nil))
               (incf count)))
      (when (null func)
        (setf func #'_internal-print))
      (tree:traverse
       (m4a:mp4-atoms m4a)
       (lambda (node depth)
         (funcall func (tree:data node) depth))))
    (when count
      (format t "~:d atom~p found~%" count count))))
