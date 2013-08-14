;;; -*- Mode: Lisp;  show-trailing-whitespace: t; Base: 10; indent-tabs: nil; Syntax: ANSI-Common-Lisp; Package: MP4-ATOM; -*-
;;; Copyright (c) 2013, Mark VandenBrink. All rights reserved.
(in-package #:mp4-atom)

(log5:defcategory cat-log-mp4-atom)
(defmacro log-mp4-atom (&rest log-stuff) `(log5:log-for (cat-log-mp4-atom) ,@log-stuff))

(define-condition mp4-atom-condition ()
  ((location :initarg :location :reader location :initform nil)
   (object   :initarg :object   :reader object   :initform nil)
   (messsage :initarg :message  :reader message  :initform "Undefined Condition"))
  (:report (lambda (condition stream)
			 (format stream "mp4-atom condition at location: <~a> with object: <~a>: message: <~a>"
					 (location condition) (object condition) (message condition)))))

(defmethod print-object ((me mp4-atom-condition) stream)
  (format stream "location: <~a>, object: <~a>, message: <~a>" (location me) (object me) (message me)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ATOMS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; A word about atoms (aka "boxes").  There are three kinds of atoms: ones that are containers, ones
;;; that are data, and ones that are both. A lot of the source code for taggers out there mostly ignore
;;; the third class and treat "container atoms that also have data" as a big blob of data that they
;;; rummage around in via indices.  Seems sort of broke, IMHO, so we'll try to handle all three if
;;; at all possible.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun as-int (str)
  "Given a 4-byte string, return an integer type equivalent.
(eg (as-int \"hdlr\" == +audioprop-hdlr+))"
  (let ((int 0))
	(declare (integer int))
	(setf (ldb (byte 8 24) int) (char-code (aref str 0)))
	(setf (ldb (byte 8 16) int) (char-code (aref str 1)))
	(setf (ldb (byte 8 8) int)  (char-code (aref str 2)))
	(setf (ldb (byte 8 0) int)  (char-code (aref str 3)))
	int))

(defmethod as-string ((atom-type integer))
  "Given an integer representing an atom type, return the string form"
  (with-output-to-string (s nil)
	(write-char (code-char (ldb (byte 8 24) atom-type)) s)
	(write-char (code-char (ldb (byte 8 16) atom-type)) s)
	(write-char (code-char (ldb (byte 8 8)  atom-type)) s)
	(write-char (code-char (ldb (byte 8 0)  atom-type)) s)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun as-octet (c)
	"Used below so that we can create atom 'types' from char/ints"
	(cond ((typep c 'standard-char) (coerce (char-code c) '(unsigned-byte 8)))
		  ((typep c 'integer) (coerce c '(unsigned-byte 8)))
		  (t (error "can any handle characters and integers"))))

  (defmacro mk-mp4-atom-type (l1 l2 l3 l4)
	"Given 4 chars/ints, create a 32-bit word representing an atom 'type' (aka name)"
	`(let ((retval 0))
	   (setf (ldb (byte 8 24) retval) ,(as-octet l1))
	   (setf (ldb (byte 8 16) retval) ,(as-octet l2))
	   (setf (ldb (byte 8 8) retval)  ,(as-octet l3))
	   (setf (ldb (byte 8 0) retval)  ,(as-octet l4))
	   retval)))

(defconstant +itunes-album+          (mk-mp4-atom-type #xa9 #\a #\l #\b) "text: album name")
(defconstant +itunes-album-artist+   (mk-mp4-atom-type #\a  #\A #\R #\T) "text: album artist")
(defconstant +itunes-artist+         (mk-mp4-atom-type #xa9 #\A #\R #\T) "text: artist name")
(defconstant +itunes-comment+        (mk-mp4-atom-type #xa9 #\c #\m #\t) "text: comment, commonly used by iTunes for sound info, etc")
(defconstant +itunes-compilation+    (mk-mp4-atom-type #\c  #\p #\i #\l) "byte/boolean: is this file part of a compilation?")
(defconstant +itunes-composer+       (mk-mp4-atom-type #xa9 #\c #\o #\m) "text: composer name")
(defconstant +itunes-copyright+	     (mk-mp4-atom-type #\c  #\p #\r #\t) "text: copyright info")
(defconstant +itunes-cover-art+      (mk-mp4-atom-type #\c  #\o #\v #\r) "octets: cover art, PNG, etc")
(defconstant +itunes-disk+           (mk-mp4-atom-type #\d  #\i #\s #\k) "octets: disk number, can be n of N")
(defconstant +itunes-encoder+        (mk-mp4-atom-type #xa9 #\e #\n #\c) "text: who encoded")
(defconstant +itunes-genre+		     (mk-mp4-atom-type #\g  #\n #\r #\e) "octets: genre of file")
(defconstant +itunes-genre-x+		 (mk-mp4-atom-type #xa9 #\n #\r #\e) "text: yet another genre atom")
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
(defconstant +audioprop-mp4a+        (mk-mp4-atom-type #\m #\p #\4 #\a) "Found under trak.mdia.minf.stbl.stsd")
(defconstant +audioprop-essd+        (mk-mp4-atom-type #\e #\s #\s #\d) "Found under trak.mdia.minf.stbl.stsd.mp4a")

(defconstant +mp4-atom-ilst+         (mk-mp4-atom-type #\i #\l #\s #\t))
(defconstant +mp4-atom-mdia+         (mk-mp4-atom-type #\m #\d #\i #\a))
(defconstant +mp4-atom-meta+         (mk-mp4-atom-type #\m #\e #\t #\a))
(defconstant +mp4-atom-minf+         (mk-mp4-atom-type #\m #\i #\n #\f))
(defconstant +mp4-atom-moov+         (mk-mp4-atom-type #\m #\o #\o #\v))
(defconstant +mp4-atom-stbl+         (mk-mp4-atom-type #\s #\t #\b #\l))
(defconstant +mp4-atom-trak+         (mk-mp4-atom-type #\t #\r #\a #\k))
(defconstant +mp4-atom-udta+         (mk-mp4-atom-type #\u #\d #\t #\a))

(defun atom-read-loop (mp4-file end func)
  "Loop from start to end through a file and call FUNC for every ATOM we find. Used
at top-level and also for container ATOMs that need to read their contents."
  (log5:with-context "atom-read-loop"
	(do ()
		((>= (stream-seek mp4-file 0 :current) end))
	  (log-mp4-atom "atom-read-loop: @~:d before dispatch" (stream-seek mp4-file 0 :current))
	  (funcall func)
	  (log-mp4-atom "atom-read-loop: @~:d after dispatch" (stream-seek mp4-file 0 :current)))))

(defclass mp4-atom ()
  ((atom-file-position :accessor atom-file-position :initarg :atom-file-position)
   (atom-size :accessor atom-size :initarg :atom-size)
   (atom-type :accessor atom-type :initarg :atom-type)
   (atom-children :accessor atom-children :initform (make-mp4-atom-collection)))
  (:documentation "The minimal mp4-atom.  Note: not all atoms have children, but we put them here anyway to make things 'simple'"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; A collection of atoms (siblings) ;;;;;;;;;;;;;;;;;;;;
(defclass atom-collection ()
  ((atoms :accessor atoms :initform nil))
  (:documentation "A collection of sibling atoms"))

(defun make-mp4-atom-collection () (make-instance 'atom-collection))

(defmethod add ((me atom-collection) new-atom)
  "Adds new atom to the *end* (need to keep them in order we found them in the file) of this collection"
  (log5:with-context "add-atom-collection"
	(with-slots (atoms) me
	  ;(log-mp4-atom "adding ~a to atom collection: ~a" new-atom atoms)
	  (setf atoms (append atoms (list new-atom)))
	  ;(log-mp4-atom "collection now: ~a" atoms)
	  )))

(defmethod size ((me atom-collection))
  "Returns the number of atoms in this collection"
  (length (slot-value me 'atoms)))

(defmethod map-mp4-atom ((me atom-collection) &key (func nil) (depth nil))
  "Given a collection of atoms, call map-mp4-atom for each one"
  (log5:with-context "map-mp4-atom(collection)"
	(log-mp4-atom "map-mp4-atom: mapping collection: ~a" (slot-value me 'atoms))
	(dolist (a (slot-value me 'atoms))
	  (map-mp4-atom a :func func :depth depth))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Concreate atoms ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defclass atom-skip (mp4-atom) ())
(defmethod initialize-instance :after ((me atom-skip) &key (mp4-file nil) &allow-other-keys)
  "The 'skip' atom.  Used when we want to capture the header of atom, but don't want/need
to read the payload of an atom."
  (with-slots (atom-size atom-type) me
	(stream-seek mp4-file (- atom-size 8) :current)))

(defclass atom-raw-mixin ()
  ((raw-data :accessor raw-data)))
(defmethod initialize-instance :after ((me atom-raw-mixin) &key (mp4-file nil) &allow-other-keys)
  "The 'don't need to know contents, but want 'blob' of data read in' atom"
  (log5:with-context "atom-raw-mixin"
	(with-slots (raw-data atom-type atom-size) me
	  (log-mp4-atom "atom-raw-mixin: reading in ~d raw bytes for ~a" (- atom-size 8) (vpprint me nil))
	  (setf raw-data (stream-read-sequence mp4-file (- atom-size 8)))
	  ;;(utils::dump-data "/tmp/o.txt" raw-data)
	  )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ILST ATOMS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defclass atom-ilst (mp4-atom) ())
(defmethod initialize-instance :after ((me atom-ilst) &key (mp4-file nil) &allow-other-keys)
  "Construct an ilst atom.  ILST atoms are containers that hold data elements related to tagging.
Loop through this container and construct constituent atoms"
  (log5:with-context "atom-ilst-initializer"
	(with-slots (atom-size atom-type atom-children) me
	  (log-mp4-atom "atom-ilst-init: found ilst atom <~a> @ ~:d, looping for ~:d bytes"
					(as-string atom-type) (stream-seek mp4-file 0 :current) (- atom-size 8))
	  (atom-read-loop mp4-file (+ (stream-seek mp4-file 0 :current)  (- atom-size 8))
					  (lambda ()
						(let ((child (make-mp4-atom mp4-file atom-type)))
						  ;(log-mp4-atom "adding new child ~a" (vpprint child nil))
						  (add atom-children child))))))
  ;(log-mp4-atom "returning ilst atom: ~a" (vpprint me nil))
  )

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
   (log5:with-context "atom-data-init"
	 (with-slots (atom-size atom-type atom-version atom-flags atom-value atom-parent-type) me
	   (setf atom-version (stream-read-u8 mp4-file))
	   (setf atom-flags (stream-read-u24 mp4-file))
	   (assert (= 0 (stream-read-u32 mp4-file)) () "a data atom lacks the required null field") ; XXX is this true?
	   (log-mp4-atom "atom-data-init: size = ~:d, name = ~a, version = ~d, flags = ~x" atom-size (as-string atom-type) atom-version atom-flags)
	   (setf atom-value (decode-ilst-data-atom atom-type me atom-parent-type mp4-file))
	   (log-mp4-atom "atom-data-init: made an ilst atom-data: ~a" (vpprint me nil)))))

;;; the ILST atom decoders.  First, a lot of the decoders do the same thing, so we define a macros
;;; and use those for the relevants atoms.
(defgeneric decode-ilst-data-atom (type atom atom-parent-type mp4-file))

(defmacro simple-text-decode (type)
  `(defmethod decode-ilst-data-atom ((type (eql +itunes-ilst-data+)) atom (atom-parent-type (eql ,type)) mp4-file)
	 (stream-read-string-with-len mp4-file (- (atom-size atom) 16))))

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

(defmacro simple-a-b-decode (type)
  `(defmethod decode-ilst-data-atom ((type (eql +itunes-ilst-data+)) atom (atom-parent-type (eql ,type)) mp4-file)
	 (declare (ignore atom))
	 (stream-read-u16 mp4-file)					; throw away XXX Why?
	 (let ((a) (b))
	   (setf a (stream-read-u16 mp4-file))
	   (setf b (stream-read-u16 mp4-file))
	   (stream-read-u16 mp4-file)				; throw away XXX Why?
	   (list a b))))

(simple-a-b-decode +itunes-track+)
(simple-a-b-decode +itunes-track-n+)

(defmethod decode-ilst-data-atom ((type (eql +itunes-ilst-data+)) atom (atom-parent-type (eql +itunes-disk+)) mp4-file)
  (declare (ignore atom))
  (stream-read-u16 mp4-file)					; throw away XXX Why?
  (let ((a) (b))
	(setf a (stream-read-u16 mp4-file))
	(setf b (stream-read-u16 mp4-file))
	(list a b)))

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
;; (defclass mp4-hdlr-atom (atom-raw-mixin mp4-atom)())  ; XXX need to get real format for this...

;;; song length is seconds is (float duration) / (float scale)
;; (defclass atom-mdhd (mp4-atom)
;;   ((version  :accessor version)
;;    (flags    :accessor flags)
;;    (c-time   :accessor c-time)
;;    (m-time   :accessor m-time)
;;    (scale    :accessor scale)
;;    (duration :accessor duration)
;;    (lang     :accessor lang)
;;    (quality  :accessor quality)))

;; (defmethod initialize-instance :after ((me atom-mdhd) &key (mp4-file nil) &allow-other-keys)
;;   (with-slots (version flags c-time m-time scale duration lang quality) me
;; 	(setf version  (stream-read-u8 mp4-file))
;; 	(setf flags    (stream-read-u24 mp4-file))
;; 	(setf c-time   (stream-read-u32 mp4-file))
;; 	(setf m-time   (stream-read-u32 mp4-file))
;; 	(setf scale    (stream-read-u32 mp4-file))
;; 	(setf duration (if (= 0 version) (stream-read-u32 mp4-file) (stream-read-u64 mp4-file)))
;; 	(setf lang     (stream-read-u16 mp4-file))
;; 	(setf quality  (stream-read-u16 mp4-file))))

;; (defclass atom-stsd (mp4-atom)
;;   ((flags       :accessor flags)
;;    (version     :accessor version)
;;    (num-entries :accessor num-entries)))

;; (defmethod initialize-instance :after ((me atom-stsd) &key (mp4-file nil) &allow-other-keys)
;;   (log5:with-context "atom-stsd"
;; 	(with-slots (flags version num-entries)
;; 	  (setf version (stream-read-u8 mp4-file))
;; 	  (setf flags (stream-read-u24 mp4-file))
;; 	  (setf num-entries (stream-read-u32 mp4-file))
;; 	  (log-mp4-atom "atom-stsd: version = ~d, flags = ~x, num-fields = ~d" version flags num-entries))))

;; (defclass atom-mp4a (mp4-atom)
;;   ((reserved    :accessor reserved)    ; 6 bytes
;;    (d-ref-idx   :accessor d-ref-idx)   ; 2 bytes
;;    (version     :accessor version)     ; 2 bytes
;;    (revision    :accessor revision)    ; 2 bytes
;;    (vendor      :accessor vendor)      ; 4 bytes
;;    (num-chans   :accessor num-chans)   ; 2 bytes
;;    (samp-size   :accessor samp-size)   ; 2 bytes
;;    (comp-id     :accessor comp-id)     ; 2 bytes
;;    (packet-size :accessor packet-size) ; 2 bytes
;;    (samp-rate   :accessor samp-rate)   ; 4 bytes

;; (defmethod initialize-instance :after ((me atom-mp4a) &key (mp4-file nil) &allow-other-keys)
;;   (log5:with-context "atom-mp4a"
;; 	(with-slots (reserved d-ref-idx version revision vendor num-chans samp-size comp-id packet-size samp-rate) me
;; 	  (setf reserved   (stream-read-sequence mp4-file 6))
;; 	  (setf d-ref-idx  (stream-read-u16 mp4-file))
;; 	  (setf version	   (stream-read-u16 mp4-file))
;; 	  (setf revision   (stream-read-u16 mp4-file))
;; 	  (setf vendor	   (stream-read-u32 mp4-file))
;; 	  (setf num-chans  (stream-read-u16 mp4-file))
;; 	  (setf samp-size  (stream-read-u16 mp4-file))
;; 	  (setf comp-id	   (stream-read-u16 mp4-file))
;; 	  (setf packet-size (stream-read-u16 mp4-file))
;; 	  (setf samp-rate  (stream-read-u32 mp4-file)))))

(defun read-container-atoms (mp4-file parent-atom)
  "loop through a container atom and add it's children to it"
  (with-slots (atom-children atom-file-position atom-of-interest atom-size atom-type atom-decoded) parent-atom
	(atom-read-loop mp4-file (+ atom-file-position atom-size)
					(lambda ()
					  (let ((child (make-mp4-atom mp4-file atom-type)))
						(log-mp4-atom "read-container-atoms: adding new child ~a" (vpprint child nil))
						(add atom-children child))))))

(defclass atom-meta (mp4-atom)
  ((version  :accessor version)
   (flags    :accessor flags)))
(defmethod initialize-instance :after ((me atom-meta) &key (mp4-file nil) &allow-other-keys)
   (with-slots (version flags) me
	 (setf version  (stream-read-u8 mp4-file))
	 (setf flags    (stream-read-u24 mp4-file))
	 (read-container-atoms mp4-file me)))

(defclass atom-moov (mp4-atom) ())
(defmethod initialize-instance :after ((me atom-moov) &key (mp4-file nil) &allow-other-keys)
  (read-container-atoms mp4-file me))
(defclass atom-udta (mp4-atom) ())
(defmethod initialize-instance :after ((me atom-udta) &key (mp4-file nil) &allow-other-keys)
  (read-container-atoms mp4-file me))
(defclass atom-mdia (mp4-atom) ())
(defmethod initialize-instance :after ((me atom-mdia) &key (mp4-file nil) &allow-other-keys)
  (read-container-atoms mp4-file me))

(defun find-atom-class (id)
  "Search by concatenating 'atom-' with ID and look for that symbol in this package"
  (log5:with-context "find-atom-class"
	(log-mp4-atom "find-atom-class: looking for class <~a>" (as-string id))
	(let ((found-class-symbol (find-symbol (string-upcase (concatenate 'string "atom-" (as-string id))) :MP4-ATOM))
		  (found-class))

	  ;; if we found the class name, return the class (to be used for MAKE-INSTANCE)
	  (when found-class-symbol
		(setf found-class (find-class found-class-symbol))
		(log-mp4-atom "find-atom-class: found class: ~a" found-class)
		(return-from find-atom-class (find-class found-class-symbol))))

	;; didn't find a class, so return ATOM-SKIP class
	(log-mp4-atom "find-atom-class: class not found")
	'atom-skip))

(defun make-mp4-atom (mp4-file &optional parent-type)
  "Get current file position, read in size/type, then construct the correct atom."
  (log5:with-context "make-mp4-atom"
	(let* ((pos (stream-seek mp4-file 0 :current))
		   (siz (stream-read-u32 mp4-file))
		   (typ (stream-read-u32 mp4-file))
		   (atom))
	  (declare (type integer pos siz typ))

	  (log-mp4-atom "make-mp4-atom: @ pos = ~:d of size = ~:d and type = ~a" pos siz (as-string typ))

	  (when (= 0 siz)
		(error "trying to make an atom ~a with size of 0 at offset ~:d in ~a, ammending size to be 8" 
			   (as-string typ) pos (stream-filename mp4-file)))

	  (setf atom (make-instance (find-atom-class typ) :atom-size siz :atom-type typ :atom-file-position pos :mp4-file mp4-file :atom-parent-type parent-type))
	  (log-mp4-atom "make-mp4-atom: made ~a" (vpprint atom nil))
	  atom)))

(defmethod vpprint ((me mp4-atom) stream)
  (format stream "~a" (with-output-to-string (s)
						(with-slots (atom-children atom-file-position atom-size atom-type) me
						  (format s "ATOM: type: <~a> @ ~:d of size ~:d and child count of ~d"
								  (as-string atom-type) atom-file-position atom-size (size atom-children)))
						(if (typep me 'atom-data)
							(with-slots (atom-version atom-flags atom-value atom-type atom-parent-type) me
							  (format s " having ilst fields: atom-parent-type = ~a, verison = ~d, flags = ~x, data = ~x"
									  (as-string atom-parent-type) atom-version atom-flags 
									  (if (typep atom-value 'array) (printable-array atom-value) atom-value)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun is-valid-m4-file (mp4-file)
  "Make sure this is an MP4 file.  Quick check: is first atom (at file-offset 4) == FSTYP?"
  (stream-seek mp4-file 0 :start)
  (let* ((size (stream-read-u32 mp4-file))
		 (header (stream-read-u32 mp4-file)))
	(declare (ignore size))
	(stream-seek mp4-file 0 :start)
	(= header +m4-ftyp+)))

(defun find-mp4-atoms (mp4-file)
  "Given a valid MP4 file mp4-file, look for the 'right' atoms and return them."
  (log5:with-context "find-mp4-atoms"
	(when (not (is-valid-m4-file mp4-file))
	  (error 'mp4-atom-condition :location "find-mp4-atoms" :object mp4-file :message "is not an mp4-file" ))

	(log-mp4-atom "find-mp4-atoms: ~a, before read-file loop, file-position = ~:d, end = ~:d"
				  (stream-filename mp4-file) (stream-seek mp4-file 0 :current) (stream-size mp4-file))
	(setf (mp4-atoms mp4-file) (make-mp4-atom-collection))
	(atom-read-loop mp4-file (stream-size mp4-file)
					(lambda ()
					  (let ((new-atom (make-mp4-atom mp4-file)))
						(when new-atom
						  (add (mp4-atoms mp4-file) new-atom)))))

	(log-mp4-atom "find-mp4-atoms: returning atom-collection of size ~d" (size (mp4-atoms mp4-file)))))

(defmethod map-mp4-atom ((me mp4-atom) &key (func nil) (depth nil))
  "traverse all atoms under a given atom"
  (log5:with-context "map-mp4-atom(single)"
	(labels ((_indented-atom (atom depth)
			   (format t "~vt~a~%"  (if (null depth) 0 depth) (vpprint atom nil))))
	  (with-slots (atom-type atom-children) me
		(log-mp4-atom "map-mp4-atom: begining traversal with ~a, I have ~d children" (as-string atom-type) (size atom-children))
		(when (null func)
		  (setf func #'_indented-atom))
		(funcall func me depth)
		(map-mp4-atom atom-children :func func :depth (if (null depth) nil (+ 1 depth)))))))

(defmethod traverse ((me mp4-atom) path)
  "Used in finding nested atoms.
Given an atom and a path, if atom-type matches first element of path, then we've found our match."
  (log5:with-context "traverse-atom"
	(log-mp4-atom "traverse (mp4-atom): entered with ~a ~a" (as-string (atom-type me)) path)
	(cond ((null path)
		   (error "Path exhausted in travese atom")	; don't think this can happen?
		   nil)
		  ((= (atom-type me) (first path))
		   (log-mp4-atom "traverse (mp4-atom): current path matches thus far ~a ~a" (atom-type me) path)
		   (cond
			 ((= 1 (length path))
			  (log-mp4-atom "traverse (mp4-atom): length of path is 1, so found!")
			  (return-from traverse me)))))

	(log-mp4-atom "traverse (mp4-atom): current path doesn't match ~a ~a" (atom-type me) path)
	nil))

(defmethod traverse ((me atom-collection) path)
  "Used in finding nested atoms. Seach the collection and if we find a match with first of path,
call traverse atom (unless length of path == 1, in which case, we've found our match)"
  (log5:with-context "traverse-atom-collection"
	(log-mp4-atom "traverse (atom-collection): entering with ~a ~a" me path)
	(dolist (sibling (atoms me))	; cleaner than using map-mp4-atom, but still a kludge
	  (with-slots (atom-type atom-children) sibling
		(log-mp4-atom "traverse (atom-collection): looking at ~x::~x" atom-type (first path))
		(when (= atom-type (first path))
		  (cond
			((= 1 (length path))
			 (log-mp4-atom "traverse (atom-collection): found ~a" sibling)
			 (return-from traverse sibling))
			(t
			 (log-mp4-atom "traverse (atom-collection): path matches, calling traverse atom with ~a, ~a" atom-children (rest path))
			 (let ((found (traverse atom-children (rest path))))
			   (if found (return-from traverse found))))))))
	(log-mp4-atom "traverse (atom-collection): looked at all, found nothing")
	nil))

(defmethod tag-get-value (atoms node)
  "Helper function to extract text from atom's data atom"
  (let ((atom (traverse atoms
						(list +mp4-atom-moov+ +mp4-atom-udta+ +mp4-atom-meta+ +mp4-atom-ilst+ node +itunes-ilst-data+))))
	(if atom
		(atom-value atom)
		nil)))

(defun mp4-show-raw-tag-atoms (mp4-file-stream)
  (map-mp4-atom (mp4-atom::traverse (mp4-atoms mp4-file-stream)
									(list +mp4-atom-moov+ +mp4-atom-udta+ +mp4-atom-meta+ +mp4-atom-ilst+))
				:depth 0
				:func (lambda (atom depth)
						(when (= (atom-type atom) +itunes-ilst-data+)
						  (format t "~vt~a~%" depth (vpprint atom nil))))))


(defun find-all (base name)
  "Starting as BASE atom, recursively search for all instances of NAME"
  (let* ((search-name (if (typep name 'string) (as-int name) name))
		 (found))

	(map-mp4-atom base
				  :func (lambda (atom depth)
						  (declare (ignore depth))
						  (when (= (atom-type atom) search-name)
							(push atom found))))
	found))

;; (defun get-audio-properties-atoms (mp4-file)
;;   "First, find all TRAKs under moov. For the one that contains a HDLR atom with DATA of 'soun',
;; return trak.mdia.mdhd and trak.mdia.minf.stbl.stsd"
;;   (dolist (track (find-all (traverse (mp4-atoms mp4-file) (list +mp4-atom-moov+)) "trak"))
;; 	(format t "track = ~a~%" track)
;; 	(let ((hdlr (traverse track (list +mp4-atom-mdia+ +audioprop-hdlr+))))
;; 	  (format t "hdlr = ~a~%" hdlr)
;; 	  (when (and (not (null hdlr))
;; 				 (string= "soun" (subseq (data hdlr) 8 12)))

;; 		;; we've found the correct track, extract atoms
;; 		(return-from get-audio-properties-atoms (values (traverse track (list +mp4-atom-mdia+ +audioprop-mdhd+))
;; 														(traverse track (list +mp4-atom-mdia+ +mp4-atom-minf+ +mp4-atom-stbl+ +audioprop-stsd+)))))))
;;   nil)
