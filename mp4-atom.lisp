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

(defmethod  as-string ((atom-type integer))
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
	   retval))

  (defconstant +m4-ftyp+             (mk-mp4-atom-type #\f #\t #\y #\p)
	"This should be the first atom type found in file")

  (defconstant +itunes-ilst-data+      (mk-mp4-atom-type #\d #\a #\t #\a)
	"Carries the actual data under an ilst atom")

  (defconstant +itunes-lyrics+         (mk-mp4-atom-type #xa9 #\l #\y #\r)) ; text
  (defconstant +itunes-copyright+	   (mk-mp4-atom-type #\c  #\p #\r #\t)) ; text
  (defconstant +itunes-album+          (mk-mp4-atom-type #xa9 #\a #\l #\b)) ; text
  (defconstant +itunes-artist+         (mk-mp4-atom-type #xa9 #\A #\R #\T)) ; text
  (defconstant +itunes-comment+        (mk-mp4-atom-type #xa9 #\c #\m #\t)) ; text
  (defconstant +itunes-compilation+    (mk-mp4-atom-type #\c  #\p #\i #\l)) ; byte/boolean
  (defconstant +itunes-composer+       (mk-mp4-atom-type #xa9 #\c #\o #\m)) ; text
  (defconstant +itunes-cover-art+      (mk-mp4-atom-type #\c  #\o #\v #\r)) ; octets
  (defconstant +itunes-year+           (mk-mp4-atom-type #xa9 #\d #\a #\y)) ; text
  (defconstant +itunes-disk+           (mk-mp4-atom-type #\d  #\i #\s #\k)) ; octets
  (defconstant +itunes-tool+           (mk-mp4-atom-type #xa9 #\t #\o #\o)) ; text
  (defconstant +itunes-genre+		   (mk-mp4-atom-type #\g  #\n #\r #\e)) ; octet
  (defconstant +itunes-genre-x+		   (mk-mp4-atom-type #xa9 #\n #\r #\e)) ; text
  (defconstant +itunes-groups+         (mk-mp4-atom-type #xa9 #\g #\r #\p)) ; text
  (defconstant +itunes-title+          (mk-mp4-atom-type #xa9 #\n #\a #\m)) ; text
  (defconstant +itunes-tempo+          (mk-mp4-atom-type #\t  #\m #\p #\o)) ; octet
  (defconstant +itunes-track+          (mk-mp4-atom-type #xa9 #\t #\r #\k)) ; octet
  (defconstant +itunes-track-n+        (mk-mp4-atom-type #\t  #\r #\k #\n)) ; octet
  (defconstant +itunes-writer+         (mk-mp4-atom-type #xa9 #\w #\r #\t)) ; text
  (defconstant +itunes-encoder+        (mk-mp4-atom-type #xa9 #\e #\n #\c)) ; text
  (defconstant +itunes-album-artist+   (mk-mp4-atom-type #\a  #\A #\R #\T)) ; text
  (defconstant +itunes-purchased-date+ (mk-mp4-atom-type #\p  #\u #\r #\d)) ; text

  (defparameter *itunes-text-atom-types*
	(list
	 +itunes-album+
	 +itunes-album-artist+
	 +itunes-artist+
	 +itunes-comment+
	 +itunes-composer+
	 +itunes-copyright+
	 +itunes-year+
	 +itunes-encoder+
	 +itunes-groups+
	 +itunes-genre-x+
	 +itunes-lyrics+
	 +itunes-purchased-date+
	 +itunes-title+
	 +itunes-tool+
	 +itunes-writer+)
	"These are all the itunes atoms that are stored as text")

  (defparameter *itunes-atom-types*
	(append *itunes-text-atom-types*
			(list
			 +itunes-compilation+
			 +itunes-cover-art+
			 +itunes-disk+
			 +itunes-genre+
			 +itunes-tempo+
			 +itunes-track+
			 +itunes-track-n+))
	"The iTunes atom types we can decode")

  (defconstant +mp4-atom-moov+ (mk-mp4-atom-type #\m #\o #\o #\v))
  (defconstant +mp4-atom-udta+ (mk-mp4-atom-type #\u #\d #\t #\a))
  (defconstant +mp4-atom-mdia+ (mk-mp4-atom-type #\m #\d #\i #\a))
  (defconstant +mp4-atom-meta+ (mk-mp4-atom-type #\m #\e #\t #\a))
  (defconstant +mp4-atom-ilst+ (mk-mp4-atom-type #\i #\l #\s #\t))

  (defparameter *atoms-of-interest*
	(list +mp4-atom-moov+
		  +mp4-atom-udta+
		  +mp4-atom-mdia+
		  +mp4-atom-meta+
		  +mp4-atom-ilst+)
	"For these container atoms, we look inside these atoms to read nested atoms")

  (defparameter *tag-path* (list +mp4-atom-moov+ +mp4-atom-udta+ +mp4-atom-meta+ +mp4-atom-ilst+)
	"The 'path' of nested atoms at which tag data can be found.")

  (defgeneric decode-ilst-data-atom (type atom atom-parent-type mp4-file))

  (defmacro generate-generic-text-methods ()
	"generate the decode methods for text atoms"
	(let ((methods))
	  (dolist (type *itunes-text-atom-types*)
		(push `(defmethod decode-ilst-data-atom ((type (eql +itunes-ilst-data+)) atom (atom-parent-type (eql ,type)) mp4-file)
				 (stream-read-string-with-len mp4-file (- (atom-size atom) 16))) methods))
	  `(progn ,@methods)))
  )

(generate-generic-text-methods)

(defmethod decode-ilst-data-atom ((type (eql +itunes-ilst-data+)) atom (atom-parent-type (eql +itunes-disk+)) mp4-file)
  "decode itunes DISK atom"
  (declare (ignore atom))
  (stream-read-u16 mp4-file)					; throw away
  (let ((a) (b))
	(setf a (stream-read-u16 mp4-file))
	(setf b (stream-read-u16 mp4-file))
	(list a b)))

(defmethod decode-ilst-data-atom ((type (eql +itunes-ilst-data+)) atom (atom-parent-type (eql +itunes-track+)) mp4-file)
  "decode itunes TRK atom"
  (declare (ignore atom))
  (stream-read-u16 mp4-file)					; throw away
  (let ((a) (b))
	(setf a (stream-read-u16 mp4-file))
	(setf b (stream-read-u16 mp4-file))
	(stream-read-u16 mp4-file)					; throw away
	(list a b)))

(defmethod decode-ilst-data-atom ((type (eql +itunes-ilst-data+)) atom (atom-parent-type (eql +itunes-track-n+)) mp4-file)
  "decode itunes TRKN atom"
  (declare (ignore atom))
  (stream-read-u16 mp4-file)					; throw away
  (let ((a) (b))
	(setf a (stream-read-u16 mp4-file))
	(setf b (stream-read-u16 mp4-file))
	(stream-read-u16 mp4-file)					; throw away
	(list a b)))

(defmethod decode-ilst-data-atom ((type (eql +itunes-ilst-data+)) atom (atom-parent-type (eql +itunes-tempo+)) mp4-file)
  "decode itunes TMPO atom"
  (declare (ignore atom))
  (stream-read-u16 mp4-file))

(defmethod decode-ilst-data-atom ((type (eql +itunes-ilst-data+)) atom (atom-parent-type (eql +itunes-genre+)) mp4-file)
  "decode itunes GNRE atom"
  (declare (ignore atom))
  (stream-read-u16 mp4-file))

(defmethod decode-ilst-data-atom ((type (eql +itunes-ilst-data+)) atom (atom-parent-type (eql +itunes-compilation+)) mp4-file)
  "decode itunes CPIL atom"
  (declare (ignore atom))
  (stream-read-u8 mp4-file))

(defmethod decode-ilst-data-atom ((type (eql +itunes-ilst-data+)) atom (atom-parent-type (eql +itunes-cover-art+)) mp4-file)
  (let ((blob (make-instance 'mp4-unhandled-data)))
	(setf (slot-value blob 'blob) (stream-read-sequence mp4-file (- (atom-size atom) 16)))
	blob))


(defclass mp4-atom ()
  ((atom-file-position :accessor atom-file-position :initarg :atom-file-position)
   (atom-size :accessor atom-size :initarg :atom-size)
   (atom-type :accessor atom-type :initarg :atom-type)
   (atom-children :accessor atom-children :initform (make-mp4-atom-collection)))
  (:documentation "The minimal mp4-atom.  Note: not all atoms have children, but we put them here anyway to make things 'simple'"))

(defclass mp4-ilst-atom (mp4-atom)
  ())

(defmethod initialize-instance :after ((me mp4-ilst-atom) &key (mp4-file nil) &allow-other-keys)
  "Construct an ilst atom"
  (log5:with-context "mp4-ilst-atom-initializer"
	(assert (not (null mp4-file)) () "Must pass a stream into this method")
	(with-slots (atom-size atom-type atom-children) me
	  (let* ((start (stream-seek mp4-file 0 :current))
			 (end (+ start (- atom-size 8))))
		(log-mp4-atom "mp4-ilst-atom-initializer:entry, start = ~:d, end = ~:d" start end)
		(do* ()
			 ((>= (stream-seek mp4-file 0 :current) end))
		  (log-mp4-atom "ilst atom top of loop: start = ~:d, current = ~:d, end = ~:d"
						start (stream-seek mp4-file 0 :current) end)
		  (let ((child (make-mp4-atom mp4-file atom-type)))
			 (log-mp4-atom "adding new child ~a" (vpprint child nil))
			 (add atom-children child)))))
	 (log-mp4-atom "Returning ilst atom: ~a" (vpprint me nil))))

 (defclass mp4-ilst-generic-data-atom (mp4-atom)
   ((atom-version :accessor atom-version :initarg :atom-version)
	(atom-flags   :accessor atom-flags   :initarg :atom-flags)
	(atom-value   :accessor atom-value   :initarg :atom-value)
	(atom-parent-type  :accessor atom-parent-type  :initarg :atom-parent-type :initform nil))
   (:documentation   "Represents the 'data' portion of ilst data atom"))

 (defmethod initialize-instance :after ((me mp4-ilst-generic-data-atom) &key mp4-file &allow-other-keys)
   (log5:with-context "mp4-ilst-generic-data-atom-initializer"
	 (assert (not (null mp4-file)) () "Must pass a stream into this method")
	 (log-mp4-atom "mp4-ilst-generic-data-atom-initializer:entry")
	 (with-slots (atom-size atom-type atom-version atom-flags atom-value atom-parent-type) me
	   (setf atom-version (stream-read-u8 mp4-file))
	   (setf atom-flags (stream-read-u24 mp4-file))
	   (if (= atom-type +itunes-ilst-data+)
		   (assert (= 0 (stream-read-u32 mp4-file)) () "a data atom lacks the required null field"))
	   (log-mp4-atom "size = ~:d, name = ~a, version = ~d, flags = ~x"
					 atom-size (as-string atom-type) atom-version atom-flags)
	   (setf atom-value (decode-ilst-data-atom atom-type me atom-parent-type mp4-file))
	   (log-mp4-atom "generic atom: ~a" (vpprint me nil)))))

(defclass mp4-container-atom (mp4-atom)
  ()
  (:documentation "The class representing an mp4 container atom"))

(defmethod initialize-instance :after ((me mp4-container-atom) &key (mp4-file nil) &allow-other-keys)
  "Upon initializing a container mp4 atom, read the nested atoms within it.'"
  (log5:with-context "mp4-container-atom-initializer"
	(assert (not (null mp4-file)) () "Must pass a stream into this method")

	(log-mp4-atom "mp4-container-atom-initializer")
	(with-slots (atom-children atom-file-position atom-of-interest atom-size atom-type atom-decoded) me

	  (log-mp4-atom "entry: starting file position = ~:d, atom ~a" atom-file-position (vpprint me nil))
	  (log-mp4-atom "type ~a is container atom of interest; read the nested atoms" (as-string atom-type))
	  (cond ((= atom-type +mp4-atom-meta+)
			 (log-mp4-atom "got META, moving file position forward 4 bytes") ;null field
			 (stream-seek mp4-file 4 :current)))

	  ;; we are now at the file-position we are need to be at, so start reading those atoms!
	  (block stream-read-file
		(log-mp4-atom "starting stream-read-file block with file-position = ~:d and end = ~:d" atom-file-position (+ atom-file-position atom-size))
		(do ()
			((>= (stream-seek mp4-file 0 :current) (+ atom-file-position atom-size)))
		  (log-mp4-atom "Top of loop: currently at file-position ~:d (reading up to ~:d)" (stream-seek mp4-file 0 :current) (+ atom-file-position atom-size))
		  (let ((child (make-mp4-atom mp4-file)))
			(log-mp4-atom "adding new child ~a" (vpprint child nil))
			(add atom-children child))))
	  (log-mp4-atom "ended stream-read-file block, file position now ~:d" (stream-seek mp4-file 0 :current)))))

(defun make-mp4-atom (mp4-file &optional atom-parent-type)
  "Get current file position, read in size/type, then construct the correct atom.
If type is an ilst type, read it all it.  If it is a container atom of interest,
leave file position as is, since caller will want to read in nested atoms.  Otherwise,
seek forward past end of this atom."
  (log5:with-context "make-mp4-atom"
	(let* ((pos (stream-seek mp4-file 0 :current))
		   (siz (stream-read-u32 mp4-file))
		   (typ (stream-read-u32 mp4-file))
		   (atom))
	  (declare (type integer pos siz typ))
	  (when (= 0 siz)
		(warn "trying to make an atom ~a with size of 0 at offset ~:d in ~a, ammending size to be 8" (as-string typ) pos (stream-filename mp4-file))
		(setf siz 8))
	  (log-mp4-atom "pos = ~:d, size = ~:d, type = ~a" pos siz (as-string typ))
	  (cond ((member typ *atoms-of-interest*)
			 (log-mp4-atom  "~a is a container atom we are interested in" (as-string typ))
			 (setf atom (make-instance 'mp4-container-atom :atom-size siz :atom-type typ :atom-file-position pos :mp4-file mp4-file)))
			((member typ *itunes-atom-types*)
			 (log-mp4-atom "~a is an ilst atom, read it all in" (as-string typ))
			 (setf atom (make-instance 'mp4-ilst-atom :atom-size siz :atom-type typ :atom-file-position pos :mp4-file mp4-file)))
			((= typ +itunes-ilst-data+)
			 (log-mp4-atom "~a is an ilst data atom, read it all in" (as-string typ))
			 (setf atom (make-instance 'mp4-ilst-generic-data-atom :atom-parent-type atom-parent-type :atom-size siz :atom-type typ :atom-file-position pos :mp4-file mp4-file)))
			(t
			 (log-mp4-atom "~a is an atom we are NOT interested in; seek past it" (as-string typ))
			 (setf atom (make-instance 'mp4-atom :atom-size siz :atom-type typ :atom-file-position pos))
			 (stream-seek mp4-file (- siz 8) :current)))
	  (log-mp4-atom "returning ~a" (vpprint atom nil))
	  atom)))

(defparameter *pprint-mp4-atom* nil
  "Controls whether we pretty print an atom")

(defmethod print-object ((me mp4-atom) stream)
  (if (null *pprint-mp4-atom*)
	  (call-next-method)
	  ;; else
	  (format stream "~a" (with-output-to-string (s)
							(with-slots (atom-children atom-file-position atom-size atom-type) me
							  (format s "Atom <~a> @ ~:d of size ~:d and child count of ~d"
									  (as-string atom-type) atom-file-position atom-size (size atom-children)))
							(if (typep me 'mp4-ilst-generic-data-atom)
								(with-slots (atom-version atom-flags atom-value atom-type atom-parent-type) me
								  (format s " having ilst fields: atom-parent-type = ~a, verison = ~d, flags = ~x, data = ~x"
										  (as-string atom-parent-type) atom-version atom-flags atom-value)))))))

(defmethod vpprint ((me mp4-atom) stream &key (indent 0))
  "set *pprint-mp4-atom* to get pretty printing and call print-object via format"
  (let ((*pprint-mp4-atom* t))
	(format stream "~vt~a" (* indent 1) me)))

(defclass mp4-unhandled-data ()
  ((blob :accessor blob :initarg :blob :initform nil))
  (:documentation "abstraction for a 'blob' of data we don't want to or can't parse"))

(defparameter *pprint-max-array-len* 10
  "Controls how long an array (atom-value, typically) we will print in pprint-atom")

(defmethod print-object ((me mp4-unhandled-data) stream)
  "Print a 'blob' (unstructered data), limiting it to no more than *PPRINT-MAX-ARRAY-LEN* octets"
  (let* ((len (length (slot-value me 'blob)))
		 (print-len (min len *pprint-max-array-len*))
		 (printable-array (make-array print-len :displaced-to (slot-value me 'blob))))
	(format stream "[~:d of ~:d bytes] <~x>" print-len len printable-array)))

;;;;;;;;;;;;;;;;;;;; A collection of atoms (siblings) ;;;;;;;;;;;;;;;;;;;;
(defclass atom-collection ()
  ((atoms :accessor atoms :initform nil))
  (:documentation "A collection of sibling atoms"))

(defun make-mp4-atom-collection () (make-instance 'atom-collection))

(defmethod add ((me atom-collection) new-atom)
  "Adds new atom to the *end* (need to keep them in order we found them in the file) of this collection"
  (log5:with-context "add-atom-collection"
	(with-slots (atoms) me
	  (log-mp4-atom "adding ~a to atom collection: ~a" new-atom atoms)
	  (setf atoms (append atoms (list new-atom)))
	  (log-mp4-atom "collection now: ~a" atoms))))

(defmethod size ((me atom-collection))
  "Returns the number of atoms in this collection"
  (length (slot-value me 'atoms)))

(defmethod map-mp4-atom ((me atom-collection) &key (func nil) (depth nil))
  "Given a collection of atoms, call map-mp4-atom for each one"
  (log5:with-context "map-mp4-atom(collection)"
	(log-mp4-atom "mapping collection: ~a" (slot-value me 'atoms))
	(dolist (a (slot-value me 'atoms))
	  (map-mp4-atom a :func func :depth depth))))

(defun is-valid-m4-file (mp4-file)
  "Make sure this is an MP4 file.  Quick check: is first atom (at file-offset 4) == FSTYP?"
  (stream-seek mp4-file 0 :start)
  (let* ((size (stream-read-u32 mp4-file))
		 (header (stream-read-u32 mp4-file)))
	(declare (ignore size))
	(stream-seek mp4-file 0 :start)
	(= header +m4-ftyp+)))

(defun find-mp4-atoms (mp4-file)
  "Given a valid MP4 file mp4-file, look for the 'right' atoms and return them.
The 'right' atoms are those in *atoms-of-interest*"
  (log5:with-context "find-mp4-atoms"
	(when (not (is-valid-m4-file mp4-file))
	  (error 'mp4-atom-condition :location "find-mp4-atoms" :object mp4-file :message "is not an mp4-file" ))

	(log-mp4-atom "before read-file loop, file-position = ~:d, end = ~:d" (stream-seek mp4-file 0 :current) (stream-size mp4-file))

	(setf (mp4-atoms mp4-file) (make-mp4-atom-collection))
	(do ((new-atom))
		((> (+ 8 (stream-seek mp4-file 0 :current)) (stream-size mp4-file)))
	  (log-mp4-atom "top of read-file loop, current file-position = ~:d, end = ~:d" (stream-seek mp4-file 0 :current) (stream-size mp4-file))
	  (setf new-atom (make-mp4-atom mp4-file))
	  (when new-atom (add (mp4-atoms mp4-file) new-atom)))

	(log-mp4-atom "returning atom-collection of size ~d" (size (mp4-atoms mp4-file)))))


(defmethod map-mp4-atom ((me mp4-atom) &key (func nil) (depth nil))
  "traverse all atoms under a given atom"
  (log5:with-context "map-mp4-atom(single)"
	(labels ((_indented-atom (atom depth)
			   (format t "~a~%" (vpprint atom nil :indent (if (null depth) 0 depth)))))
	  (with-slots (atom-type atom-children) me
		(log-mp4-atom "Begining traversal with ~a, I have ~d children" (as-string atom-type) (size atom-children))
		(when (null func)
		  (setf func #'_indented-atom))
		(funcall func me depth)
		(map-mp4-atom atom-children :func func :depth (if (null depth) nil (+ 1 depth)))))))

(defmethod traverse ((me mp4-atom) path)
  "Used in finding nested atoms.
Given an atom and a path, if atom-type matches first element of path, then we've found our match."
  (log5:with-context "traverse-atom"
	(log-mp4-atom "traverse-atom entered with ~a ~a" (atom-type me) path)
	(cond ((null path)
		   (error "Path exhausted in travese atom")	; don't think this can happen?
		   nil)
		  ((= (atom-type me) (first path))
		   (log-mp4-atom "current path matches thus far ~a ~a" (atom-type me) path)
		   (cond
			 ((= 1 (length path))
			  (log-mp4-atom "length of path is 1, so found!")
			  (return-from traverse me)))))

	(log-mp4-atom "Current path doesn't match ~a ~a" (atom-type me) path)
	nil))

(defmethod traverse ((me atom-collection) path)
  "Used in finding nested atoms. Seach the collection and if we find a match with first of path,
call traverse atom (unless length of path == 1, in which case, we've found out match)"
  (log5:with-context "traverse-atom-collection"
	(log-mp4-atom "entering with ~a ~a" me path)
	(dolist (sibling (atoms me))	; cleaner than using map-mp4-atom, but still a kludge
	  (with-slots (atom-type atom-children) sibling
		(log-mp4-atom "looking at ~x::~x" atom-type (first path))
		(when (= atom-type (first path))
		  (cond
			((= 1 (length path))
			 (log-mp4-atom "Found ~a" sibling)
			 (return-from traverse sibling))
			(t
			 (log-mp4-atom "path matches, calling traverse atom with ~a, ~a" atom-children (rest path))
			 (let ((found (traverse atom-children (rest path))))
			   (if found (return-from traverse found))))))))
	(log-mp4-atom "Looked at all, found nothing")
	nil))

(defmethod tag-get-value (atoms node)
  "Helper function to extract text from atom's data atom"
  (let ((atom (traverse atoms
						(list +mp4-atom-moov+ +mp4-atom-udta+ +mp4-atom-meta+ +mp4-atom-ilst+ node +itunes-ilst-data+))))
	(if atom
		(atom-value atom)
		nil)))

(defun mp4-show-raw-tag-atoms (mp4-file-stream)
  (map-mp4-atom (mp4-atom::traverse  (mp4-atoms mp4-file-stream) (list +mp4-atom-moov+ +mp4-atom-udta+ +mp4-atom-meta+ +mp4-atom-ilst+)))))

