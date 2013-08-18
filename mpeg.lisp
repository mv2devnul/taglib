;;; -*- Mode: Lisp;  show-trailing-whitespace: t; Base: 10; indent-tabs: nil; Syntax: ANSI-Common-Lisp; Package: MPEG; -*-
;;; Copyright (c) 2013, Mark VandenBrink. All rights reserved.
(in-package #:mpeg)

(log5:defcategory cat-log-mpeg-frame)
(defmacro log-mpeg-frame (&rest log-stuff) `(log5:log-for (cat-log-mpeg-frame) ,@log-stuff))

(define-condition mpeg-condition () 
  ((location :initarg :location :reader location :initform nil)
   (object   :initarg :object   :reader object   :initform nil)
   (messsage :initarg :message  :reader message  :initform "Undefined Condition"))
  (:report (lambda (condition stream) 
			 (format stream "MP3 condition at location <~a> with object <~a>: message<~a>"
					 (location condition) (object condition) (message condition)))))

(define-condition mpeg-bad-header (mpeg-condition) ())

(defconstant +sync-word+  #xffe0)

(defconstant +mpeg-2.5+   0)
(defconstant +v-reserved+ 1)
(defconstant +mpeg-2+     2)
(defconstant +mpeg-1+     3)

(defun valid-version (version)
  (or ;; can't deal with 2.5's yet (= (the fixnum +mpeg-2.5+) (the fixnum version))
	  (= (the fixnum +mpeg-2+) (the fixnum version))
	  (= (the fixnum +mpeg-1+) (the fixnum version))))

(defun get-mpeg-version-string (version) (nth version '("MPEG 2.5" "Reserved" "MPEG 2" "MPEG 1")))

(defconstant +layer-reserved+  0)
(defconstant +layer-3+         1)
(defconstant +layer-2+		   2)
(defconstant +layer-1+		   3)

(defun valid-layer (layer)
  (or (= (the fixnum +layer-3+) (the fixnum layer))
	  (= (the fixnum +layer-2+) (the fixnum layer))
	  (= (the fixnum +layer-1+) (the fixnum layer))))

(defun get-layer-string (layer) (nth layer '("Reserved" "Layer III" "Layer II" "Layer I")))

(defconstant +channel-mode-stereo+ 0)
(defconstant +channel-mode-joint+  1)
(defconstant +channel-mode-dual+   2)
(defconstant +channel-mode-mono+   3)
(defun get-channel-mode-string (mode)  (nth mode '("Stereo" "Joint" "Dual" "Mono")))

(defconstant +emphasis-none+     0)
(defconstant +emphasis-50-15+    1)
(defconstant +emphasis-reserved+ 2)
(defconstant +emphasis-ccit+     3)
(defun get-emphasis-string (e)   (nth e '("None" "50/15 ms" "Reserved" "CCIT J.17")))
(defun valid-emphasis (e) (or (= (the fixnum e) (the fixnum +emphasis-none+))
							  (= (the fixnum e) (the fixnum +emphasis-50-15+))
							  (= (the fixnum e) (the fixnum +emphasis-ccit+))))

(defconstant +mode-extension-0+ 0)
(defconstant +mode-extension-1+ 1)
(defconstant +mode-extension-2+ 2)
(defconstant +mode-extension-3+ 3)
(defun get-mode-extension-string (channel-mode layer mode-extension)
  (if (not (= channel-mode +channel-mode-joint+))
	  ""
	  (if (or (= layer +layer-1+)
			  (= layer +layer-2+))
		  (format nil "Bands ~[4~;8~;12~;16~] to 31" mode-extension)
		  (format nil "Intensity Stereo: ~[off~;on~], MS Stereo: ~[off~;on~]" (ash mode-extension -1) (logand mode-extension 1)))))
  
(defun get-samples-per-frame (version layer)
  (cond ((= (the fixnum layer) (the fixnum +layer-1+)) 384)
		((= (the fixnum layer) (the fixnum +layer-2+)) 1152)
		((= (the fixnum layer) (the fixnum +layer-3+))
		 (cond ((= (the fixnum version) +mpeg-1+) 1152)
			   ((or (= (the fixnum version) (the fixnum +mpeg-2+))
					(= (the fixnum version) (the fixnum +mpeg-2.5+))) 576)))))

(defclass frame ()
  ((pos		       :accessor pos :initarg :pos)
   (b-array        :accessor b-array :initarg :b-array)
   (samples        :accessor samples :initarg :samples)
   (sync           :accessor sync :initarg :sync)
   (version        :accessor version :initarg :version)
   (layer          :accessor layer :initarg :layer)
   (protection     :accessor protection :initarg :protection)
   (bit-rate       :accessor bit-rate :initarg :bit-rate)
   (sample-rate    :accessor sample-rate :initarg :sample-rate)
   (padded         :accessor padded :initarg :padded)
   (private        :accessor private :initarg :private)
   (channel-mode   :accessor channel-mode :initarg :channel-mode)
   (mode-extension :accessor mode-extension :initarg :mode-extension)
   (copyright      :accessor copyright :initarg :copyright)
   (original       :accessor original :initarg :original)
   (emphasis       :accessor emphasis :initarg :emphasis)
   (size           :accessor size :initarg :size)
   (vbr	           :accessor vbr :initarg :vbr)
   (payload	       :accessor payload :initarg :payload))
  (:default-initargs :pos nil :b-array nil :samples 0 :sync 0 :version 0 :layer 0 :protection 0 :bit-rate 0
					 :sample-rate 0 :padded 0 :private 0 :channel-mode 0 :mode-extension 0
					 :copyright 0 :original 0 :emphasis 0 :size nil :vbr nil :payload nil))

(defmacro with-frame-slots ((instance) &body body)
  `(with-slots (pos b-array samples sync version layer protection bit-rate sample-rate 
					padded private channel-mode mode-extension copyright  
					original emphasis size vbr payload) ,instance
	 ,@body))

(let ((bit-array-table
	   (make-array '(14 5) :initial-contents
				   '((32   32  32  32   8)
					 (64   48  40  48  16)
					 (96   56  48  56  24)
					 (128  64  56  64  32)
					 (160  80  64  80  40)
					 (192  96  80  96  48)
					 (224 112  96 112  56)
					 (256 128 112 128  64)
					 (288 160 128 144  80)
					 (320 192 160 160  96)
					 (352 224 192 176 112)
					 (384 256 224 192 128)
					 (416 320 256 224 144)
					 (448 384 320 256 160)))))

  (defun valid-bit-rate-index (br-index)
	(and (> (the fixnum br-index) 0) (< (the fixnum br-index) 15)))

  (defun get-bit-rate (version layer bit-rate-index)
	(log5:with-context "get-bit-rate"
	  (log-mpeg-frame "version = ~d, layer = ~d, bit-rate-index = ~d" version layer bit-rate-index)
	  (let ((row (1- bit-rate-index))
			(col (cond ((= (the fixnum version) (the fixnum +mpeg-1+))
						(cond ((= (the fixnum layer) (the fixnum +layer-1+)) 0)
							  ((= (the fixnum layer) (the fixnum +layer-2+)) 1)
							  ((= (the fixnum layer) (the fixnum +layer-3+)) 2)
							  (t nil)))
					   ((= (the fixnum version) (the fixnum +mpeg-2+))
						(cond ((= (the fixnum layer) (the fixnum +layer-1+)) 3)
							  ((= (the fixnum layer) (the fixnum +layer-2+)) 4)
							  ((= (the fixnum layer) (the fixnum +layer-3+)) 4)
							  (t nil)))
					   (t (error "don't support MPEG 2.5 yet")))))

		(log-mpeg-frame "version = ~d, row = ~d, col = ~d" version row col)
		(if (or (null col) (< row 0) (> row 14))
			nil
			(let ((ret (* 1000 (aref bit-array-table row col))))
			  (log-mpeg-frame "returning ~:d" ret)
			  ret))))))

(defun valid-sample-rate-index (sr-index)
  (and (>= (the fixnum sr-index) 0)
	   (<  (the fixnum sr-index) 3)))

(defun get-sample-rate (version sr-index)
  (cond ((= (the fixnum version) (the fixnum +mpeg-1+))
		 (case (the fixnum sr-index) (0 44100) (1 48000) (2 32000)))
		((= (the fixnum version) (the fixnum +mpeg-2+))
		 (case (the fixnum sr-index) (0 22050) (1 24000) (2 16000)))
		(t nil)))

(defun get-frame-size (version layer bit-rate sample-rate padded)
  (truncate (float (cond ((= (the fixnum layer) (the fixnum +layer-1+))
						  (* 4 (+ (/ (* 12 bit-rate) sample-rate) padded)))
						 ((= (the fixnum layer) (the fixnum +layer-2+))
						  (+ (* 144 (/ bit-rate sample-rate)) padded))
						 ((= (the fixnum layer) (the fixnum +layer-3+))
						  (if (= (the fixnum version) (the fixnum +mpeg-1+))
							  (+ (* 144 (/ bit-rate sample-rate)) padded)
							  (+ (* 72  (/ bit-rate sample-rate)) padded)))))))

(defmethod load-frame ((me frame) &key instream (read-payload nil))
  (log5:with-context "load-frame"
	(with-frame-slots (me)
	  (when (null b-array)				; has header already been read in?
		(setf pos (stream-seek instream 0 :current))
		(setf b-array (stream-read-sequence instream 4)))

	  (if (parse-header me)
		  (progn
			(log-mpeg-frame "header parsed ok")
			(setf size (get-frame-size version layer bit-rate sample-rate padded))
			(when read-payload
			  (setf payload (stream-read-sequence instream (- size 4))))
			t)
		  (progn
			(log-mpeg-frame "header didn't parse!")
			nil)))))

(defmethod parse-header ((me frame))
  (log5:with-context "parse-header"
	(with-frame-slots (me)

	  (setf (ldb (byte 8 8) sync) (aref b-array 0))
	  (setf (ldb (byte 3 5) sync) (ldb (byte 3 5) (aref b-array 1)))
	  (when (not (= sync +sync-word+))
		(return-from parse-header nil))

	  (setf version (ldb (byte 2 3) (aref b-array 1)))
	  (when (not (valid-version version))
		(log-mpeg-frame "bad version ~d" version)
		(return-from parse-header nil))

	  (setf layer (ldb (byte 2 1) (aref b-array 1)))
	  (when (not (valid-layer layer))
		(log-mpeg-frame "bad layer ~d" layer)
		(return-from parse-header nil))

	  (setf protection (ldb (byte 1 0) (aref b-array 1)))
	  (setf samples (get-samples-per-frame version layer))

	  (let ((br-index (the fixnum (ldb (byte 4 4) (aref b-array 2)))))
		(when (not (valid-bit-rate-index br-index))
		  (log-mpeg-frame "bad bit-rate index ~d" br-index)
		  (return-from parse-header nil))
		(setf bit-rate (get-bit-rate version layer br-index)))

	  (let ((sr-index (the fixnum (ldb (byte 2 2) (aref b-array 2)))))
		(when (not (valid-sample-rate-index sr-index))
		  (log-mpeg-frame "bad sample-rate index ~d" sr-index)
		  (return-from parse-header nil))
		(setf sample-rate (get-sample-rate version sr-index)))

	  (setf padded (ldb (byte 1 1) (aref b-array 2)))
	  (setf private (ldb (byte 1 0) (aref b-array 2)))

	  (setf channel-mode (ldb (byte 2 6) (aref b-array 3)))
	  (setf mode-extension (ldb (byte 2 4) (aref b-array 3)))
	  (setf copyright (ldb (byte 1 3) (aref b-array 3)))
	  (setf original (ldb (byte 1 2) (aref b-array 3)))
	  (setf emphasis (ldb (byte 2 0) (aref b-array 3)))
	  (when (not (valid-emphasis emphasis))
		(log-mpeg-frame "bad emphasis ~d" emphasis)
		(return-from parse-header nil))

	  (log-mpeg-frame "good parse: ~a" me)
	  t)))

(defmethod vpprint ((me frame) stream)
  (with-frame-slots (me)
	(format stream "MPEG Frame: position in file = ~:d, header in (hex) bytes = ~x, size = ~d, sync word = ~x, " pos b-array size sync)
	(when vbr
	  (format stream "~&vbr-info: ~a~%" vbr))
	(format stream "version = ~a, layer = ~a, crc protected? = ~[yes~;no~], bit-rate = ~:d bps, sampling rate = ~:d bps, padded? = ~[no~;yes~], private bit set? = ~[no~;yes~], channel mode = ~a, "
			(get-mpeg-version-string version) (get-layer-string layer)
			protection bit-rate sample-rate padded private (get-channel-mode-string channel-mode))
	(format stream "mode extension = ~a, copyrighted? = ~[no~;yes~], original? = ~[no~;yes~], emphasis = ~a"
			(get-mode-extension-string channel-mode layer mode-extension) copyright original (get-emphasis-string emphasis))
	(when payload
	  (format stream "~%frame payload[~:d] = ~a~%" (length payload) payload))))

(defclass vbr-info ()
  ((tag    :accessor tag :initarg :tag)
   (flags  :accessor flags :initarg :flags)
   (frames :accessor frames :initarg :frames)
   (bytes  :accessor bytes :initarg :bytes)
   (tocs   :accessor tocs :initarg :tocs)
   (scale  :accessor scale :initarg :scale))
  (:default-initargs :tag nil :flags 0 :frames nil :bytes nil :tocs nil :scale nil))

(defmacro with-vbr-info-slots ((instance) &body body)
  `(with-slots (tag flags frames bytes tocs scale) ,instance
	 ,@body))

(defconstant +vbr-frames+  1)
(defconstant +vbr-bytes+   2)
(defconstant +vbr-tocs+    4)
(defconstant +vbr-scale+   8)

(defun get-side-info-size (version channel-mode)
  (cond ((= (the fixnum version) (the fixnum +mpeg-1+))
		 (cond ((= (the fixnum channel-mode) (the fixnum +channel-mode-mono+)) 17)
			   (t 32)))
		(t (cond ((= (the fixnum channel-mode) (the fixnum +channel-mode-mono+)) 9)
				 (t 17)))))

(defmethod check-vbr ((me frame))
  (log5::with-context "check-vbr"
	(with-frame-slots (me)
	  (let ((i (get-side-info-size version channel-mode)))
		(log-mpeg-frame "array index = ~d, payload size = ~d" i (length payload))
		(when (or (and (= (aref payload (+ i 0)) (char-code #\X))
					   (= (aref payload (+ i 1)) (char-code #\i))
					   (= (aref payload (+ i 2)) (char-code #\n))
					   (= (aref payload (+ i 3)) (char-code #\g)))
				  (and (= (aref payload (+ i 0)) (char-code #\I))
					   (= (aref payload (+ i 1)) (char-code #\n))
					   (= (aref payload (+ i 2)) (char-code #\f))
					   (= (aref payload (+ i 3)) (char-code #\o))))

		  (log-mpeg-frame "found xing/info: ~c ~c ~c ~c"
					  (code-char (aref payload (+ i 0)))
					  (code-char (aref payload (+ i 1)))
					  (code-char (aref payload (+ i 2)))
					  (code-char (aref payload (+ i 3))))

		  (setf vbr (make-instance 'vbr-info))
		  (let ((v (make-mem-stream (payload me))))
			(stream-seek v i :start)			; seek to xing/info info
			(setf (tag vbr)   (stream-read-iso-string-with-len v 4))
			(setf (flags vbr) (stream-read-u32 v))
			(when (logand (flags vbr) +vbr-frames+)
			  (setf (frames vbr) (stream-read-u32 v))
			  (if (= 0 (frames vbr)) (warn-user "warning Xing/Info header flags has FRAMES set, but field is zero")))
			(when (logand (flags vbr) +vbr-bytes+)
			  (setf (bytes vbr) (stream-read-u32 v))
			  (if (= 0 (bytes vbr)) (warn-user "warning Xing/Info header flags has BYTES set, but field is zero")))
			(when (logand (flags vbr) +vbr-tocs+)
			  (setf (tocs vbr) (stream-read-sequence v 100)))
			(when (logand (flags vbr) +vbr-scale+)
			  (setf (scale vbr) (stream-read-u32 v)))
			(log-mpeg-frame "vbr-info = ~a" (vpprint vbr nil))))))))

(defmethod vpprint ((me vbr-info) stream)
  (with-vbr-info-slots (me)
	(format stream "tag = ~a, flags = 0x~x, frames = ~:d, bytes = ~:d, tocs = ~d, scale = ~d, "
			tag flags frames bytes tocs scale)))

(defun find-first-sync (in)
  (log5:with-context "find-first-sync"

	(log-mpeg-frame "Looking for first sync, begining at file position ~:d" (stream-seek in 0 :current))
	(let ((b-array (make-octets 4))
		  (pos))

	  (handler-case 
		  ;;
		  ;; loop through entire file if we have to
		  ;; XXX question: if we read FF E from the file (two bytes), but the
		  ;; parse fails (i.e. a false sync), do we skip forward, or try to parse
		  ;; the second byte as the FF?
		  (loop 
			 (setf pos (stream-seek in 0 :current))
			 (setf (aref b-array 0) (stream-read-u8 in))
			 (when (= (aref b-array 0) #xff)
			   (setf (aref b-array 1) (stream-read-u8 in))
			   (when (= (logand (aref b-array 1) #xe0) #xe0)
				 (log-mpeg-frame "Potential sync bytes at ~:d: <~x>" pos b-array)
				 (setf (aref b-array 2) (stream-read-u8 in))
				 (setf (aref b-array 3) (stream-read-u8 in))

				 (let ((hdr (make-instance 'frame :b-array b-array :pos pos)))
				   (if (load-frame hdr :instream in :read-payload t)
					   (progn
						 (check-vbr hdr)
						 (log-mpeg-frame "Valid header being returned: ~a" hdr)
						 (return-from find-first-sync hdr))
					   (progn
						 (log-mpeg-frame "hdr wasn't valid: ~a" hdr)))))))
		(end-of-file (c) (progn 
						   (log-mpeg-frame "got a condition while looking for first sync: ~a" c)
						   (error c))))
	  nil)))

(defmethod next-frame ((me frame) &key instream read-payload)
  (log5:with-context "next-frame"
	(let ((nxt-frame (make-instance 'frame)))
	  (when (not (payload me))
		(log-mpeg-frame "no payload in current frame, skipping from ~:d forward ~:d bytes"
						(stream-seek instream 0 :current)
						(- (size me) 4) :current)
		(stream-seek instream (- (size me) 4) :current))

	  (if (load-frame nxt-frame :instream instream :read-payload read-payload)
		  nxt-frame
		  nil))))

(defun map-frames (in func &key (start-pos nil) (read-payload nil) (max nil))
  (log5:with-context "next-frame"
	(log-mpeg-frame "mapping frame, start pos ~:d" start-pos)

	(when start-pos
	  (stream-seek in start-pos :start))

	(loop 
	   for max-frames = (if max max most-positive-fixnum)
	   for count = 0 then (incf count)
	   for frame = (find-first-sync in) then (next-frame frame :instream in :read-payload read-payload)
	    while (and frame (< count max-frames)) do
		 (log-mpeg-frame "At pos ~:d, dispatching function" (pos frame))
		 (funcall func frame))))

(defun get-mpeg-bit-rate-exhaustive (in)
  (let ((n-frames 0)
		(total-len 0)
		(last-bit-rate nil)
		(bit-rate-total 0)
		(vbr nil))
	(map-frames in (lambda (f)
					 (incf n-frames)
					 (incf total-len (float (/ (samples f) (sample-rate f))))
					 (incf bit-rate-total (bit-rate f))
					 (if (null last-bit-rate)
						 (setf last-bit-rate (bit-rate f))
						 (progn
						   (when (not (= last-bit-rate (bit-rate f)))
							 (setf vbr t))
						   (setf last-bit-rate (bit-rate f)))))
				:read-payload nil)
	(if (or (zerop n-frames) (zerop bit-rate-total))
		(values nil nil nil) 
		(values vbr (float (/ bit-rate-total n-frames)) total-len))))

(defun get-mpeg-bit-rate-ff (in)
  (let ((ff (find-first-sync in)))
	(if (not ff)
		(return-from get-mpeg-bit-rate-ff (values nil nil)))
	(if (vbr ff)
		(let* ((len (float (* (frames (vbr ff)) (/ (samples ff) (sample-rate ff)))))
			   (br (float (/ (* 8 (bytes (vbr ff)) ) len))))
		  (values t br len))
		(values nil nil nil))))

(defclass mpeg-audio-info ()
  ((is-vbr      :accessor is-vbr :initarg :is-vbr :initform nil)
   (bit-rate    :accessor bit-rate :initarg :bit-rate :initform nil)
   (sample-rate :accessor sample-rate :initarg :sample-rate :initform nil)
   (len         :accessor len :initarg :len :initform nil)
   (version     :accessor version :initarg :version :initform nil)
   (layer       :accessor layer :initarg :layer :initform nil)))

(defmethod vpprint ((me mpeg-audio-info) stream)
  (with-slots (is-vbr sample-rate  bit-rate len version layer) me
	(format stream "~a, ~a, ~:[CBR,~;VBR,~] sample rate: ~:d Hz, bit rate: ~:d Kbps, duration: ~:d:~2,'0d"
			(get-mpeg-version-string version)
			(get-layer-string layer)
			is-vbr
			sample-rate
			(round (/ bit-rate 1000))
			(floor (/ len 60)) (round (mod len 60)))))

(defun get-mpeg-audio-info (in &key (max-frames nil))
  "Get MPEG Layer 3 audio information."
  (log5:with-context "get-mpeg-audio-info"
	(let ((pos (stream-seek in 0 :current))
		  (first-frame (find-first-sync in))
		  (info (make-instance 'mpeg-audio-info)))

	  (log-mpeg-frame "search for first frame yielded ~a" first-frame)
	  (when (null first-frame)
		(return-from get-mpeg-audio-info nil))

	  (with-slots (is-vbr sample-rate bit-rate len version layer) info
		(setf version (version first-frame))
		(setf layer (layer first-frame))
		(setf sample-rate (sample-rate first-frame))
		(if (vbr first-frame)
			(progn
			  (log-mpeg-frame "found Xing/Info header")
			  (setf is-vbr t)
			  (setf len (float (* (frames (vbr first-frame)) (/ (samples first-frame) (sample-rate first-frame)))))
			  (setf bit-rate  (float (/ (* 8 (bytes (vbr first-frame)) ) len))))
			(let ((n-frames 0)
				  (total-len 0)
				  (last-bit-rate nil)
				  (bit-rate-total 0)
				  (vbr nil))
			  (stream-seek in pos :start)
			  (log-mpeg-frame "no Xing/Info, so mapping frames")
			  (map-frames in (lambda (f)
							   (incf n-frames)
							   (incf total-len (float (/ (samples f) (sample-rate f))))
							   (incf bit-rate-total (bit-rate f))
							   (if (null last-bit-rate)
								   (setf last-bit-rate (bit-rate f))
								   (progn
									 (when (not (= last-bit-rate (bit-rate f)))
									   (setf vbr t))
									 (setf last-bit-rate (bit-rate f)))))
						  :read-payload nil :max max-frames)
			  (if (or (< n-frames 10) (zerop bit-rate-total))
				  (progn
					(log-mpeg-frame "couldn't get audio-info: only got ~d frames" n-frames)
					(return-from get-mpeg-audio-info nil))
				  (progn
					(setf is-vbr vbr)
					(setf len total-len)
					(setf bit-rate (float (/ bit-rate-total n-frames))))))))
	  info)))


#|

if we have a xing header, we use
  num-frames, num-bytes from xing header and
  sample-rate and layer info (to get num samples/sec---for layer 3 its 1152)

then:

  length in seconds is  =  num-frames * (1152 / sample-rate)
  bit-rate is = (8 * num-bytes) / length in seconds, then divide by 1000 to get kbits/sec
---------

|#
