;;; -*- Mode: Lisp;  show-trailing-whitespace: t; Base: 10; indent-tabs: nil; Syntax: ANSI-Common-Lisp; Package: MPEG; -*-
;;; Copyright (c) 2013, Mark VandenBrink. All rights reserved.

;;; Parsing MPEG audio frames.  See http://www.datavoyage.com/mpgscript/mpeghdr.htm for format of a frame.
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

(defconstant +sync-word+  #x7ff "NB: this is 11 bits so as to be able to recognize V2.5")

;;; the versions
(defconstant +mpeg-2.5+   0)
(defconstant +v-reserved+ 1)
(defconstant +mpeg-2+     2)
(defconstant +mpeg-1+     3)

(defun valid-version (version)
  (declare #.utils:*standard-optimize-settings*)
  (or ;; can't deal with 2.5's yet (= (the fixnum +mpeg-2.5+) (the fixnum version))
      (= (the fixnum +mpeg-2+) (the fixnum version))
      (= (the fixnum +mpeg-1+) (the fixnum version))))

(defun get-mpeg-version-string (version)
  (declare #.utils:*standard-optimize-settings*)
  (nth version '("MPEG 2.5" "Reserved" "MPEG 2" "MPEG 1")))

;;; the layers
(defconstant +layer-reserved+  0)
(defconstant +layer-3+         1)
(defconstant +layer-2+         2)
(defconstant +layer-1+         3)

(defun valid-layer (layer)
  (declare #.utils:*standard-optimize-settings*)
  (or (= (the fixnum +layer-3+) (the fixnum layer))
      (= (the fixnum +layer-2+) (the fixnum layer))
      (= (the fixnum +layer-1+) (the fixnum layer))))

(defun get-layer-string (layer)
  (declare #.utils:*standard-optimize-settings*)
  (nth layer '("Reserved" "Layer III" "Layer II" "Layer I")))

;;; the modes
(defconstant +channel-mode-stereo+ 0)
(defconstant +channel-mode-joint+  1)
(defconstant +channel-mode-dual+   2)
(defconstant +channel-mode-mono+   3)
(defun get-channel-mode-string (mode)
  (declare #.utils:*standard-optimize-settings*)
  (nth mode '("Stereo" "Joint" "Dual" "Mono")))

;;; the emphases
(defconstant +emphasis-none+     0)
(defconstant +emphasis-50-15+    1)
(defconstant +emphasis-reserved+ 2)
(defconstant +emphasis-ccit+     3)

(defun get-emphasis-string (e)
  (declare #.utils:*standard-optimize-settings*)
  (nth e '("None" "50/15 ms" "Reserved" "CCIT J.17")))

(defun valid-emphasis (e)
  (declare #.utils:*standard-optimize-settings*)
  (or (= (the fixnum e) (the fixnum +emphasis-none+))
      (= (the fixnum e) (the fixnum +emphasis-50-15+))
      (= (the fixnum e) (the fixnum +emphasis-ccit+))))

;;; the modes
(defconstant +mode-extension-0+ 0)
(defconstant +mode-extension-1+ 1)
(defconstant +mode-extension-2+ 2)
(defconstant +mode-extension-3+ 3)
(defun get-mode-extension-string (channel-mode layer mode-extension)
  (declare #.utils:*standard-optimize-settings*)
  (if (not (= channel-mode +channel-mode-joint+))
      ""
      (if (or (= layer +layer-1+)
              (= layer +layer-2+))
          (format nil "Bands ~[4~;8~;12~;16~] to 31" mode-extension)
          (format nil "Intensity Stereo: ~[off~;on~], MS Stereo: ~[off~;on~]" (ash mode-extension -1) (logand mode-extension 1)))))

(defun get-samples-per-frame (version layer)
  (declare #.utils:*standard-optimize-settings*)
  (cond ((= (the fixnum layer) (the fixnum +layer-1+)) 384)
        ((= (the fixnum layer) (the fixnum +layer-2+)) 1152)
        ((= (the fixnum layer) (the fixnum +layer-3+))
         (cond ((= (the fixnum version) +mpeg-1+) 1152)
               ((or (= (the fixnum version) (the fixnum +mpeg-2+))
                    (= (the fixnum version) (the fixnum +mpeg-2.5+))) 576)))))

(defclass frame ()
  ((pos            :accessor pos            :initarg :pos)
   (hdr-u32        :accessor hdr-u32        :initarg :hdr-u32)
   (samples        :accessor samples        :initarg :samples)
   (sync           :accessor sync           :initarg :sync)
   (version        :accessor version        :initarg :version)
   (layer          :accessor layer          :initarg :layer)
   (protection     :accessor protection     :initarg :protection)
   (bit-rate       :accessor bit-rate       :initarg :bit-rate)
   (sample-rate    :accessor sample-rate    :initarg :sample-rate)
   (padded         :accessor padded         :initarg :padded)
   (private        :accessor private        :initarg :private)
   (channel-mode   :accessor channel-mode   :initarg :channel-mode)
   (mode-extension :accessor mode-extension :initarg :mode-extension)
   (copyright      :accessor copyright      :initarg :copyright)
   (original       :accessor original       :initarg :original)
   (emphasis       :accessor emphasis       :initarg :emphasis)
   (size           :accessor size           :initarg :size)
   (vbr            :accessor vbr            :initarg :vbr)
   (payload        :accessor payload        :initarg :payload))
  (:documentation "Data in and associated with an MPEG audio frame.")
  (:default-initargs :pos nil :hdr-u32 nil :samples 0 :sync 0 :version 0 :layer 0 :protection 0 :bit-rate 0
                     :sample-rate 0 :padded 0 :private 0 :channel-mode 0 :mode-extension 0
                     :copyright 0 :original 0 :emphasis 0 :size nil :vbr nil :payload nil))

(defmacro with-frame-slots ((instance) &body body)
  `(with-slots (pos hdr-u32 samples sync version layer protection bit-rate sample-rate
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
    (declare #.utils:*standard-optimize-settings*)
    (and (> (the fixnum br-index) 0) (< (the fixnum br-index) 15)))

  (defun get-bit-rate (version layer bit-rate-index)
    (declare #.utils:*standard-optimize-settings*)
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
  (declare #.utils:*standard-optimize-settings*)
  (and (>= (the fixnum sr-index) 0)
       (<  (the fixnum sr-index) 3)))

(defun get-sample-rate (version sr-index)
  (declare #.utils:*standard-optimize-settings*)
  (cond ((= (the fixnum version) (the fixnum +mpeg-1+))
         (case (the fixnum sr-index) (0 44100) (1 48000) (2 32000)))
        ((= (the fixnum version) (the fixnum +mpeg-2+))
         (case (the fixnum sr-index) (0 22050) (1 24000) (2 16000)))
        (t nil)))

(defun get-frame-size (version layer bit-rate sample-rate padded)
  (declare #.utils:*standard-optimize-settings*)
  (truncate (float (cond ((= (the fixnum layer) (the fixnum +layer-1+))
                          (* 4 (+ (/ (* 12 bit-rate) sample-rate) padded)))
                         ((= (the fixnum layer) (the fixnum +layer-2+))
                          (+ (* 144 (/ bit-rate sample-rate)) padded))
                         ((= (the fixnum layer) (the fixnum +layer-3+))
                          (if (= (the fixnum version) (the fixnum +mpeg-1+))
                              (+ (* 144 (/ bit-rate sample-rate)) padded)
                              (+ (* 72  (/ bit-rate sample-rate)) padded)))))))

(defmethod load-frame ((me frame) &key instream (read-payload nil))
  "Load an MPEG frame from current file position.  If READ-PAYLOAD is set, read in frame's content."
  (declare #.utils:*standard-optimize-settings*)
  (declare #.utils:*standard-optimize-settings*)
  (log5:with-context "load-frame"
    (handler-case
        (with-frame-slots (me)
          (log-mpeg-frame "loading frame from pos ~:d" (stream-seek instream))
          (when (null hdr-u32)          ; has header already been read in?
            (log-mpeg-frame "reading in header")
            (setf pos (stream-seek instream))
            (setf hdr-u32 (stream-read-u32 instream))
            (when (null hdr-u32)
              (log-mpeg-frame "hit EOF")
              (return-from load-frame nil)))

          (if (parse-header me)
              (progn
                (log-mpeg-frame "header parsed ok")
                (setf size (get-frame-size version layer bit-rate sample-rate padded))
                (when read-payload
                  (setf payload (stream-read-sequence instream (- size 4))))
                t)
              (progn
                (log-mpeg-frame "header didn't parse!")
                nil)))
      (end-of-file (c)
        (declare (ignore c))
        (log-mpeg-frame "Hit EOF")
        nil))))

(defmethod parse-header ((me frame))
  "Given a frame, verify that is a valid MPEG audio frame by examining the header.
A header looks like this:
Bits 31-21 (11 bits): the sync word.  Must be #xffe (NB version 2.5 standard)
Bits 20-19 (2  bits): the version
Bits 18-17 (2  bits): the layer
Bit     16 (1  bit ): the protection bit
Bits 15-12 (4  bits): the bit-rate index
Bits 11-10 (2  bits): the sample-rate index
Bit      9 (1  bit ): the padding bit
Bit      8 (1  bit ): the private bit
Bits   7-6 (2  bits): the channel mode
Bits   5-4 (2  bits): the mode extension
Bit      3 (1  bit ): the copyright bit
Bit      2 (1  bit ): the original bit
Bits   1-0 (2  bits): the emphasis"

  (declare #.utils:*standard-optimize-settings*)
  (log5:with-context "parse-header"
    (with-frame-slots (me)
      ;; check sync word
      (setf sync (get-bitfield hdr-u32 31 11))
                                        ;(setf (ldb (byte 8 8) sync) (ldb (byte 8 24) hdr-u32))
                                        ;(setf (ldb (byte 3 5) sync) (ldb (byte 3 5) (ldb (byte 8 16) hdr-u32)))
      (when (not (= sync +sync-word+))
        (log-mpeg-frame "bad sync ~x/~x" sync hdr-u32)
        (return-from parse-header nil))

      ;; check version
                                        ;(setf version (ldb (byte 2 3) (ldb (byte 8 16) hdr-u32)))
      (setf version (get-bitfield hdr-u32 20 2))
      (when (not (valid-version version))
        (log-mpeg-frame "bad version ~d" version)
        (return-from parse-header nil))

      ;; check layer
                                        ;(setf layer (ldb (byte 2 1) (ldb (byte 8 16) hdr-u32)))
      (setf layer (get-bitfield hdr-u32 18 2))
      (when (not (valid-layer layer))
        (log-mpeg-frame "bad layer ~d" layer)
        (return-from parse-header nil))

                                        ;(setf protection (ldb (byte 1 0) (ldb (byte 8 16) hdr-u32)))
      (setf protection (get-bitfield hdr-u32 16 1))

      (setf samples (get-samples-per-frame version layer))

      ;; check bit-rate
                                        ;(let ((br-index (the fixnum (ldb (byte 4 4) (ldb (byte 8 8) hdr-u32)))))
      (let ((br-index (get-bitfield hdr-u32 15 4)))
        (when (not (valid-bit-rate-index br-index))
          (log-mpeg-frame "bad bit-rate index ~d" br-index)
          (return-from parse-header nil))

        (setf bit-rate (get-bit-rate version layer br-index)))

      ;; check sample rate
                                        ;(let ((sr-index (the fixnum (ldb (byte 2 2) (ldb (byte 8 8) hdr-u32)))))
      (let ((sr-index (get-bitfield hdr-u32 11 2)))
        (when (not (valid-sample-rate-index sr-index))
          (log-mpeg-frame "bad sample-rate index ~d" sr-index)
          (return-from parse-header nil))

        (setf sample-rate (get-sample-rate version sr-index)))

                                        ;(setf padded (ldb (byte 1 1) (ldb (byte 8 8) hdr-u32)))
      (setf padded (get-bitfield hdr-u32 9 1))

                                        ;(setf private (ldb (byte 1 0) (ldb (byte 8 8) hdr-u32)))
      (setf private (get-bitfield hdr-u32 8 1))

                                        ;(setf channel-mode (ldb (byte 2 6) (ldb (byte 8 0) hdr-u32)))
      (setf channel-mode (get-bitfield hdr-u32 7 2))

                                        ;(setf mode-extension (ldb (byte 2 4) (ldb (byte 8 0) hdr-u32)))
      (setf mode-extension (get-bitfield hdr-u32 5 2))

                                        ;(setf copyright (ldb (byte 1 3) (ldb (byte 8 0) hdr-u32)))
      (setf copyright (get-bitfield hdr-u32 3 1))

                                        ;(setf original (ldb (byte 1 2) (ldb (byte 8 0) hdr-u32)))
      (setf original (get-bitfield hdr-u32 2 1))

                                        ;(setf emphasis (ldb (byte 2 0) (ldb (byte 8 0) hdr-u32)))
      (setf emphasis (get-bitfield hdr-u32 1 2))

      ;; check emphasis
      (when (not (valid-emphasis emphasis))
        (log-mpeg-frame "bad emphasis ~d" emphasis)
        (return-from parse-header nil))

      (log-mpeg-frame "good parse: ~a" me)
      t)))

(defmethod vpprint ((me frame) stream)
  (format stream "~a"
          (with-output-to-string (s)
            (with-frame-slots (me)
              (format s "MPEG Frame: position in file = ~:d, header in (hex) bytes = ~x, size = ~d, sync word = ~x, " pos hdr-u32 size sync)
              (when vbr
                (format s "~&vbr-info: ~a~%" vbr))
              (format s "version = ~a, layer = ~a, crc protected? = ~[yes~;no~], bit-rate = ~:d bps, sampling rate = ~:d bps, padded? = ~[no~;yes~], private bit set? = ~[no~;yes~], channel mode = ~a, "
                      (get-mpeg-version-string version) (get-layer-string layer)
                      protection bit-rate sample-rate padded private (get-channel-mode-string channel-mode))
              (format s "mode extension = ~a, copyrighted? = ~[no~;yes~], original? = ~[no~;yes~], emphasis = ~a"
                      (get-mode-extension-string channel-mode layer mode-extension) copyright original (get-emphasis-string emphasis))
              (when payload
                (format s "~%frame payload[~:d] = ~a~%" (length payload) (utils:printable-array payload)))))))

(defclass vbr-info ()
  ((tag    :accessor tag    :initarg :tag)
   (flags  :accessor flags  :initarg :flags)
   (frames :accessor frames :initarg :frames)
   (bytes  :accessor bytes  :initarg :bytes)
   (tocs   :accessor tocs   :initarg :tocs)
   (scale  :accessor scale  :initarg :scale))
  (:default-initargs :tag nil :flags 0 :frames nil :bytes nil :tocs nil :scale nil))

(defmacro with-vbr-info-slots ((instance) &body body)
  `(with-slots (tag flags frames bytes tocs scale) ,instance
     ,@body))

(defconstant +vbr-frames+  1)
(defconstant +vbr-bytes+   2)
(defconstant +vbr-tocs+    4)
(defconstant +vbr-scale+   8)

(defun get-side-info-size (version channel-mode)
  (declare #.utils:*standard-optimize-settings*)
  (cond ((= (the fixnum version) (the fixnum +mpeg-1+))
         (cond ((= (the fixnum channel-mode) (the fixnum +channel-mode-mono+)) 17)
               (t 32)))
        (t (cond ((= (the fixnum channel-mode) (the fixnum +channel-mode-mono+)) 9)
                 (t 17)))))

(defmethod check-vbr ((me frame) fn)
  (declare #.utils:*standard-optimize-settings*)
  (log5::with-context "check-vbr"
    (with-frame-slots (me)

      (let ((i (get-side-info-size version channel-mode)))
        (log-mpeg-frame "array index = ~d, payload size = ~d" i (length payload))
        (when (>= i (length payload))
          (return-from check-vbr nil))

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
            (stream-seek v i :start)            ; seek to Xing/Info offset
            (setf (tag vbr) (stream-read-iso-string-with-len v 4))
            (setf (flags vbr) (stream-read-u32 v))

            (when (logand (flags vbr) +vbr-frames+)
              (setf (frames vbr) (stream-read-u32 v))
              (log-mpeg-frame "Xing frames set: read ~d" (frames vbr))
              (when (zerop (frames vbr))
                (warn-user "warning file ~a Xing/Info header flags has FRAMES set, but field is zero." fn)))

            (when (logand (flags vbr) +vbr-bytes+)
              (setf (bytes vbr) (stream-read-u32 v))
              (log-mpeg-frame "Xing bytes set: read ~d" (bytes vbr)))

            (when (logand (flags vbr) +vbr-tocs+)
              (setf (tocs vbr) (stream-read-sequence v 100))
              (log-mpeg-frame "Xing tocs set: read ~a" (tocs vbr)))

            (when (logand (flags vbr) +vbr-scale+)
              (setf (scale vbr) (stream-read-u32 v))
              (log-mpeg-frame "Xing scale set: read ~d" (scale vbr)))
            (log-mpeg-frame "vbr-info = ~a" (vpprint vbr nil))))))))

(defmethod vpprint ((me vbr-info) stream)
  (with-vbr-info-slots (me)
    (format stream "tag = ~a, flags = 0x~x, frame~p = ~:d, bytes = ~:d, tocs = ~d, scale = ~d, "
            tag flags frames frames bytes tocs scale)))

(defun find-first-sync (in)
  (declare #.utils:*standard-optimize-settings*)
  (log5:with-context "find-first-sync"

    (log-mpeg-frame "Looking for first sync, begining at file position ~:d" (stream-seek in))
    (let ((hdr-u32)
          (count 0)
          (pos))

      (handler-case
          (loop
            (setf pos (stream-seek in))
            (setf hdr-u32 (stream-read-u32 in))
            (when (null hdr-u32)
              (return-from find-first-sync nil))
            (incf count)

            (when (= (logand hdr-u32 #xffe00000) #xffe00000) ; magic number is potential sync frame header
              (log-mpeg-frame "Potential sync bytes at ~:d: <~x>" pos hdr-u32)
              (let ((hdr (make-instance 'frame :hdr-u32 hdr-u32 :pos pos)))
                (if (load-frame hdr :instream in :read-payload t)
                    (progn
                      (check-vbr hdr (stream-filename in))
                      (log-mpeg-frame "Valid header being returned: ~a, searched ~:d times" hdr count)
                      (return-from find-first-sync hdr))
                    (progn
                      (log-mpeg-frame "hdr wasn't valid: ~a" hdr))))))
        (condition (c) (progn
                         (warn-user "Condtion <~a> signaled while looking for first sync" c)
                         (log-mpeg-frame "got a condition while looking for first sync: ~a" c)
                         (error c)))) ; XXX should I propogate this, or just return nil
      nil)))

(defmethod next-frame ((me frame) &key instream read-payload)
  "Get next frame.  If READ-PAYLOAD is true, read in contents for frame, else, seek to next frame header."
  (declare #.utils:*standard-optimize-settings*)
  (log5:with-context "next-frame"
    (let ((nxt-frame (make-instance 'frame)))
      (when (not (payload me))
        (log-mpeg-frame "no payload load required in current frame, skipping from ~:d forward ~:d bytes"
                        (stream-seek instream)
                        (- (size me) 4) :current)
        (stream-seek instream (- (size me) 4) :current))

      (log-mpeg-frame "at pos ~:d, read-payload is ~a" (stream-seek instream) read-payload)
      (if (load-frame nxt-frame :instream instream :read-payload read-payload)
          nxt-frame
          nil))))

(defparameter *max-frames-to-read* most-positive-fixnum "when trying to determine bit-rate, etc, read at most this many frames")

(defun map-frames (in func &key (start-pos nil) (read-payload nil) (max nil))
  "Loop through the MPEG audio frames in a file.  If *MAX-FRAMES-TO-READ* is set, return after reading that many frames."
  (declare #.utils:*standard-optimize-settings*)
  (log5:with-context "next-frame"
    (log-mpeg-frame "mapping frames, start pos ~:d" start-pos)

    (when start-pos
      (stream-seek in start-pos :start))

    (loop
      for max-frames = (if max max *max-frames-to-read*)
      for count = 0 then (incf count)
      for frame = (find-first-sync in) then (next-frame frame :instream in :read-payload read-payload)
      while (and frame (< count max-frames)) do
        (log-mpeg-frame "map-frames: at pos ~:d, dispatching function" (pos frame))
        (funcall func frame))))

(defclass mpeg-audio-info ()
  ((is-vbr      :accessor is-vbr      :initarg :is-vbr      :initform nil)
   (n-frames    :accessor n-frames    :initarg :n-frames    :initform 0)
   (bit-rate    :accessor bit-rate    :initarg :bit-rate    :initform nil)
   (sample-rate :accessor sample-rate :initarg :sample-rate :initform nil)
   (len         :accessor len         :initarg :len         :initform nil)
   (version     :accessor version     :initarg :version     :initform nil)
   (layer       :accessor layer       :initarg :layer       :initform nil)))

(defmethod vpprint ((me mpeg-audio-info) stream)
  (with-slots (is-vbr sample-rate bit-rate len version layer n-frames) me
    (format stream "~:d frame~p read, ~a, ~a, ~:[CBR,~;VBR,~] sample rate: ~:d Hz, bit rate: ~:d Kbps, duration: ~:d:~2,'0d"
            n-frames n-frames
            (get-mpeg-version-string version)
            (get-layer-string layer)
            is-vbr
            sample-rate
            (round (/ bit-rate 1000))
            (floor (/ len 60)) (round (mod len 60)))))

(defun calc-bit-rate-exhaustive (in start info)
  "Map every MPEG frame in IN and calculate the bit-rate"
  (declare #.utils:*standard-optimize-settings*)
  (log5:with-context "calc-bit-rate-exhaustive"
    (let ((total-len 0)
          (last-bit-rate nil)
          (bit-rate-total 0)
          (vbr nil))
      (log-mpeg-frame "broken Xing/Info header found, reading all frames")
      (with-slots (is-vbr sample-rate bit-rate len version layer n-frames) info
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
                    :read-payload nil :start-pos start)

        (log-mpeg-frame "finished mapping. read ~:d frames" n-frames)

        (when (or (< n-frames 10) (zerop bit-rate-total))
          (log-mpeg-frame "couldn't get audio-info: only got ~d frames" n-frames)
          (return-from calc-bit-rate-exhaustive))

        (setf is-vbr t)
        (setf len total-len)
        (setf bit-rate (float (/ bit-rate-total n-frames)))
        (log-mpeg-frame "len = ~:d, bit-rate = ~f" len bit-rate)))))

 (defun get-mpeg-audio-info (in &key) ;; (max-frames *max-frames-to-read*))
   "Get MPEG Layer 3 audio information.
 If the first MPEG frame we find is a Xing/Info header, return that as info.
 Else, we assume CBR and calculate the duration, etc."
  (declare #.utils:*standard-optimize-settings*)
   (log5:with-context "get-mpeg-audio-info"
     (let ((first-frame (find-first-sync in))
           (info (make-instance 'mpeg-audio-info)))

       (log-mpeg-frame "search for first frame yielded ~a" (vpprint first-frame nil))
       (when (null first-frame)
         (return-from get-mpeg-audio-info nil))

       (with-slots (is-vbr sample-rate bit-rate len version layer n-frames) info
         (setf version (version first-frame))
         (setf layer (layer first-frame))
         (setf sample-rate (sample-rate first-frame))

         (if (vbr first-frame)
             ;; found a Xing header, now check to see if it is correct
             (if (zerop (frames (vbr first-frame)))
                 (calc-bit-rate-exhaustive in (pos first-frame) info) ; Xing header broken, read all frames to calc
                 ;; Good Xing header, use info in VBR to calc
                 (progn
                   (setf n-frames 1)
                   (setf is-vbr t)
                   (setf len (float (* (frames (vbr first-frame)) (/ (samples first-frame) (sample-rate first-frame)))))
                   (setf bit-rate (float (/ (* 8 (bytes (vbr first-frame))) len)))))

             ;; No Xing header found.  Assume CBR and calculate based on first frame
             (let* ((first (pos first-frame))
                    (last (- (audio-streams:stream-size in) (if (id3-frame::v21-tag-header (id3-header in)) 128 0)))
                    (n-fr (round (/ (float (- last first)) (float (size first-frame)))))
                    (n-sec (round (/ (float (* (size first-frame) n-fr)) (float (* 125 (float (/ (bit-rate first-frame) 1000))))))))
               (setf is-vbr nil)
               (setf n-frames 1)
               (setf len n-sec)
               (setf bit-rate (float (bit-rate first-frame))))))

             info)))
