;;; -*- Mode: Lisp;  show-trailing-whitespace: t; Base: 10; indent-tabs: nil; Syntax: ANSI-Common-Lisp; Package: ABSTRACT-TAG; -*-
;;; Copyright (c) 2013, Mark VandenBrink. All rights reserved.
(in-package #:abstract-tag)

(defparameter *raw-tags* nil)

(defparameter *id3v1-genres*
  #("Blues" "Classic Rock" "Country" "Dance" "Disco" "Funk" "Grunge"
    "Hip-Hop" "Jazz" "Metal" "New Age" "Oldies" "Other" "Pop" "R&B" "Rap"
    "Reggae" "Rock" "Techno" "Industrial" "Alternative" "Ska" "Death Metal"
    "Pranks" "Soundtrack" "Euro-Techno" "Ambient" "Trip-Hop" "Vocal"
    "Jazz+Funk" "Fusion" "Trance" "Classical" "Instrumental" "Acid" "House"
    "Game" "Sound Clip" "Gospel" "Noise" "Alternative Rock" "Bass" "Soul"
    "Punk" "Space" "Meditative" "Instrumental Pop" "Instrumental Rock" "Ethnic"
    "Gothic" "Darkwave" "Techno-Industrial" "Electronic" "Pop-Folk" "Eurodance"
    "Dream" "Southern Rock" "Comedy" "Cult" "Gangsta" "Top 40" "Christian Rap"
    "Pop/Funk" "Jungle" "Native American" "Cabaret" "New Wave" "Psychedelic"
    "Rave" "Showtunes" "Trailer" "Lo-Fi" "Tribal" "Acid Punk" "Acid Jazz"
    "Polka" "Retro" "Musical" "Rock & Roll" "Hard Rock" "Folk" "Folk/Rock"
    "National Folk" "Swing" "Fusion" "Bebob" "Latin" "Revival" "Celtic"
    "Bluegrass" "Avantgarde" "Gothic Rock" "Progressive Rock" "Psychedelic Rock"
    "Symphonic Rock" "Slow Rock" "Big Band" "Chorus" "Easy Listening"
    "Acoustic" "Humour" "Speech" "Chanson" "Opera" "Chamber Music" "Sonata"
    "Symphony" "Booty Bass" "Primus" "Porn Groove" "Satire" "Slow Jam" "Club"
    "Tango" "Samba" "Folklore" "Ballad" "Power Ballad" "Rhythmic Soul"
    "Freestyle" "Duet" "Punk Rock" "Drum Solo" "A Cappella" "Euro-House" "Dance Hall"
    "Goa" "Drum & Bass" "Club-House" "Hardcore" "Terror" "Indie"
    "BritPop" "Negerpunk" "Polsk Punk" "Beat" "Christian Gangsta Rap" "Heavy Metal"
    "Black Metal" "Crossover" "Contemporary Christian" "Christian Rock"
    "Merengue" "Salsa" "Thrash Metal" "Anime" "Jpop" "Synthpop"))

(defun find-genre (name)
  "For debug purpose only: test function to return index of genre, given a name. ignores case and returns first complete match"
  (let ((i 0)
        (match-str (string-downcase name)))
    (loop for s across *id3v1-genres* do
      (if (string= (string-downcase s) match-str)
          (return-from find-genre i))
      (incf i))))

(defun get-id3v1-genre (n)
  "Given N, a supposed ID3 genre, range check it to make sure it is > 0 and < (sizeof *ID3V1-GENRES*)"
  (declare #.utils:*standard-optimize-settings*)
  (if (or (> n (1- (length *id3v1-genres*)))
            (< n 0))
        "BAD GENRE"
        (aref *id3v1-genres* n)))

;;; The abstract tag interface
(defgeneric album (stream))
(defgeneric album (stream))
(defgeneric artist (stream))
(defgeneric comment (stream))
(defgeneric composer (stream))
(defgeneric copyright (stream))
(defgeneric cover (stream))
(defgeneric year (stream))
(defgeneric encoder (stream))
(defgeneric groups (stream))
(defgeneric lyrics (stream))
(defgeneric title (stream))
(defgeneric writer (stream))
(defgeneric compilation (stream))
(defgeneric disk (stream))
(defgeneric tempo (stream))
(defgeneric genre (stream))

;;;;;;;;;;;;;;;;;;;; MP3 ;;;;;;;;;;;;;;;;;;;;
(defun get-frames (stream names)
  "Given a MP3-STREAM, search its frames for NAMES.  Return file-order list of matching frames"
  (let (found-frames)
    (map-id3-frames stream
                    :func (lambda (f)
                            (when (member (id f) names :test #'string=)
                              (push f found-frames))))
    (nreverse found-frames)))

(defmethod cover ((me mp3-file-stream))
  (declare #.utils:*standard-optimize-settings*)
  (let ((pictures)
        (frames (get-frames me '("PIC" "APIC"))))
    (when frames
      (dolist (f frames)
        (push (picture-info f) pictures)))
    pictures))

(defmethod album ((me mp3-file-stream))
  (declare #.utils:*standard-optimize-settings*)
  (let ((frames (get-frames me '("TAL" "TALB"))))
    (when frames
      (assert (= 1 (length frames)) () "There can be only one album tag")
      (return-from album (info (first frames)))))
  (if (v21-tag-header (id3-header me))
      (album (v21-tag-header (id3-header me)))
      nil))

(defmethod artist ((me mp3-file-stream))
  (declare #.utils:*standard-optimize-settings*)
  (let ((frames (get-frames me '("TP1" "TPE1"))))
    (when frames
      (assert (= 1 (length frames)) () "There can be only one artist tag")
      (return-from artist (info (first frames)))))
  (if (v21-tag-header (id3-header me))
      (artist (v21-tag-header (id3-header me)))
      nil))

(defmethod comment ((me mp3-file-stream))
  (declare #.utils:*standard-optimize-settings*)
  (let ((frames (get-frames me '("COM" "COMM"))))
    (when frames
      (let ((new-frames))
        (dolist (f frames)
          (push (list (encoding f) (lang f) (desc f) (val f)) new-frames))
        (return-from comment new-frames))))
  (if (v21-tag-header (id3-header me))
      (comment (v21-tag-header (id3-header me)))
      nil))

(defmethod year ((me mp3-file-stream))
  (declare #.utils:*standard-optimize-settings*)
  (let ((frames (get-frames me '("TRD" "TDRC"))))
    (when frames
      (assert (= 1 (length frames)) () "There can be only one year tag")
      (return-from year (info (first frames)))))
  (if (v21-tag-header (id3-header me))
      (year (v21-tag-header (id3-header me)))
      nil))

(defmethod title ((me mp3-file-stream))
  (declare #.utils:*standard-optimize-settings*)
  (let ((frames (get-frames me '("TT2" "TIT2"))))
    (when frames
      (assert (= 1 (length frames)) () "There can be only one title tag")
      (return-from title (info (first frames)))))
  (if (v21-tag-header (id3-header me))
      (title (v21-tag-header (id3-header me)))
      nil))

(defmethod genre ((me mp3-file-stream))
  (declare #.utils:*standard-optimize-settings*)
  (let ((frames (get-frames me '("TCO" "TCON"))))
    (when frames
      (when (> (length frames) 1)
        (warn-user "file ~a has more than one genre frame, will only use the first" (stream-filename me)))
      (let ((count)
            (end)
            (str (info (first frames))))

        ;; For V23/V24 TCON frames, a genre can be pretty gnarly.
        ;; if the first byte of the TCON INFO field is a '(', what is between this '('
        ;; and the next ')' is interpreted as an ID3v2.1 genre number.
        ;; These can stack up (called "refinements") too.
        ;; The INFO field can also just be a string.
        ;; We're taking a simplistic approach here: we can handle the '(' case, but
        ;; only allow one (no refinements) OR we can handle the simple string case
        (when (and (>= (length str) 1) (eq #\( (aref str 0)))
          (setf count (count #\( str))
          (when (> count 1) (warn-user "Don't support genre refinement yet, found ~d genres" count))
          (setf end (position #\) str))
          (when (null end) (warn-user "Bad format for genre, ending paren is missing"))
          (setf str (get-id3v1-genre (parse-integer (subseq str 1 end)))))
        (return-from genre str))))

  (if (v21-tag-header (id3-header me))
      (get-id3v1-genre (genre (v21-tag-header (id3-header me))))
      nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; no V2.1 tags for any of these ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod album-artist ((me mp3-file-stream))
  (declare #.utils:*standard-optimize-settings*)
  (let ((frames (get-frames me '("TP2" "TPE2"))))
    (when frames
      (assert (= 1 (length frames)) () "There can be only one album-artist tag")
      (return-from album-artist (info (first frames)))))
  nil)

(defmethod composer ((me mp3-file-stream))
  (declare #.utils:*standard-optimize-settings*)
  (let ((frames (get-frames me '("TCM" "TCOM"))))
    (when frames
      (assert (= 1 (length frames)) () "There can be only one composer tag")
      (return-from composer (info (first frames)))))
  nil)

(defmethod copyright ((me mp3-file-stream))
  (declare #.utils:*standard-optimize-settings*)
  (let ((frames (get-frames me '("TCR" "TCOP"))))
    (when frames
      (assert (= 1 (length frames)) () "There can be only one copyright tag")
      (return-from copyright (info (first frames)))))
  nil)

(defmethod encoder ((me mp3-file-stream))
  (declare #.utils:*standard-optimize-settings*)
  (let ((frames (get-frames me '("TEN" "TENC"))))
    (when frames
      (assert (= 1 (length frames)) () "There can be only one encoder tag")
      (return-from encoder (info (first frames)))))
  nil)

(defmethod groups ((me mp3-file-stream))
  (declare #.utils:*standard-optimize-settings*)
  (let ((frames (get-frames me '("TT1" "TTE1"))))
    (when frames
      (assert (= 1 (length frames)) () "There can be only one group tag")
      (return-from groups (info (first frames)))))
  nil)

(defmethod lyrics ((me mp3-file-stream))
  (declare #.utils:*standard-optimize-settings*)
  (let ((frames (get-frames me '("ULT" "USLT"))))
    (when frames
      (assert (= 1 (length frames)) () "There can be only one lyrics tag")
      (return-from lyrics (val (first frames)))))
  nil)

(defmethod writer ((me mp3-file-stream))
  (declare #.utils:*standard-optimize-settings*)
  (let ((frames (get-frames me '("TCM" "TCOM"))))
    (when frames
      (assert (= 1 (length frames)) () "There can be only one composer tag")
      (return-from writer (info (first frames)))))
  nil)

(defmethod compilation ((me mp3-file-stream))
  (declare #.utils:*standard-optimize-settings*)
  (let ((frames (get-frames me '("TCMP" "TCP"))))
    (if frames
      (info (first frames))
      "no")))

(defmethod disk ((me mp3-file-stream))
  (declare #.utils:*standard-optimize-settings*)
  (let ((frames (get-frames me '("TPA" "TPOS"))))
    (when frames
      (assert (= 1 (length frames)) () "There can be only one disk number tag")
      (return-from disk (mk-lst (info (first frames))))))
  nil)

(defmethod tempo ((me mp3-file-stream))
  (declare #.utils:*standard-optimize-settings*)
  (let ((frames (get-frames me '("TBP" "TBPM"))))
    (when frames
      (assert (= 1 (length frames)) () "There can be only one tempo tag")
      (return-from tempo (info (first frames)))))
  nil)

(defun mk-lst (str)
  (declare #.utils:*standard-optimize-settings*)
  (let ((pos (position #\/ str)))
    (if (null pos)
        (list str)
        (list (subseq str 0 pos) (subseq str (+ 1 pos))))))

(defmethod track ((me mp3-file-stream))
  (declare #.utils:*standard-optimize-settings*)
  (let ((frames (get-frames me '("TRK" "TRCK"))))
    (when frames
      (assert (= 1 (length frames)) () "There can be only one track number tag")
      (return-from track (mk-lst (info (first frames))))))
  nil)

(defmethod show-tags ((me mp3-file-stream) &key (raw *raw-tags*))
  "Show the tags for an mp3-file.  If RAW is non-nil, dump all the frames; else, print out a subset."
  (declare #.utils:*standard-optimize-settings*)
  (if raw
      (format t "~a~%~a~%" (stream-filename me)
              (with-output-to-string (s)
                (when (audio-info me)
                  (mpeg::vpprint (audio-info me) s)
                  (format s "~%"))
                (vpprint (id3-header me) s)))
      (let ((album (album me))
            (album-artist (album-artist me))
            (artist (artist me))
            (comment (comment me))
            (compilation (compilation me))
            (composer (composer me))
            (copyright (copyright me))
            (cover (cover me))
            (disk (disk me))
            (encoder (encoder me))
            (genre (genre me))
            (groups (groups me))
            (lyrics (lyrics me))
            (tempo (tempo me))
            (title (title me))
            (track (track me))
            (writer (writer me))
            (year (year me)))
        (format t "~a~%~a~%" (stream-filename me)
                (if (audio-info me)
                    (mpeg::vpprint (audio-info me) nil) ""))
        (when album (format t "~4talbum: ~a~%" album))
        (when album-artist (format t "~4talbum-artist: ~a~%" album-artist))
        (when artist (format t "~4tartist: ~a~%" artist))
        (when comment (format t "~4tcomment: ~a~%" comment))
        (when compilation (format t "~4tcompilation: ~a~%" compilation))
        (when composer (format t "~4tcomposer: ~a~%" composer))
        (when copyright (format t "~4tcopyright: ~a~%" copyright))
        (when cover (format t "~4tcover: ~a~%" cover))
        (when disk (format t "~4tdisk: ~a~%" disk))
        (when encoder (format t "~4tencoder: ~a~%" encoder))
        (when genre (format t "~4tgenre: ~a~%" genre))
        (when groups (format t "~4tgroups: ~a~%" groups))
        (when lyrics (format t "~4tlyrics: ~a~%" lyrics))
        (when tempo (format t "~4ttempo: ~a~%" tempo))
        (when title (format t "~4ttitle: ~a~%" title))
        (when track (format t "~4ttrack: ~a~%" track))
        (when writer (format t "~4twriter: ~a~%" writer))
        (when year (format t "~4tyear: ~a~%" year)))))

;;;;;;;;;;;;;;;;;;;; MP4 ;;;;;;;;;;;;;;;;;;;;
(defmethod album        ((me mp4-file-stream)) (mp4-atom:tag-get-value (mp4-atoms me) mp4-atom:+itunes-album+))
(defmethod album-artist ((me mp4-file-stream)) (mp4-atom:tag-get-value (mp4-atoms me) mp4-atom:+itunes-album-artist+))
(defmethod artist       ((me mp4-file-stream)) (mp4-atom:tag-get-value (mp4-atoms me) mp4-atom:+itunes-artist+))
(defmethod comment      ((me mp4-file-stream)) (mp4-atom:tag-get-value (mp4-atoms me) mp4-atom:+itunes-comment+))
(defmethod composer     ((me mp4-file-stream)) (mp4-atom:tag-get-value (mp4-atoms me) mp4-atom:+itunes-composer+))
(defmethod copyright    ((me mp4-file-stream)) (mp4-atom:tag-get-value (mp4-atoms me) mp4-atom:+itunes-copyright+))
;;;(defmethod cover        ((me mp4-file-stream)) (mp4-atom:tag-get-value (mp4-atoms me) mp4-atom:+itunes-cover-art+))
(defmethod year         ((me mp4-file-stream)) (mp4-atom:tag-get-value (mp4-atoms me) mp4-atom:+itunes-year+))
(defmethod encoder      ((me mp4-file-stream)) (mp4-atom:tag-get-value (mp4-atoms me) mp4-atom:+itunes-encoder+))
(defmethod groups       ((me mp4-file-stream)) (mp4-atom:tag-get-value (mp4-atoms me) mp4-atom:+itunes-groups+))
(defmethod lyrics       ((me mp4-file-stream)) (mp4-atom:tag-get-value (mp4-atoms me) mp4-atom:+itunes-lyrics+))
(defmethod title        ((me mp4-file-stream)) (mp4-atom:tag-get-value (mp4-atoms me) mp4-atom:+itunes-title+))
(defmethod writer       ((me mp4-file-stream)) (mp4-atom:tag-get-value (mp4-atoms me) mp4-atom:+itunes-writer+))
(defmethod compilation  ((me mp4-file-stream)) (mp4-atom:tag-get-value (mp4-atoms me) mp4-atom:+itunes-compilation+))
(defmethod disk         ((me mp4-file-stream)) (mp4-atom:tag-get-value (mp4-atoms me) mp4-atom:+itunes-disk+))
(defmethod tempo        ((me mp4-file-stream)) (mp4-atom:tag-get-value (mp4-atoms me) mp4-atom:+itunes-tempo+))
(defmethod genre        ((me mp4-file-stream))
  (let ((genre   (mp4-atom:tag-get-value (mp4-atoms me) mp4-atom:+itunes-genre+))
        (genre-x (mp4-atom:tag-get-value (mp4-atoms me) mp4-atom:+itunes-genre-x+)))
    (assert (not (and genre genre-x)))
    (cond
      (genre   (format nil "~d (~a)" genre (get-id3v1-genre (1- genre))))
      (genre-x genre-x)
      (t       "not present"))))
(defmethod track ((me mp4-file-stream))
  (let ((track   (mp4-atom:tag-get-value (mp4-atoms me) mp4-atom:+itunes-track+))
        (track-n (mp4-atom:tag-get-value (mp4-atoms me) mp4-atom:+itunes-track-n+)))
    (assert (not (and track track-n)))
    (if track
        track
        track-n)))

(defmethod show-tags ((me mp4-file-stream) &key (raw *raw-tags*))
  "Show the tags for an MP4-FILE. If RAW is non-nil, dump the DATA atoms; else show subset of DATA atoms"
  (format t "~a~%" (stream-filename me))
  (if raw
      (progn
        (if (audio-info me)
            (mp4-atom:vpprint (audio-info me) t))
        (mp4-atom:mp4-show-raw-tag-atoms me t))
      (let ((album (album me))
            (album-artist (album-artist me))
            (artist (artist me))
            (comment (comment me))
            (compilation (compilation me))
            (composer (composer me))
            (copyright (copyright me))
;;;            (cover (cover me))
            (disk (disk me))
            (encoder (encoder me))
            (genre (genre me))
            (groups (groups me))
            (lyrics (lyrics me))
            (tempo (tempo me))
            (title (title me))
            (track (track me))
            (writer (writer me))
            (year (year me)))

        (if (audio-info me)
          (mp4-atom:vpprint (audio-info me) t))

        (when album (format t "~&~4talbum: ~a~%" album))
        (when album-artist (format t "~4talbum-artist: ~a~%" album-artist))
        (when artist (format t "~4tartist: ~a~%" artist))
        (when comment (format t "~4tcomment: ~a~%" comment))
        (format t "~4tcompilation: ~[no~;yes~;unknown~]~%" (if compilation compilation 2))
        (when composer (format t "~4tcomposer: ~a~%" composer))
        (when copyright (format t "~4tcopyright: ~a~%" copyright))
;;;        (when cover (format t "~4tcover: Number of covers: :~d~%" cover))
        (when disk (format t "~4tdisk: ~a~%" disk))
        (when encoder (format t "~4tencoder: ~a~%" encoder))
        (when genre (format t "~4tgenre: ~a~%" genre))
        (when groups (format t "~4tgroups: ~a~%" groups))
        (when lyrics (format t "~4tlyrics: ~a~%" lyrics))
        (when tempo (format t "~4ttempo: ~a~%" tempo))
        (when title (format t "~4ttitle: ~a~%" title))
        (when track (format t "~4ttrack: ~a~%" track))
        (when writer (format t "~4twriter: ~a~%" writer))
        (when year (format t "~4tyear: ~a~%" year)))))

;;;;;;;;;;;;;;;;;;;; FLAC ;;;;;;;;;;;;;;;;;;;;
(defmacro get-flac-tag-info (stream name)
  `(flac-frame:flac-get-tag (flac-tags ,stream) ,name))

(defmethod album        ((me flac-file-stream)) (get-flac-tag-info me "album"))
(defmethod artist       ((me flac-file-stream)) (get-flac-tag-info me "artist"))
(defmethod album-artist ((me flac-file-stream)) (get-flac-tag-info me "album artist"))
(defmethod comment      ((me flac-file-stream)) (get-flac-tag-info me "comment"))
(defmethod composer     ((me flac-file-stream)) (get-flac-tag-info me "composer"))
(defmethod copyright    ((me flac-file-stream)) (get-flac-tag-info me "copyright"))
(defmethod disk         ((me flac-file-stream)) (get-flac-tag-info me "disk"))
(defmethod encoder      ((me flac-file-stream)) (get-flac-tag-info me "encoder"))
(defmethod year         ((me flac-file-stream)) (get-flac-tag-info me "date"))
(defmethod title        ((me flac-file-stream)) (get-flac-tag-info me "title"))
(defmethod genre        ((me flac-file-stream)) (get-flac-tag-info me "genre"))
(defmethod track        ((me flac-file-stream)) (let ((tr (get-flac-tag-info me "tracknumber"))
                                                      (tn (get-flac-tag-info me "tracktotal")))
                                                  (if tn (list tr tn) tr)))

(defmethod show-tags ((me flac-file-stream) &key (raw *raw-tags*))
  "Show the tags for a FLAC-FILE."
  (format t "~a~%" (stream-filename me))
  (if raw
      (flac-frame:flac-show-raw-tag me t)
      (let ((album (album me))
            (album-artist (album-artist me))
            (artist (artist me))
            (copyright (copyright me))
            (comment (comment me))
            (composer (composer me))
            (encoder (encoder me))
            (genre (genre me))
            (title (title me))
            (track (track me))
            (year (year me)))

        (if (audio-info me)
            (flac-frame:vpprint (audio-info me) t))

        (when album (format t "~&~4talbum: ~a~%" album))
        (when album-artist (format t "~4talbum-artist: ~a~%" album-artist))
        (when artist (format t "~4tartist: ~a~%" artist))
        (when copyright (format t "~4tcopyright: ~a~%" copyright))
        (when comment (format t "~4tcomment: ~a~%" comment))
        (when composer (format t "~4tcomposer: ~a~%" composer))
        (when encoder (format t "~4tencoder: ~a~%" encoder))
        (when genre (format t "~4tgenre: ~a~%" genre))
        (when title (format t "~4ttitle: ~a~%" title))
        (when track (format t "~4ttrack: ~a~%" track))
        (when year (format t "~4tyear: ~a~%" year)))))
