;;; -*- Mode: Lisp;  show-trailing-whitespace: t; Base: 10; indent-tabs: nil; Syntax: ANSI-Common-Lisp; Package: MP3-TAG; -*-
;;; Copyright (c) 2013, Mark VandenBrink. All rights reserved.
(in-package #:mp3-tag)

(defparameter *id3v1-genres*
  #("Blues"
	"Classic Rock"
	"Country"
	"Dance"
	"Disco"
	"Funk"
	"Grunge"
	"Hip-Hop"
	"Jazz"
	"Metal"
	"New Age"
	"Oldies"
	"Other"
	"Pop"
	"R&B"
	"Rap"
	"Reggae"
	"Rock"
	"Techno"
	"Industrial"
	"Alternative"
	"Ska"
	"Death Metal"
	"Pranks"
	"Soundtrack"
	"Euro-Techno"
	"Ambient"
	"Trip-Hop"
	"Vocal"
	"Jazz+Funk"
	"Fusion"
	"Trance"
	"Classical"
	"Instrumental"
	"Acid"
	"House"
	"Game"
	"Sound Clip"
	"Gospel"
	"Noise"
	"Alternative Rock"
	"Bass"
	"Soul"
	"Punk"
	"Space"
	"Meditative"
	"Instrumental Pop"
	"Instrumental Rock"
	"Ethnic"
	"Gothic"
	"Darkwave"
	"Techno-Industrial"
	"Electronic"
	"Pop-Folk"
	"Eurodance"
	"Dream"
	"Southern Rock"
	"Comedy"
	"Cult"
	"Gangsta"
	"Top 40"
	"Christian Rap"
	"Pop/Funk"
	"Jungle"
	"Native American"
	"Cabaret"
	"New Wave"
	"Psychedelic"
	"Rave"
	"Showtunes"
	"Trailer"
	"Lo-Fi"
	"Tribal"
	"Acid Punk"
	"Acid Jazz"
	"Polka"
	"Retro"
	"Musical"
	"Rock & Roll"
	"Hard Rock"
	"Folk"
	"Folk/Rock"
	"National Folk"
	"Swing"
	"Fusion"
	"Bebob"
	"Latin"
	"Revival"
	"Celtic"
	"Bluegrass"
	"Avantgarde"
	"Gothic Rock"
	"Progressive Rock"
	"Psychedelic Rock"
	"Symphonic Rock"
	"Slow Rock"
	"Big Band"
	"Chorus"
	"Easy Listening"
	"Acoustic"
	"Humour"
	"Speech"
	"Chanson"
	"Opera"
	"Chamber Music"
	"Sonata"
	"Symphony"
	"Booty Bass"
	"Primus"
	"Porn Groove"
	"Satire"
	"Slow Jam"
	"Club"
	"Tango"
	"Samba"
	"Folklore"
	"Ballad"
	"Power Ballad"
	"Rhythmic Soul"
	"Freestyle"
	"Duet"
	"Punk Rock"
	"Drum Solo"
	"A Cappella"
	"Euro-House"
	"Dance Hall"
	"Goa"
	"Drum & Bass"
	"Club-House"
	"Hardcore"
	"Terror"
	"Indie"
	"BritPop"
	"Negerpunk"
	"Polsk Punk"
	"Beat"
	"Christian Gangsta Rap"
	"Heavy Metal"
	"Black Metal"
	"Crossover"
	"Contemporary Christian"
	"Christian Rock"
	"Merengue"
	"Salsa"
	"Thrash Metal"
	"Anime"
	"Jpop"
	"Synthpop"))

(defun get-id3v1-genre (n) 
  (let ((idx (- n 1))) ; arrays are zero-based
	(if (> idx (length *id3v1-genres*))
		"BAD GENRE!?!?!?"
		(aref *id3v1-genres* idx))))

(defun get-frames (stream names)
  (let (found-frames)
	(mp3-map-frames stream
					:func (lambda (f)
							(when (member (id f) names :test #'string=)
							  (push f found-frames))))
	found-frames))

(defmethod album ((me mp3-file-stream))
  (let ((ret (get-frames me '("TAL" "TALB"))))
	(when ret
	  (assert (= 1 (length ret)) () "There can be only one album tag")
	  (return-from album (info (first ret)))))
  (if (v21-tag-header (mp3-header me))
	  (album (v21-tag-header (mp3-header me)))
	  nil))

(defmethod artist ((me mp3-file-stream))
  (let ((ret (get-frames me '("TP1" "TPE1"))))
	(when ret
	  (assert (= 1 (length ret)) () "There can be only one artist tag")
	  (return-from artist (info (first ret)))))
  (if (v21-tag-header (mp3-header me))
	  (artist (v21-tag-header (mp3-header me)))
	  nil))

(defmethod comment ((me mp3-file-stream))
  (let ((ret (get-frames me '("COM" "COMM"))))
	(when ret
	  (let ((new-ret))
		(dolist (f ret)
		  (push (list (encoding f) (lang f) (desc f) (val f)) new-ret))
		(return-from comment new-ret))))
  (if (v21-tag-header (mp3-header me))
	  (comment (v21-tag-header (mp3-header me)))
	  nil))

(defmethod year ((me mp3-file-stream))
  (let ((ret (get-frames me '("TRD" "TDRC"))))
	(when ret
	  (assert (= 1 (length ret)) () "There can be only one year tag")
	  (return-from year (info (first ret)))))
  (if (v21-tag-header (mp3-header me))
	  (year (v21-tag-header (mp3-header me)))
	  nil))

(defmethod title ((me mp3-file-stream))
  (let ((ret (get-frames me '("TT2" "TIT2"))))
	(when ret
	  (assert (= 1 (length ret)) () "There can be only one title tag")
	  (return-from title (info (first ret)))))
  (if (v21-tag-header (mp3-header me))
	  (title (v21-tag-header (mp3-header me)))
	  nil))

(defmethod genre ((me mp3-file-stream))
  (let ((ret (get-frames me '("TCO" "TCON"))))
	(when ret
	  (assert (= 1 (length ret)) () "There can be only one genre tag")
	  (return-from genre (info (first ret)))))
  (if (v21-tag-header (mp3-header me))
	  (get-id3v1-genre (genre (v21-tag-header (mp3-header me))))
	  nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; no V2.1 tags for any of these ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod album-artist ((me mp3-file-stream))
  (let ((ret (get-frames me '("TP2" "TPE2"))))
	(when ret
	  (assert (= 1 (length ret)) () "There can be only one album-artist tag")
	  (return-from album-artist (info (first ret)))))
  nil)

(defmethod composer ((me mp3-file-stream))
  (let ((ret (get-frames me '("TCM" "TCOM"))))
	(when ret
	  (assert (= 1 (length ret)) () "There can be only one composer tag")
	  (return-from composer (info (first ret)))))
  nil)

(defmethod copyright ((me mp3-file-stream))
  (let ((ret (get-frames me '("TCR" "TCOP"))))
	(when ret
	  (assert (= 1 (length ret)) () "There can be only one copyright tag")
	  (return-from copyright (info (first ret)))))
  nil)

(defmethod encoder ((me mp3-file-stream))
  (let ((ret (get-frames me '("TEN" "TENC"))))
	(when ret
	  (assert (= 1 (length ret)) () "There can be only one encoder tag")
	  (return-from encoder (info (first ret)))))
  nil)

(defmethod groups ((me mp3-file-stream))
  (let ((ret (get-frames me '("TT1" "TTE1"))))
	(when ret
	  (assert (= 1 (length ret)) () "There can be only one group tag")
	  (return-from groups (info (first ret)))))
  nil)

(defmethod lyrics ((me mp3-file-stream))
  (let ((ret (get-frames me '("ULT" "USLT"))))
	(when ret
	  (assert (= 1 (length ret)) () "There can be only one lyrics tag")
	  (return-from lyrics (val (first ret)))))
  nil)

(defmethod purchased-date ((me mp3-file-stream)) "NIY")

(defmethod tool ((me mp3-file-stream)) "NIY")

(defmethod writer ((me mp3-file-stream))
  (let ((ret (get-frames me '("TCM" "TCOM"))))
	(when ret
	  (assert (= 1 (length ret)) () "There can be only one composer tag")
	  (return-from writer (info (first ret)))))
  nil)

(defmethod compilation ((me mp3-file-stream)) "NIY")

(defmethod disk ((me mp3-file-stream))
  (let ((ret (get-frames me '("TPA" "TPOS"))))
	(when ret
	  (assert (= 1 (length ret)) () "There can be only one disk number tag")
	  (return-from disk (info (first ret)))))
  nil)

(defmethod tempo ((me mp3-file-stream))
  (let ((ret (get-frames me '("TBP" "TBPM"))))
	(when ret
	  (assert (= 1 (length ret)) () "There can be only one tempo tag")
	  (return-from tempo (info (first ret)))))
  nil)

(defmethod track ((me mp3-file-stream))
  (let ((ret (get-frames me '("TRK" "TRCK"))))
	(when ret
	  (assert (= 1 (length ret)) () "There can be only one track number tag")
	  (return-from track (info (first ret)))))
  nil)

(defmethod show-tags ((me mp3-file-stream) &key (raw nil))
  "Show the tags for an mp3-file"
  (if raw
	  (format t "~a:~a~%" (stream-filename me) (mp3-frame:vpprint (audio-streams:mp3-header me) nil))
	  (let ((album (album me))
			(album-artist (album-artist me))
			(artist (artist me))
			(comment (comment me))
			(compilation (compilation me))
			(composer (composer me))
			(copyright (copyright me))
			(disk (disk me))
			(encoder (encoder me))
			(genre (genre me))
			(groups (groups me))
			(lyrics (lyrics me))
			(purchased-date (purchased-date me))
			(tempo (tempo me))
			(title (title me))
			(tool (tool me))
			(track (track me))
			(writer (writer me))
			(year (year me)))
		(format t "~a~%" (stream-filename me))
		(when album (format t "~4talbum: ~a~%" album))
		(when album-artist (format t "~4talbum-artist: ~a~%" album-artist))
		(when artist (format t "~4tartist: ~a~%" artist))
		(when comment (format t "~4tcomment: ~a~%" comment))
		(format t "~4tcompilation: ~a~%" compilation)
		(when composer (format t "~4tcomposer: ~a~%" composer))
		(when copyright (format t "~4tcopyright: ~a~%" copyright))
		(when disk (format t "~4tdisk: ~a~%" disk))
		(when encoder (format t "~4tencoder: ~a~%" encoder))
		(when genre (format t "~4tgenre: ~a~%" genre))
		(when groups (format t "~4tgroups: ~a~%" groups))
		(when lyrics (format t "~4tlyrics: ~a~%" lyrics))
		(when purchased-date (format t "~4tpurchased date: ~a~%" purchased-date))
		(when tempo (format t "~4ttempo: ~a~%" tempo))
		(when title (format t "~4ttitle: ~a~%" title))
		(when tool (format t "~4ttool: ~a~%" tool))
		(when track (format t "~4ttrack: ~a~%" track))
		(when writer (format t "~4twriter: ~a~%" writer))
		(when year (format t "~4tyear: ~a~%" year)))))
