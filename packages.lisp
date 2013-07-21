;;; -*- Mode: Lisp;  show-trailing-whitespace: t; Base: 10; indent-tabs: nil; Syntax: ANSI-Common-Lisp; Package: CL-USER; -*-
;;; Copyright (c) 2013, Mark VandenBrink. All rights reserved.
(in-package #:cl-user)

(defpackage #:audio-streams
  (:export #:octets #:make-octets
		   #:mp3-file-stream #:mp4-file-stream #:base-mem-stream
		   #:parse-mp3-file #:parse-mp4-file #:mp3-frame-condition
		   #:make-mem-stream #:stream-filename
		   #:mp4-atoms #:mp3-header
		   #:stream-read-u8 #:stream-read-u16 #:stream-read-u24 #:stream-read-u32
		   #:stream-decode-iso-string #:stream-deocode-ucs-string #:stream-decode-ucs-be-string
		   #:stream-decode-utf-8-string #:stream-decode-string #:stream-read-iso-string-with-len
		   #:stream-read-ucs-string-with-len #:stream-read-ucs-be-string-with-len
		   #:stream-read-utf-8-string-with-len #:stream-read-string-with-len
		   #:stream-read-iso-string #:stream-read-ucs-string #:stream-read-ucs-be-string
		   #:stream-read-utf-8-string #:stream-read-string #:trim-string
		   #:stream-read-string #:stream-read-sequence #:stream-size
		   #:stream-seek #:stream-close)
  (:use #:common-lisp))

(defpackage #:mp4-atom
  (:export #:mp4-atom #:map-mp4-atom #:find-mp4-atoms #:traverse #:mp4-atom-condition
		   #:atom-file-position #:atom-children #:atom-size #:atom-of-interest #:atom-decoded
		   #:atom-type #:vpprint #:*tag-path* #:tag-get-value #:mp4-atom-condition
		   #:mp4-show-raw-tag-atoms
		   #:+itunes-album+
		   #:+itunes-album-artist+
		   #:+itunes-artist+
		   #:+itunes-comment+
		   #:+itunes-composer+
		   #:+itunes-copyright+
		   #:+itunes-year+
		   #:+itunes-encoder+
		   #:+itunes-groups+
		   #:+itunes-lyrics+
		   #:+itunes-purchased-date+
		   #:+itunes-title+
		   #:+itunes-tool+
		   #:+itunes-writer+
		   #:+itunes-compilation+
		   #:+itunes-cover-art+
		   #:+itunes-disk+
		   #:+itunes-genre+
		   #:+itunes-genre-x+
		   #:+itunes-tempo+
		   #:+itunes-track+
		   #:+itunes-track-n+)
  (:use #:common-lisp #:audio-streams))

(defpackage :mp3-frame
  (:export :mp3-frame #:find-mp3-frames #:mp3-frame-condition #:vpprint #:header :get-frame-info
		   :v21-tag-header :info :version)
  (:use :common-lisp :audio-streams))

(defpackage :mp3-tag
  (:export :show-tags)
  (:use :common-lisp :audio-streams :mp3-frame))

(defpackage #:tag
  (:export #:get-genre-text)
  (:use #:common-lisp))

(defpackage #:mp4-tag
  (:export #:show-tags #:album #:album-artist #:artist #:comment #:composer #:copyright #:created
		   #:encoder #:groups #:lyrics #:purd #:title #:tool #:writer)
  (:use #:common-lisp #:audio-streams))

(defpackage #:logging
  (:export #:with-logging)
  (:use #:common-lisp))
