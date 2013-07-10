;;; -*- Mode: Lisp;  show-trailing-whitespace: t; Base: 10; indent-tabs: nil; Syntax: ANSI-Common-Lisp; Package: CL-USER; -*-
;;; Copyright (c) 2013, Mark VandenBrink. All rights reserved.
(in-package #:cl-user)

(defpackage #:audio-streams
  (:export #:octets #:make-octets
		   #:base-stream
		   #:filename #:instream #:file-size #:endian
		   #:stream-read-u8 #:stream-read-u16 #:stream-read-u24 #:stream-read-u32
		   #:stream-read-string #:stream-read-octets
		   #:stream-seek #:stream-close
		   #:mp4-stream #:make-mp4-stream #:mp4-atoms
		   #:mp3-stream #:make-mp3-stream #:mp3-header
		   #:stream-read-sync-safe-u32 #:stream-read-sync-safe-octets)
  (:use #:common-lisp))

(defpackage #:mp4-atom
  (:export #:mp4-atom #:map-mp4-atom #:find-mp4-atoms #:traverse #:mp4-atom-condition
		   #:atom-file-position #:atom-children #:atom-size #:atom-of-interest #:atom-decoded
		   #:atom-type #:vpprint #:*tag-path* #:tag-get-value
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
  (:export :mp3-frame #:find-mp3-frames #:mp3-frame-condition #:vpprint #:header)
  (:use :common-lisp :audio-streams))

(defpackage :mp3-tag
  (:export :show-tags)
  (:use :common-lisp :audio-streams))

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
