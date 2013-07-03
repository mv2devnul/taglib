;;; -*- Mode: Lisp;  show-trailing-whitespace: t; Base: 10; indent-tabs: nil; Syntax: ANSI-Common-Lisp; Package: CL-USER; -*-
;;; Copyright (c) 2013, Mark VandenBrink. All rights reserved.
(in-package #:cl-user)

(defpackage #:base-file
  (:export #:close-audio-file #:octets #:make-octets #:base-file
		   #:filename #:instream #:file-size #:endian
		   #:read-u8 #:read-u16 #:read-u24 #:read-u32
		   #:read-string #:read-octets #:seek)
  (:use #:common-lisp #:binary-types))

(defpackage #:mp4-file
  (:export #:mp4-file #:make-mp4-file #:atoms)
  (:use #:common-lisp))

(defpackage #:mp3-file
  (:export #:mp3-file #:make-mp3-file #:header #:read-sync-safe-u32)
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
  (:use #:common-lisp #:binary-types #:base-file))

(defpackage :mp3-frame
  (:export :mp3-frame #:find-mp3-frames #:mp3-frame-condition #:vpprint #:header)
  (:use :common-lisp :binary-types :base-file))

(defpackage :mp3-tag
  (:export :show-tags)
  (:use :common-lisp :binary-types :base-file))

(defpackage #:tag
  (:export #:get-genre-text)
  (:use #:common-lisp))

(defpackage #:mp4-tag
  (:export #:show-tags #:album #:album-artist #:artist #:comment #:composer #:copyright #:created
		   #:encoder #:groups #:lyrics #:purd #:title #:tool #:writer)
  (:use #:common-lisp))

(defpackage #:logging
  (:export #:with-logging)
  (:use #:common-lisp))
