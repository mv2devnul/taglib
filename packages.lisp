;;; -*- Mode: Lisp;  show-trailing-whitespace: t; Base: 10; indent-tabs: nil; Syntax: ANSI-Common-Lisp; Package: CL-USER; -*-
;;; Copyright (c) 2013, Mark VandenBrink. All rights reserved.
(in-package #:cl-user)

(defpackage #:tree
  (:export #:add-child
           #:at-path
           #:data
           #:find-tree
           #:first-child
           #:make-node
           #:next-sibling
           #:print-tree
           #:traverse)
  (:use #:common-lisp))

(defpackage #:utils
  (:export #:*standard-optimize-settings*
           #:aif
           #:awhen
           #:dbg
           #:dbg-helper
           #:defconstant*
           #:dump-data
           #:get-bitfield
           #:it
           #:make-keyword
           #:make-octets
           #:memoize
           #:octet
           #:octets
           #:printable-array
           #:redirect
           #:timings
           #:upto-null
           #:warn-user *break-on-warn-user*
           #:while
           #:with-gensyms)
  (:use #:common-lisp))

(defpackage #:profile
  (:export #:off
           #:on
           #:reset
           #:report)
  (:use #:common-lisp))

(defpackage #:iso-639-2
  (:export #:get-iso-639-2-language)
  (:use #:common-lisp :utils))

(defpackage #:audio-streams
  (:export #:make-audio-stream
           #:open-audio-file
           #:stream-filename
           #:stream-read-iso-string
           #:stream-read-sequence
           #:stream-read-u128
           #:stream-read-u16
           #:stream-read-u24
           #:stream-read-u32
           #:stream-read-u64
           #:stream-read-u8
           #:stream-read-ucs-string
           #:stream-read-utf-8-string
           #:stream-seek
           #:stream-size
           *get-audio-info*)
  (:use #:common-lisp #:utils))

(defpackage #:flac
  (:export #:audio-info
           #:filename
           #:flac-file
           #:flac-get-tag
           #:flac-headers
           #:flac-show-raw-tag
           #:flac-tags
           #:get-flac-audio-info
           #:get-flac-audio-info
           #:is-valid-flac-file
           #:parse-audio-file
           #:vpprint)
  (:use #:common-lisp #:utils #:audio-streams))

(defpackage #:m4a
  (:export #:*skipped-m4a-atoms*
           #:+itunes-album+
           #:+itunes-album-artist+
           #:+itunes-artist+
           #:+itunes-comment+
           #:+itunes-compilation+
           #:+itunes-composer+
           #:+itunes-copyright+
           #:+itunes-cover-art+
           #:+itunes-disk+
           #:+itunes-encoder+
           #:+itunes-genre+
           #:+itunes-genre-x+
           #:+itunes-groups+
           #:+itunes-lyrics+
           #:+itunes-purchased-date+
           #:+itunes-tempo+
           #:+itunes-title+
           #:+itunes-tool+
           #:+itunes-track+
           #:+itunes-track-n+
           #:+itunes-writer+
           #:+itunes-year+
           #:atom-file-pos
           #:atom-size
           #:atom-type
           #:audio-info
           #:clear-skipped
           #:filename
           #:get-mp4-audio-info
           #:is-valid-m4-file
           #:map-mp4-atoms
           #:mp4-atom
           #:mp4-atoms
           #:mp4-file
           #:mp4-show-raw-tag-atoms
           #:parse-audio-file
           #:tag-get-value
           #:vpprint)
  (:use #:common-lisp #:audio-streams #:utils))

(defpackage #:id3
  (:export #:album
           #:artist
           #:audio-info
           #:clear-skipped
           #:comment
           #:comment
           #:desc
           #:encoding
           #:filename
           #:frames
           #:genre
           #:get-frames
           #:id
           #:id3-frame
           #:id3-header
           #:info
           #:is-valid-mp3-file
           #:lang
           #:map-id3-frames
           #:mp3-file
           #:parse-audio-file
           #:picture-info
           #:title
           #:v21-tag-header
           #:val
           #:version
           #:vpprint
           #:year
           #:*skipped-id3-frames*)
  (:use #:common-lisp #:audio-streams #:utils #:iso-639-2))

(defpackage #:abstract-tag
  (:export #:album
           #:album-artist
           #:artist
           #:comment
           #:composer
           #:copyright
           #:encoder
           #:get-id3v1-genre
           #:groups
           #:lyrics
           #:*raw-tags*
           #:title
           #:show-tags
           #:writer)
  (:use #:common-lisp #:audio-streams #:utils))

(defpackage #:mpeg
  (:export #:get-mpeg-audio-info
           #:vpprint)
  (:use #:common-lisp #:audio-streams #:utils))
