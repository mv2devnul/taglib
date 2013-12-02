Copyright (c) 2013, Mark VandenBrink. All rights reserved.

# Introduction

A pure Lisp implementation for reading audio tags and audio information.

Currently reads MP3/MP4/FLAC audio files.

Runs (in single-thread mode) under CCL, SBCL, CLISP, and ABCL
Note: my primary Lisp variant is CCL, so it's the most tested; however,
this code should run on any Lisp that is supported by FLEXI-STREAMS

# Dependencies

All avalailable via quicklisp

* optima and optima.ppcre: for quick parsing of FLAC tags
* flexi-streams: for in-memory streams

# References

Note: There a lot of good (some great) audio file resources out there.  Here are a few of them that I found useful:

* [l-smash](http://code.google.com/p/l-smash/): Exhaustively comprehensive MP4 box parser in C.
* [taglib](http://taglib.github.io/): Clean library in C++.
* [mplayer](http://www.mplayerhq.hu): For me, the definitive tool on how to crack audio files.
* [eyeD3](http://eyed3.nicfit.net/): Great command line tool.
* [MP3Diags](http://mp3diags.sourceforge.net/): Good GUI-based-tool.  Tends to slow, but very thorough.
* [MediaInfo](http://mediaarea.net/en/MediaInfo): C++, can dump out all the info to command line and also has a GUI.
* [The MP4 Book](http://www.amazon.com/gp/search?index=books&linkCode=qs&keywords=0130616214): I actually didn't order this until well into writing this code.   What a maroon.
  It would have saved me TONS of time.

# General Notes

* As the author(s) of taglib state in their comments, parsing ID3s is actually pretty hard. There are so many broken taggers out there
  that it is tough to compensate for all their errors.
* I've parsed just enough of the MP4 atoms/boxes to suit the needs of this tool.  l-smash appears to parse all boxes.  Maybe one day this lib will too.
* WRT error handling: in some cases, I've made them recoverable, but in general, I've went down the path of erroring out when
  I get problems.
* I've run this tool across my 21,000+ audio collection and compared the results to some of the tools above, with little to no variations.
  That said, I have a pretty uniform collection, mostly from ripping CDs, then iTunes-purchases/matched, and then Amazon-matched. YMMV.
* Parsing the CBR audio info in an MP3 is hideously inefficient if done exhaustively.  Instead, this library, only looks at the first
  MPEG frame and calculates the duration, etc from that.  In addition, if you just want TAG info, you can bind AUDIO-STREAMS:*get-audio-info* to nil.

Things to consider adding/changing:

* Add more file types.
* Add writing of tags.
* Improve error handling.
* Implement a DSL ala Practical Common Lisp.

# Sample Invocations and Results

````
(let (foo)
    (unwind-protect
        (setf foo (parse-mp4-file "01 Keep Yourself Alive.m4a"))
    (when foo
	    (mp4-tag:show-tags foo)
		(stream-close foo)))

````

Yields:

```
01 Keep Yourself Alive.m4a
sample rate: 44100.0 Hz, # channels: 2, bits-per-sample: 16, max bit-rate: 314 Kbps, avg bit-rate: 256 Kbps, duration: 4:03
    album: Queen I
    album-artist: Queen
    artist: Queen
    compilation: no
    disk: (1 1)
    genre: 80 (Hard Rock)
    title: Keep Yourself Alive
    track: (1 11)
    year: 1973
```

The show-tags methods also have a "raw" capability.  Example:

```
(let (foo)
    (unwind-protect
        (setf foo (parse-mp3-file "Queen/At the BBC/06 Great King Rat.mp3"))
    (when foo
		  (mp3-tag:show-tags foo :raw t)
		   (stream-close foo)))

```

Yields:

```
Queen/At the BBC/06 Great King Rat.mp3: MPEG 1, Layer III, VBR, sample rate: 44,100 Hz, bit rate: 128 Kbps, duration: 5:60
Header: version/revision: 3/0, flags: 0x00: 0/0/0/0, size = 11,899 bytes; No extended header; No V21 tag
    Frames[9]:
        frame-text-info: flags: 0x0000: 0/0/0/0/0/0, offset: 0, version = 3, id: TIT2, len: 15, NIL, encoding = 0, info = <Great King Rat>
        frame-text-info: flags: 0x0000: 0/0/0/0/0/0, offset: 25, version = 3, id: TPE1, len: 6, NIL, encoding = 0, info = <Queen>
        frame-text-info: flags: 0x0000: 0/0/0/0/0/0, offset: 41, version = 3, id: TPE2, len: 6, NIL, encoding = 0, info = <Queen>
        frame-text-info: flags: 0x0000: 0/0/0/0/0/0, offset: 57, version = 3, id: TALB, len: 11, NIL, encoding = 0, info = <At the BBC>
        frame-text-info: flags: 0x0000: 0/0/0/0/0/0, offset: 78, version = 3, id: TRCK, len: 4, NIL, encoding = 0, info = <6/8>
        frame-text-info: flags: 0x0000: 0/0/0/0/0/0, offset: 92, version = 3, id: TPOS, len: 4, NIL, encoding = 0, info = <1/1>
        frame-text-info: flags: 0x0000: 0/0/0/0/0/0, offset: 106, version = 3, id: TYER, len: 5, NIL, encoding = 0, info = <1995>
        frame-text-info: flags: 0x0000: 0/0/0/0/0/0, offset: 121, version = 3, id: TCON, len: 5, NIL, encoding = 0, info = <(79)>
        frame-txxx: flags: 0x0000: 0/0/0/0/0/0, offset: 136, version = 3, id: TXXX, len: 33, NIL, <Tagging time/2013-08-08T16:38:38>
```

# Design

## The Files

* __audio-streams.lisp:__ creates a STREAM-like interface to audio files and vectors, thus read/seek devolve into
  simple array-references.  Under CCL, uses MAP-FILE-TO-OCTET-VECTOR function to mmap the file. Other Lisps just
  slurp in the whole file (probably should revisit this, but since we use displaced arrays for dissecting the file,
  this would require a rewrite.
* __flac-frame.lisp:__ Parses FLAC files.
* __id3-frame.lisp:__ Parses the ID3 frames in an MP3 file.
   For each frame type we are interested in, DEFCLASS a class with
   specfic naming convention: frame-xxx/frame-xxxx, where xxx is valid ID3V2.2 frame name
   and xxxx is a valid ID3V2.[34] frame name.  Upon finding a frame name in an MP3 file,
   we can then do a FIND-CLASS on the "frame-xxx", and a MAKE-INSTANCE on the found class
   to read in that class (each defined class is assumed to have an INITIALIZE-INSTANCE method
   that reads in data to build class).

   For any class we don't want to parse (eg, haven't gotten around to it yet, etc), we create
   a RAW-FRAME class that can be subclassed.  RAW-FRAME simply reads in the frame header, and then
   the frame "payload" as raw OCTETS.

   Each frame class assumes that the STREAM being passed has been made sync-safe.

* __iso-639-2.lisp:__ Converts ISO-639-2 3-character languages into longer, more descriptive strings.
* __abstract-tag.lisp:__ The abstract interface for ID3 tags for audio files. The abstract interface is simply one of the following:
	* __album:__ Returns the name of the album.
	* __album-artist:__ Returns the name of album artist.
	* __artist:__ Returns recording artist.
	* __comment:__ Returns any comments found in file.
	* __compilation:__ A boolean indicating whether this file is part of a compilation.
	* __composer:__  Returns the composer of this file.
	* __copyright:__ Returns copyright info.
	* __disk:__ Returns the disk number of this file.  If present, may be a single number or two numbers (ie disk 1 of 2).
	* __encoder:__ Returns the tool used to encode this file.
	* __genre:__ Returns the genre of this file.
	* __groups:__ iTunes tag that allows you to group tracks.
	* __lyrics:__ Returns any (unsynchronized) lyrics found in this file.
	* __tempo:__ Returns the tempo of this file.
	* __title:__ Returns the name of the the song in this file.
	* __track:__ Returns the track number of this file.  Like *disk*, if present, may be a single number or two numbers (ie track 1 of 20).
	* __writer:__ Returns name of who wrote this song.
	* __year:__ Returns the year when the song was recorded.

* __mp4-atom.lisp:__ Parses MP4 audio files.  Similar logic to __id-frame.lisp__, but has two main differnces: first,
  it returns a tree structure (needed, since, that's how M4A atoms/boxes work), and secondly, has an *atom-skip* class
  that records the name and position of an atom, but seeks to the next atom rather than reading in contents.

  As noted in the comments of this file, there are three kinds of "boxes/atoms":
    * Pure container atoms: have no data and are only used to contain other atoms.  This is akin to a UNIX filesystem's directory notion.
    * Pure "data" atoms: has no nested atoms.  Only has a payload.
    * A mixture of both.
* __tree.lisp:__ The tree library (used by mp4-atom).  Adapted from
* __mpeg.lisp:__ Parses the audio information (ie the non-ID3 info) in an MP3 file.
* __packages.lisp:__ Holds all the package definitions.
* __taglib-tests.asd:__ Contains build instructions for taglib-tests.lisp.  An ASDF file.
* __taglib-tests.lisp:__ Some test invocations.
* __taglib.asd:__ Contains build instructions for taglib library.  An ASDF file.
* __utils.lisp:__ General utility functions/methods needed by this library.

## Known gotchas

* In previous versions of this library, if we ran into a Xing header with the number of frames set to zero, I just set the duration
  as 0.  This was fast, but not really "nice."  In new revs, I decided that if I hit a bad Xing header, the correct thing to do is to
  read every frame and calculate the duration/avg bit-rate that way.  The resulting calculations match those of iTunes, but it can __really__ slow
  things down.

* More info later...

## Experimental Stuff

**Multi-threading currently broken.

I've recently added some (very) rudimentary multi-threading (see taglib-tests.lisp) using the CHANL package.

CURRENTLY BROKEN

First, the filesystem
walker (main thread) walks the requested directory, adding each filename to an unbounded channel (\*channel\*).  The main thread then sends
\*MAX-THREADS\* \*END-THREAD\* symbols, creates \*MAX-THREADS\* worker threads who read from the channel, and then sits in a loop reading
from \*dead-channel\* until it has done \*MAX-THREADS\* recv's.

The worker threads parse the filename they retrieve from \*channel\* until they get the \*END-THREAD\* symbol, whereupon they write their thread
id to \*dead-channel\* and return (ie exit). Here are some preliminary timings:

| # Threads   | Time (seconds) |
| ----------- | -------------- |
|          10 |            ~28 |
|           5 |            ~18 |
|           2 |            ~17 |
|           1 |            ~25 |

Note: threading does NOT currently work on ECL (missing support in bordeaux threads)
or on my custom built CLISP with POSIX-THREADS turned on (fails with RECURSIVE MUTEX error).
ABCL kinda/sorta works (seems to be a problem with ATOM-TYPE being unbound
