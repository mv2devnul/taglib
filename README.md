Copyright (c) 2013, Mark VandenBrink. All rights reserved.

A pure Lisp implementation for reading MPEG-4 audio and MPEG-3 audio tags and audio information.

**Mostly complete.  Your mileage may vary. Most definitely, NOT portable.  Heavily dependent on Clozure CCL.**

Note: There a lot of good (some great) audio file resources out there.  Here are a few of them that I found useful:

* [l-smash](http://code.google.com/p/l-smash/) Exhaustively comprehensive MP4 box parser in C.
* [taglib](http://taglib.github.io/) Clean library in C++.
* [mplayer](http://www.mplayerhq.hu) For me, the definitive tool on how to crack audio files.
* [eyeD3](http://eyed3.nicfit.net/) Great command line tool.
* [MP3Diags](http://mp3diags.sourceforge.net/) Good GUI-based-tool.
* [The MP4 Book](http://www.amazon.com/gp/search?index=books&linkCode=qs&keywords=0130616214) I actually didn't order this until well into writing this code.   What a maroon.

Notes II:

* As the author(s) of taglib state in their comments, parsing ID3s is actually pretty hard. There are so many broken taggers out there
  that it is tough to compensate for all their errors.
* The parsing of MP3 audio properties (mpeg.lisp) is far from complete, especially when dealing with odd case WRT Xing headers.
* I've parsed just enough of the MP4 atoms/boxes to suit the needs of this tool.  l-smash appears to parse all boxes.  Maybe one day this lib will too.
* WRT error handling: in some cases, I've made them recoverable, but in general, I've went down the path of erroring out when
  I get problems.
* I've run this tool across my 19,000+ audio collection and compared the results to some of the tools above, with little to no variations.
  That said, I have a pretty uniform collection, mostly from ripping CDs, then iTunes purchases/matched, and the Amazon matched. YMMV

And now for some sample invocations and outputs:

````
(let (foo)
    (unwind-protect
        (setf foo (parse-mp4-file "01 Keep Yourself Alive.m4a"))
    (when foo (stream-close foo)))    ; make sure underlying open file is closed

	(mp4-tag:show-tags foo))
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
    (when foo (stream-close foo)))    ; make sure underlying open file is closed

	(mp3-tag:show-tags foo :raw t))
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


