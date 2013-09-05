(defun profile-on ()
  (dolist (p '("MP4-ATOM" "MPEG" "AUDIO-STREAMS" "ID3-FRAME" "UTILS" "LOGGING" "ISO-639-2" "MP3-TAG" "MP4-TAG"))
    (let ((pkg (find-package p)))
      (mon:monitor-all pkg)
      (format t "Package ~a, ~:d~%" pkg (length mon:*monitored-functions*)))))

(defun profile-report ()
  (mon:report :nested :inclusive :threshold 0.0 :names :all))

(defun profile-reset ()
  (mon:reset-all-monitoring))

(defun profile-off ()
  (mon:unmonitor))
