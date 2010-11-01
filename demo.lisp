
(in-package #:cipht/sdl)

;;;; DEMO RECORDING

;;; XXX needs to be rewritten to just log SDL events

;;; demo file format:
;; (:event elapsed-time typesym value mods)
;; (:random-state random-state)
;;
;;; At some point this might become a binary format just to speed up
;;; demo recording.

(defun demo-log (type &rest stuff)
  (format *demo-recording-stream* "~&(~S~{ ~S~})" type stuff))

(defun read-demo-log () (read *demo-playback-stream* nil nil))

(defmacro with-demo-recording ((demo-file) &body body)
  `(with-open-file (*demo-recording-stream* ,demo-file :direction :output
					    :if-exists :supersede
					    :if-does-not-exist :create)
     ,@body))

(declaim (inline update-demo-playback update-demo-recording))
(defun update-demo-recording (elapsed-time)
  (declare (single-float elapsed-time))
  (push elapsed-time *demo-frame-list*))

;;;; DEMO PLAYBACK

;; eventually you could enhance this to adjust event speed based on
;; where frames happened in the origin; for most purposes, though,
;; this should work fine (and anything depending on frame-locked
;; behavior is a bug, anyway).
(defmacro with-demo-playback ((demo-file &key (n-post-demo-frames 1000)) &body body)
  `(with-open-file (*demo-playback-stream* ,demo-file)
     (let ((*demo-playback-total-time* 0)
	   (*demo-playback-next-event-time* 0)
	   (*demo-events-to-replay* (make-queue))
	   (*demo-playback-post-demo-frames* ,n-post-demo-frames))
       ,@body)))

(defun update-demo-playback (elapsed-time)
  (declare (single-float elapsed-time))
  ;; read frames, accumulating events until we reach future frames
  (incf *demo-playback-total-time* elapsed-time)
  (let (done?)
    (when (> *demo-playback-total-time* *demo-playback-next-event-time*)
      (loop for entry = (read-demo-log)
	    while entry
	    do (case (first entry)
		 (:cursor (warp-cursor (second entry) (third entry)))
		 (:event (enqueue (rest entry) *demo-events-to-replay*))
		 (:frames (incf *demo-playback-next-event-time* (apply #'+ (rest entry)))))
	    while (> *demo-playback-total-time* *demo-playback-next-event-time*)
	    finally (unless entry (setf done? t))))
    (when done?
      (sinkf *demo-playback-post-demo-frames* 1)
      (zerop *demo-playback-post-demo-frames*))))

