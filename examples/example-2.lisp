(defpackage :cl-wave-file-writer-example-2
  (:use :cl))

(in-package :cl-wave-file-writer-example-2)

(defparameter *files*
  '((192000 2 :24Bit)
    (44100 1 :16Bit)
    (44100 2 :16Bit)
    (44100 8 :16Bit)
    (22050 1 :8Bit)
    (22050 2 :8Bit)
    (22050 8 :8Bit)
    (11025 1 :8Bit)))

(defun make-phase-generator (sample-rate)
  (let ((phi 0.0))
    (lambda (frequency)
      (setf phi (rem (+ phi (/ (* 2 PI frequency) sample-rate)) (* 2 PI)))
      phi)))

(defun example()
  (dolist (f *files*)
    (let* ((sample-rate (first f))
	   (channel-count (second f))
	   (sample-width (third f))
	   (phase-generator (make-phase-generator sample-rate))
	   (wave-writer
	    (cl-wave-file-writer:make-writer
	     :filename (merge-pathnames
			(format nil "cl-wave-file-writer-example-2-~a-~a-~a.wav"
				sample-rate sample-width channel-count)
			(user-homedir-pathname))
	     :channel-count channel-count
	     :sample-width sample-width
	     :sample-rate sample-rate)))
      (funcall (getf wave-writer :open-file))
      (let ((length-seconds 2) (frequency 5))
	(dotimes (i (* length-seconds sample-rate))
	  (let ((phase (funcall phase-generator frequency)))
	    (dotimes (i channel-count)
	      (funcall
	       (getf wave-writer :write-sample)
	       (if (evenp i) (sin phase) (cos phase)))))))
      (funcall (getf wave-writer :close-file))))
  "DONE")

;; (example)
