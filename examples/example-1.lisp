(defpackage :cl-wave-file-writer-example-1
  (:use :cl))

(in-package :cl-wave-file-writer-example-1)

(defconstant 2PI (* 2 PI))

(defun make-phase-generator (sample-rate)
  (let ((phi 0.0))
    (flet ((get-delta-phi (frequency)
             (/ (* 2PI frequency) sample-rate)))
      (lambda (frequency)
        (setf phi (rem (+ phi (get-delta-phi frequency)) 2PI))
	phi))))

(defun example()
  (let* ((sample-rate 44100)
         (phase-generator (make-phase-generator sample-rate)))
    ;; Instantiate writer
    (let ((wave-writer (cl-wave-file-writer:make-writer
                        :filename (merge-pathnames
				   "cl-wave-file-writer-example-1.wav"
				   (user-homedir-pathname))
                        :channel-count 2
                        :sample-width :16Bit
                        :sample-rate sample-rate)))
      ;; Open file
      (funcall (getf wave-writer :open-file))
      ;; Write samples
      (let ((length-seconds 2) (frequency 5))
        (dotimes (i (* length-seconds sample-rate))
          (let ((phase (funcall phase-generator frequency)))
            (funcall (getf wave-writer :write-sample) (sin phase))
            (funcall (getf wave-writer :write-sample) (cos phase)))))
      ;; Close file
      (funcall (getf wave-writer :close-file))
      "DONE")))

;; (example)
