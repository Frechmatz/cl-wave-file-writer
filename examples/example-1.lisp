(defpackage :cl-wave-writer-example-1
  (:use :cl))

(in-package :cl-wave-writer-example-1)

(defconstant 2PI (* 2 PI))

(defun make-phase-generator (sample-rate)
  (let ((phi 0.0))
    (flet ((get-delta-phi (frequency)
             (/ (* 2PI frequency) sample-rate)))
      (lambda (frequency)
        (setf phi (rem (+ phi (get-delta-phi frequency)) 2PI))))))

(defun example()
  (let* ((sample-rate 44100) (channel-count 2) (sample-width :16Bit)
         (phase-generator (make-phase-generator sample-rate)))
    ;; Instantiate writer
    (let ((wave-writer (cl-wave-file-writer:make-writer
                        :filename (merge-pathnames "sine.wav" (user-homedir-pathname))
                        :channel-count channel-count
                        :sample-width sample-width
                        :sample-rate sample-rate)))
      ;; Open file
      (funcall (getf wave-writer :open-file))
      ;; Write samples
      (let ((length-seconds 5))
        (dotimes (i (* length-seconds sample-rate))
          (let ((sample (sin (funcall phase-generator 440))))
            (dotimes (c channel-count)
              (funcall (getf wave-writer :write-sample) sample)))))
      ;; Close file
      (funcall (getf wave-writer :close-file))
      "DONE")))

;; (example)
