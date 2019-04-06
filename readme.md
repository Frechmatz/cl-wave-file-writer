cl-wave-file-writer
===================

A Common Lisp library for writing wave (.wav) files. The writer does not buffer data, so files of arbitrary size can be written.

**Example:**

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

API
---

**cl-wave-file-writer:make-writer** &key filename channel-count sample-rate (sample-width :16bit)

Creates a Wave-File output writer. The function has the following arguments:

*   :filename Path of the file to be written.
*   :channel-count Number of channels of the Wave file.
*   :sample-rate The sample rate, for example 44100.
*   :sample-width The resolution of the samples. One of :8Bit, :16Bit, :24Bit

Returns a property list with the following keys:

*   :open-file A function that opens the file.
*   :close-file A function that closes the file.
*   :write-sample A function that writes a sample. -1.0 <= sample <= 1.0.

Acknowledgements
----------------

*   The library uses code taken from Ryan Kings [cl-wave](https://github.com/RyanTKing/cl-wave) library.

* * *

Generated 2019-04-06 02:52:29