(in-package :cl-wave-file-writer)


;;
;; Helper functions for RIFF/Binary writing
;; Copied from Ryan Kings cl-wave library https://github.com/RyanTKing/cl-wave
;;

(defun write-sint (stream sint bytes)
  "Writes a signed integer to the stream with the specified number of bytes."
  (when (< sint 0) (incf sint (expt 2 (* bytes 8))))
  (loop for n below (* bytes 8) by 8 do (write-byte (ldb (byte 8 n) sint) stream)))

(defun write-uint (stream uint bytes)
  "Writes an unsigned integer to the stream with the specified number of bytes."
  (loop for n below (* bytes 8) by 8 do (write-byte (ldb (byte 8 n) uint) stream)))

(defun write-tag (stream tag)
  "Writes a 4-character ASCII tag to the stream."
  (loop for ch across tag do (write-byte (char-code ch) stream)))

;;
;;
;;

(defmacro clip-value (value)
  `(cond
    ((> ,value 1.0)
     1.0)
    ((< ,value -1.0)
     -1.0)
    (t ,value)))

;;
;; Regarding final clipping (after round) see also
;; https://stackoverflow.com/questions/54548304/subtlety-in-converting-doubles-to-a-sound-byte-output
;; We want to allow both +1.0 and -1.0 as input values
;;

(defun value-to-8bit-unsigned (value)
  "value: -1.0 ... 1.0"
  (setf value (clip-value value))
  (setf value (round (* 128 value)))
  (setf value (+ 128 value))
  (cond
    ((< 255 value)
     255)
    ((< value 0)
     0)
    (t
     value)))

(defun value-to-16bit-signed (value)
  "value: -1.0 ... 1.0"
  (setf value (clip-value value))
  (setf value (round (* 32768 value)))
  (cond
    ((< 32767 value)
     32767)
    ((< value -32768)
     -32768)
    (t
     value)))

(defun value-to-24bit-signed (value)
  "value: -1.0 ... 1.0"
  (setf value (clip-value value))
  (setf value (round (* 8388608 value)))
  (cond
    ((< 8388607 value)
     8388607)
    ((< value -8388608)
     -8388608)
    (t
     value)))

;;
;; Wave-File-Writer
;;

(defun make-writer (&key filename channel-count sample-rate (sample-width :16Bit))
  "Creates a Wave-File writer. The function has the following arguments:
   <ul>
      <li>:filename Path of the file to be written. An existing file will be replaced.</li>
      <li>:channel-count Number of channels.</li>
      <li>:sample-rate The sample rate, for example 44100.</li>
      <li>:sample-width The width (resolution) of the samples. One of :8Bit, :16Bit, :24Bit</li>
   </ul>
   Returns a property list with the following keys:
   <ul>
      <li>:open-file A function that opens the file.</li>
      <li>:close-file A function that closes the file.</li>
      <li>:write-sample A function that writes a sample. -1.0 <= sample <= 1.0.</li>
   </ul>"
  (let ((sample-width-mapping
	 (list
	  :8Bit
	  (list
	   :sample-width-bytes 1
	   ;; Unsigned representation for 8 bit waves
	   ;; https://en.wikipedia.org/wiki/WAV
	   ;; https://stackoverflow.com/questions/44415863/what-is-the-byte-format-of-an-8-bit-monaural-wav-file
	   :convert #'value-to-8bit-unsigned)
	  :16Bit
	  (list
	   :sample-width-bytes 2
	   :convert #'value-to-16bit-signed)
	  :24Bit
	  (list
	   :sample-width-bytes 3
	   :convert #'value-to-24bit-signed))))
    (let* ((sample-count 0)
	   (file-output-stream nil)
	   (sample-mapping (getf sample-width-mapping sample-width))
	   (sample-width-bytes (getf sample-mapping :sample-width-bytes))
	   (convert-sample (getf sample-mapping :convert)))
      (labels ((open-file ()
		 (format t "~%Open file ~a~%" filename)
		 (setf file-output-stream
		       (open
			filename
			:element-type 'unsigned-byte
			:direction :io
			:if-exists :supersede
			:if-does-not-exist :create)))
	       (close-file ()
		 (if file-output-stream
		     (progn
		       (format t "~%Close file ~a~%" filename)
		       (close file-output-stream)
		       (setf file-output-stream nil))))
	       (write-sample (value)
		 (write-sint
		  file-output-stream
		  (funcall convert-sample value)
		  sample-width-bytes))
	       (get-fmt-chunk-size (number-of-samples)
		 (declare (ignore number-of-samples))
		 16)
	       (get-data-chunk-size (number-of-samples)
		 (* number-of-samples sample-width-bytes))
	       (get-riff-chunk-size (number-of-samples)
		 (+ 4 (get-data-chunk-size number-of-samples)
		    (get-fmt-chunk-size number-of-samples)))
	       (write-riff-chunk (number-of-samples)
		 (let ((riff-size (get-riff-chunk-size number-of-samples)))
		   (write-tag file-output-stream "RIFF")
		   (write-uint file-output-stream riff-size 4)
		   (write-tag file-output-stream "WAVE")))
	       (write-format-chunk (number-of-samples)
		 (let ((compression-code 1) ;; PCM
		       ;; https://de.wikipedia.org/wiki/RIFF_WAVE
		       (byte-rate (* sample-rate channel-count sample-width-bytes)))
		   (write-tag file-output-stream "fmt ")
		   (write-uint file-output-stream (get-fmt-chunk-size number-of-samples) 4)
		   (write-uint file-output-stream compression-code 2)
		   (write-uint file-output-stream channel-count 2)
		   (write-uint file-output-stream sample-rate 4)
		   (write-uint file-output-stream byte-rate 4)
		   (write-uint file-output-stream sample-width-bytes 2)
		   (write-uint file-output-stream (* sample-width-bytes 8) 2)))
	       (write-data-chunk (number-of-samples)
		 (let ((data-size (get-data-chunk-size number-of-samples)))
		   (write-tag file-output-stream "data")
		   (write-uint file-output-stream data-size 4))))
	(list
	 :open-file (lambda ()
		      (open-file)
		      ;; Write preliminary chunks
		      (write-riff-chunk 0)
		      (write-format-chunk 0)
		      (write-data-chunk 0))
	 :write-sample (lambda (sample)
			 "sample: -1.0 ... 1.0"
			 (setf sample-count (+ 1 sample-count))
			 (write-sample sample))
	 :close-file (lambda ()
		       ;; Seek to start of file and update chunks
		       (file-position file-output-stream :start)
		       (write-riff-chunk sample-count)
		       (write-format-chunk sample-count)
		       (write-data-chunk sample-count)
		       (close-file)))))))

