;;;; Copyright 2009 Kat Marchan
;;;;
;;;; This file is part of sykobot.
;;;;
;;;; For licensing and warranty information, refer to COPYING
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package :sykobot)

;;;
;;; Time object
;;; 
;;; - The time object struct represents the decoded time as an object with accessors,
;;;   as opposed to the multiple values of lisp's decoded-time. Some additional functions
;;;   are provided that match the standard's time API.
(defstruct (time (:constructor %make-time))
  seconds minutes hours date month year day dstp zone)

(defun decoded-time-object (time &optional zone)
  "Creates a time struct based on a universal TIME. If ZONE is provided, it's used as the zone
when decoding the universal time. Otherwise, the local timezone is used."
  (multiple-value-bind (seconds minutes hours date month year day dstp zone)
      (decode-universal-time time zone)
    (%make-time :seconds seconds :minutes minutes :hours hours :date date
		:month month :year year :day day :dstp dstp :zone zone)))

(defun get-time-object ()
  "Returns a time struct representing the current local time."
  (decoded-time-object (get-universal-time)))

;;; Kiloseconds
;;; - Being able to easily get at kiloseconds is immensely important. It should be in CLtL3.
(defun decoded-kilosecond-time (time &optional zone)
  "Returns the decoded universal TIME of the day in kiloseconds since midnight.
A zone may be provided."
  (with-slots (seconds minutes hours) (decoded-time-object time zone)
    (/ (+ seconds 
	  (* minutes 60)
	  (* hours 3600))
       1000.0)))

(defun get-kilosecond-time ()
  "Returns the current local time in kiloseconds since midnight."
  (decoded-kilosecond-time (get-universal-time)))

;;; Timestamps
;;; - These make it easy to  generate timestamp strings.
;;;   The API is consistent: each function needs a universal-time as input,
;;;   and accepts an optional ZONE value.
(defun get-kilosecond-timestamp (time &optional (zone 0))
  (build-string "~3$ ks in GMT~@D" (decoded-kilosecond-time time (- zone)) zone))

(defun get-boring-timestamp (time &optional (zone 0))
  (with-slots (hours minutes seconds) (decoded-time-object time (- zone))
    (build-string "~2,'0D:~2,'0D:~2,'0D in GMT~@D" hours minutes seconds
                  (- (mod (+ 11 zone) 24) 11))))

(defun get-datestamp (time &optional (zone 0))
  (with-slots (day month date year) (decoded-time-object time (- zone))
    (build-string "~[Mon~;Tue~;Wed~;Thu~;Fri~;Sat~;Sun~], ~
                   ~[Jan~;Feb~;Mar~;Apr~;May~;Jun~;Jul~;Aug~;Sep~;Oct~;Nov~;Dec~] ~
                   ~D~[th~;st~;nd~;rd~:;th~], ~D"
                  day (1+ month) date (mod date 10) year)))

