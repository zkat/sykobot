(in-package :sykobot)

(defun get-ks-timestamp (&key (zone 0) (time (get-universal-time)))
  (build-string "~3$ ks in GMT~@D" (get-kiloseconds :zone zone :time time) zone))

(defstruct (time (:constructor %make-time))
  seconds minutes hours date month year day dstp zone)

(defun decoded-time-object (time &optional (zone 0))
  (multiple-value-bind (seconds minutes hours date month year day dstp zone)
      (decode-universal-time time zone)
    (%make-time :seconds seconds :minutes minutes :hours hours :date date
		:month month :year year :day day :dstp dstp :zone zone)))

(defun get-time-object ()
  (decoded-time-object (get-universal-time)))

(defun get-boring-timestamp (&key (zone 0) (time (get-universal-time)))
  (with-slots (hours minutes seconds) (decoded-time-object time zone)
    (build-string "~2D:~2D:~2D in GMT~@D" hours minutes seconds
                  (- (mod (+ 11 zone) 24) 11))))

(defun get-datestamp (&key (zone 0) (time (get-universal-time)))
  (with-slots (day month date year) (decoded-time-object time zone)
    (build-string "~[Mon~;Tue~;Wed~;Thu~;Fri~;Sat~;Sun~], ~
                   ~[Jan~;Feb~;Mar~;Apr~;May~;Jun~;Jul~;Aug~;Sep~;Oct~;Nov~;Dec~] ~
                   ~D~[th~;st~;nd~;rd~:;th~], ~D"
                  day (1+ month) date (mod date 10) year)))

(defun get-kiloseconds (&key (zone 0) (time (get-universal-time)))
  (with-slots (seconds minutes hours) (decoded-time-object time zone)
    (/ (+ seconds 
	  (* minutes 60)
	  (* hours 3600))
       1000.0)))