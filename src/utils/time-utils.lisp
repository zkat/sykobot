(in-package :sykobot)

(defun get-ks-timestamp (&key (zone 0) (time (get-universal-time)))
  (multiple-value-call #'build-string
    "~3$ ks in GMT~@D" (get-ks-time :zone zone :time time)))

(defmacro with-decoded-time (time zone &body body)
  `(multiple-value-bind
         (seconds minutes hours date month year day light timezone)
       (decode-universal-time ,time ,zone)
     (declare (ignore seconds minutes hours date
                      month year day light timezone))
     ,@body))

(defun get-boring-timestamp (&key (zone 0) (time (get-universal-time)))
  (with-decoded-time time zone
    (build-string "~2D:~2D:~2D in GMT~@D" hours minutes seconds
                  (- (mod (+ 11 zone) 24) 11))))

(defun get-datestamp (&key (zone 0) (time (get-universal-time)))
  (with-decoded-time time zone
    (build-string "~[Mon~;Tue~;Wed~;Thu~;Fri~;Sat~;Sun~], ~
                   ~[Jan~;Feb~;Mar~;Apr~;May~;Jun~;Jul~;Aug~;Sep~;Oct~;Nov~;Dec~] ~
                   ~D~[th~;st~;nd~;rd~:;th~], ~D"
                  day (1+ month) date (mod date 10) year)))

(defun get-ks-time (&key (zone 0) (time (get-universal-time)))
  (let ((zone (- (mod (+ 11 zone) 24) 11)))
    (values (with-decoded-time time zone
              (/ (+ seconds (* 60 (+ minutes (* 60 hours)))) 1000.0))
            zone)))