;;;; Unit tests for src/utils/time-utils.lisp
(in-package :sykobot)

(def-suite time-utils)

(def-suite time-objects :in time-utils)
(in-suite time-objects)

(test decoded-time-object
  (is (time-p (decoded-time-object (get-universal-time))))
  (let* ((universal-time (get-universal-time))
	 (time-object (decoded-time-object universal-time)))
    (multiple-value-bind (seconds minutes hours date month year day dstp zone)
	(decode-universal-time universal-time)
      (is (= seconds (time-seconds time-object)))
      (is (= minutes (time-minutes time-object)))
      (is (= hours (time-hours time-object)))
      (is (= date (time-date time-object)))
      (is (= month (time-month time-object)))
      (is (= year (time-year time-object)))
      (is (= day (time-day time-object)))
      (is (eq dstp (time-dstp time-object)))
      (is (= zone (time-zone time-object))))))

(test get-time-object
  (let ((time-object (get-time-object)))
    (multiple-value-bind (seconds minutes hours date month year day dstp zone)
	(get-decoded-time)
      ;; seconds are ignored because GET-TIME-OBJECT and GET-DECODED-TIME might not execute
      ;; within the same second, thus randomly causing the test to fail.
      ;; I'm willing to put quite a bit of faith in get-time-object if all other tests pass.
      (declare (ignore seconds)) 
      (is (= minutes (time-minutes time-object)))
      (is (= hours (time-hours time-object)))
      (is (= date (time-date time-object)))
      (is (= month (time-month time-object)))
      (is (= year (time-year time-object)))
      (is (= day (time-day time-object)))
      (is (eq dstp (time-dstp time-object)))
      (is (= zone (time-zone time-object))))))

(test decoded-kilosecond-time
  (let* ((universal-time (get-universal-time))
	 (time-object (decoded-time-object universal-time)))
    (multiple-value-bind (seconds minutes hours date month year day dstp zone)
	(decode-universal-time universal-time)
      (is (= seconds (time-seconds time-object)))
      (is (= minutes (time-minutes time-object)))
      (is (= hours (time-hours time-object)))
      (is (= date (time-date time-object)))
      (is (= month (time-month time-object)))
      (is (= year (time-year time-object)))
      (is (= day (time-day time-object)))
      (is (eq dstp (time-dstp time-object)))
      (is (= zone (time-zone time-object))))))