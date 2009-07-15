(in-package :sykobot)

(defproto karma-bot ((proto 'command-bot))
  ((karma (make-hash-table :test #'equalp))))

(defreply init-sheep :after ((bot (proto 'karma-bot)) &key)
	  (setf (karma bot) (make-hash-table :test #'equalp)))

(defreply init-bot :after ((bot (proto 'karma-bot)))
	  (load-karma bot))

(defun karma-db (bot)
  (merge-pathnames "karma-table.db" (bot-dir bot)))

(defmessage load-karma (bot))
(defmessage save-karma (bot))

(defreply load-karma ((bot (proto 'karma-bot)))
  (when (probe-file (karma-db bot))
    (setf (karma bot)
	  (cl-store:restore (karma-db bot)))))

(defreply save-karma ((bot (proto 'karma-bot)))
  (cl-store:store (karma bot) (karma-db bot)))

(defun make-karma-record (receiver giver &key (positive t))
  (list receiver giver positive (get-universal-time)))

(defmessage add-karma (bot receiver karma))
(defmessage give-karma (bot receiver giver))
(defmessage give-unkarma (bot receiver giver))

(defreply add-karma ((bot (proto 'karma-bot)) receiver karma)
  (push karma (gethash receiver (karma bot))))
(defreply add-karma :after ((bot (proto 'karma-bot)) receiver karma)
  (declare (ignore karma receiver))
  (save-karma bot))

(defreply give-karma ((bot (proto 'karma-bot)) receiver giver)
  (add-karma bot receiver (make-karma-record receiver giver)))

(defreply give-unkarma ((bot (proto 'karma-bot)) receiver giver)
  (add-karma bot receiver (make-karma-record receiver giver :positive nil)))

(defmessage calculate-base-karma (bot nick))
(defreply calculate-base-karma ((bot (proto 'karma-bot)) nick)
  (let ((karma 0))
    (loop for k-record in (gethash nick (karma bot))
	 do (if (elt k-record 2)
		(setf karma (1+ karma))
		(setf karma (1- karma))))
    karma))

(defmessage calculate-adjusted-karma (bot nick))
(defreply calculate-adjusted-karma ((bot (proto 'karma-bot)) nick)
  (let* ((base-karma (calculate-base-karma bot nick))
	 (karma base-karma))
    (loop for k-record in (gethash nick (karma bot))
	 do (destructuring-bind (receiver giver positive time) k-record
	      (declare (ignore receiver positive time))
	      (let ((giver-karma (calculate-base-karma bot giver)))
		(cond 
		  ((< 0 giver-karma) (setf karma (+ karma (/ giver-karma (if (= 0 base-karma)
									     1 base-karma)))))
		  ((> 0 giver-karma) (setf karma (+ karma (expt 2 giver-karma))))
		  (t (setf karma (+ karma (/ 1 (if (= 0 base-karma) 1 base-karma)))))))))
    karma))

(defcommand praise ("(.+)" nick)
  (if (string-equal nick *sender*)
      (progn
	(give-unkarma *bot* nick *sender*)
	"Self-glorifying dorks get no love.")
      (progn
	(give-karma *bot* nick *sender*)
	(build-string "ALL PRAISE ~:@(~A~)" nick))))
(defcommand unpraise ("(.+)" nick)
  (give-unkarma *bot* nick *sender*)
  (build-string "~A shitsux" nick))

(defcommand karma ("(.+)" nick)
  (build-string "~2$" (calculate-adjusted-karma *bot* nick)))


