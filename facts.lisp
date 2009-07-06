(in-package :sykobot)

;;; Facts
(defcommand fact ("(\\S+)*" topic)
  (cmd-msg (get-fact topic)))

(defparameter *facts-file-path* (ensure-directories-exist
                                 (merge-pathnames ".sykobot/fact-table.db" (user-homedir-pathname))))

(defproto fact ()
  ((noun "")
   (info "")))

(defmessage load-facts (bot))
(defmessage save-facts (bot))
(defmessage set-fact (bot noun info))
(defmessage get-fact (bot noun))
(defmessage erase-all-facts (bot))

(defreply load-facts ((bot (proto 'sykobot)))
  (when (probe-file *facts-file-path*)
    (setf (facts bot)
          (cl-store:restore *facts-file-path*))))

(defreply save-facts ((bot (proto 'sykobot)))
  (cl-store:store (facts bot) *facts-file-path*))

(defreply set-fact ((bot (proto 'sykobot))
                    noun info)
  (setf (gethash noun (facts bot)) info))
(defreply set-fact :after ((bot (proto 'sykobot))
                           noun info)
  (declare (ignore noun info))
  (save-facts bot))

(defreply get-fact ((bot (proto 'sykobot)) noun)
  (multiple-value-bind (info hasp)
      (gethash noun (facts bot))
    (if hasp
        info
        (format nil "I know nothing about ~A" noun))))

(defreply erase-all-facts ((bot (proto 'sykobot)))
  (clrhash (facts bot)))
(defreply erase-all-facts :after ((bot (proto 'sykobot)))
  (save-facts bot))

(defun split-into-sub-statements (statement)
  (split "\\s*(,|but|however|whereas|although|\\;|\\.)\\s*" statement))

(deflistener scan-for-fact
  (loop for statement in (split-into-sub-statements *message*)
     do (do-register-groups (article noun verb info)
            (".*?([a|an|the|this|that]*)\\s*(\\w+)\\s+(is|are|isn't|ain't)\\s+(.+)"
             statement)
          (set-fact *bot* noun (format nil "~A ~A ~A ~A" article noun verb info)))))



