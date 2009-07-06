(in-package :sykobot)

;;; Facts
(defparameter *facts-file-path* (ensure-directories-exist
                                 (merge-pathnames ".sykobot/fact-table.db" (user-homedir-pathname))))
(let ((fact-table (make-hash-table :test #'equalp)))

  (defun set-fact (noun info)
    (setf (gethash noun fact-table) info)
    (save-facts))
  
  (defun get-fact (noun)
    (multiple-value-bind (info hasp)
        (gethash noun fact-table)
      (if hasp
          info
          (format nil "I know nothing about ~A" noun))))

  (defun erase-all-facts ()
    (clrhash fact-table)
    (save-facts))

  (defun save-facts ()
    (cl-store:store fact-table *facts-file-path*))

  (defun load-facts ()
    (setf fact-table (if (probe-file *facts-file-path*)
                         (cl-store:restore *facts-file-path*)
                         (make-hash-table :test #'equalp))))
  ) ;end fact table

(defun split-into-sub-statements (statement)
  (split "\\s*(,|but|however|whereas|although|\\;|\\.)\\s*" statement))

(deflistener scan-for-fact
  (loop for statement in (split-into-sub-statements *message*)
     do (do-register-groups (article noun verb info)
            (".*?([a|an|the|this|that]*)\\s*(\\w+)\\s+(is|are|isn't|ain't)\\s+(.+)"
             statement)
          (set-fact noun (format nil "~A ~A ~A ~A" article noun verb info)))))

(defcommand fact ("(\\S+)*" topic)
  (cmd-msg (get-fact topic)))

