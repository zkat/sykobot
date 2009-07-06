(in-package :sykobot)

;;; Memos
(defcommand memo ("for (\\S+): (.*)" recipient memo)
  (add-memo *bot* recipient memo *sender*)
  (cmd-msg "Tada! Added memo for ~A. ~
            I'll let them know next time they speak."
           recipient))

(defparameter *memos-file-path* (ensure-directories-exist
                                 (merge-pathnames ".sykobot/memo-table.db" (user-homedir-pathname))))

(defun make-memo (recipient sender text)
  (list recipient sender text (get-universal-time)))

(defmessage save-memos (bot))
(defmessage load-memos (bot))
(defmessage add-memo (bot recipient memo-text sender))
(defmessage remove-memo (bot memo))
(defmessage memos-for (bot recipient))
(defmessage erase-all-memos (bot))

(defreply load-memos ((bot (proto 'sykobot)))
  (when (probe-file *memos-file-path*)
    (setf (memos bot)
          (cl-store:restore *memos-file-path*))))

(defreply save-memos ((bot (proto 'sykobot)))
  (cl-store:store (memos bot) *memos-file-path*))

(defreply add-memo ((bot (proto 'sykobot)) recipient text sender)
  (pushnew (make-memo recipient sender text) (gethash recipient (memos bot)) :test #'equalp))
(defreply add-memo :after ((bot (proto 'sykobot)) recipient text sender)
  (declare (ignore recipient text sender))
  (save-memos bot))

(defreply remove-memo ((bot (proto 'sykobot)) memo)
  (setf (gethash (car memo) (memos bot))
        (delete memo (gethash (car memo) (memos bot)) :test #'equalp)))
(defreply remove-memo :after ((bot (proto 'sykobot)) memo)
  (declare (ignore memo))
  (save-memos bot))

(defreply erase-all-memos ((bot (proto 'sykobot)))
  (clrhash (memos bot)))
(defreply erase-all-memos :after ((bot (proto 'sykobot)))
  (save-memos bot))

(defreply memos-for ((bot (proto 'sykobot)) recipient)
  (gethash recipient (memos bot)))

(deflistener send-memos
  (let ((memos (memos-for *bot* *sender*)))
    (loop for memo in memos
       do (destructuring-bind (recipient sender text time-added) memo
            (declare (ignore recipient time-added))
            (cmd-reply "Memo from ~A - \"~A\"" sender text))
       (remove-memo *bot* memo))))

