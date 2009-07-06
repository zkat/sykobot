(in-package :sykobot)

;;; Memos
(defcommand memo ("for (\\S+): (.*)" recipient memo)
  (add-memo recipient memo *sender*)
  (cmd-msg "Tada! Added memo for ~A. ~
            I'll let them know next time they speak."
           recipient))

(defparameter *memos-file-path* (ensure-directories-exist
                                 (merge-pathnames ".sykobot/memo-table.db" (user-homedir-pathname))))
(defproto memo ()
  ((recipient nil)
   (sender nil)
   (text nil)
   (time-added (get-universal-time))))

(defmessage load-memos (bot))
(defreply load-memos ((bot (proto 'sykobot)))
  (unless (has-property-p bot 'memos)
    (add-property bot 'memos (make-hash-table :test #'equalp)))
  (when (probe-file *memos-file-path*)
    (setf (memos bot)
          (cl-store:restore *memos-file-path*))))

(defmessage save-memos (bot))
(defreply save-memos ((bot (proto 'sykobot)))
  (cl-store:store (memos bot) *memos-file-path*))

(defmessage add-memo (bot recipient memo-text sender))
(defreply add-memo ((bot (proto 'sykobot)) recipient text sender)
  (pushnew (gethash recipient (memos bot))
           (defclone ((proto 'memo))
               ((recipient recipient)
                (sender sender)
                (memo memo)
                (time-added (get-universal-time))))))
(defreply add-memo :after ((bot (proto 'sykobot)) recipient text sender)
  (declare (ignore recipient text sender))
  (save-memos bot))

(defmessage remove-memo (bot memo))
(defreply remove-memo ((bot (proto 'sykobot))
                       (memo (proto 'memo)))
  (setf (gethash (recipient memo) (memos bot))
        (delete memo (gethash (recipient memo) (memos bot)))))
(defreply remove-memo :after ((bot (proto 'sykobot))
                              (memo (proto 'memo)))
  (declare (ignore memo))
  (save-memos bot))

(defmessage erase-all-memos (bot))
(defreply erase-all-memos ((bot (proto 'sykobot)))
  (clrhash (memos bot)))
(defreply erase-all-memos :after ((bot (proto 'sykobot)))
  (save-memos bot))

(deflistener send-memos
  (let ((memos (get-memos *sender*)))
    (when memos
      (loop for memo in memos
         do (destructuring-bind (text who-from) memo
              (cmd-reply "Memo from ~A - \"~A\"" who-from text))
           (remove-memo memo)))))
