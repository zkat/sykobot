;;;; Copyright 2009 Kat Marchan
;;;;
;;;; This file is part of sykobot.
;;;;
;;;; For licensing and warranty information, refer to COPYING
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package :sykobot)

;;; Memos

(defproto memos-bot ((proto 'command-bot))
  ((memos (make-hash-table :test #'equalp))))

(defreply init-sheep :after ((bot (proto 'memos-bot)) &key)
  (setf (memos bot) (make-hash-table :test #'equalp)))

(defreply init-bot :after ((bot (proto 'memos-bot)))
	  (load-memos bot))

(defcommand memo ("for (\\S+)\\s*: (.*)" recipient memo)
  (add-memo *bot* recipient memo *sender*)
  (build-string "Tada! Added memo for ~A. ~
            I'll let them know next time they speak."
		recipient))

(defun memos-db (bot)
  (merge-pathnames "memo-table.db" (bot-dir bot)))

(defun make-memo (recipient sender text)
  (list recipient sender text (get-universal-time)))

(defmessage save-memos (bot))
(defmessage load-memos (bot))
(defmessage add-memo (bot recipient memo-text sender))
(defmessage remove-memo (bot memo))
(defmessage memos-for (bot recipient))
(defmessage erase-all-memos (bot))

(defreply load-memos ((bot (proto 'memos-bot)))
  (when (probe-file (memos-db bot))
    (setf (memos bot)
          (cl-store:restore (memos-db bot)))))

(defreply save-memos ((bot (proto 'memos-bot)))
  (cl-store:store (memos bot) (memos-db bot)))

(defreply add-memo ((bot (proto 'memos-bot)) recipient text sender)
  (pushnew (make-memo recipient sender text)
           (gethash recipient (memos bot))
           :test #'equalp))
(defreply add-memo :after ((bot (proto 'memos-bot)) recipient text sender)
  (declare (ignore recipient text sender))
  (save-memos bot))

(defreply remove-memo ((bot (proto 'memos-bot)) memo)
  (setf (gethash (car memo) (memos bot))
        (delete memo (gethash (car memo) (memos bot)) :test #'equalp)))
(defreply remove-memo :after ((bot (proto 'memos-bot)) memo)
  (declare (ignore memo))
  (save-memos bot))

(defreply erase-all-memos ((bot (proto 'memos-bot)))
  (clrhash (memos bot)))
(defreply erase-all-memos :after ((bot (proto 'memos-bot)))
  (save-memos bot))

(defreply memos-for ((bot (proto 'memos-bot)) recipient)
  (nth-value 0 (gethash recipient (memos bot))))

(deflistener send-memos
  (let ((memos (memos-for *bot* *sender*)))
    (loop for memo in (reverse memos)
       do (destructuring-bind (recipient sender text time-added) memo
            (declare (ignore recipient time-added))
            (send-reply *bot* *channel* *sender* (build-string "Memo from ~A - \"~A\"" sender text)))
         (remove-memo *bot* memo))))

