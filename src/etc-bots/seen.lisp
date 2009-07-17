
(in-package :sykobot)

;;; Config options



;;; Seen

(defproto seen-bot ((proto 'command-bot))
  ((seen nil)))

(defreply init-sheep :after ((bot (proto 'seen-bot)) &key)
	  (setf (seen bot) nil))

(defreply init-bot :after ((bot (proto 'seen-bot)))
	  (load-seen bot))

(defreply join :after ((bot (proto 'seen-bot)) channel)
	  (unless (alref channel (seen bot))
	    (setf (alref channel (seen bot)) (make-hash-table :test #'equalp))))

;;; utility

(defun seen-db (bot)
  (merge-pathnames "seen-table.db" (bot-dir bot)))

;;; loading, saving

(defmessage load-seen (bot))
(defmessage save-seen (bot))

(defreply load-seen ((bot (proto 'seen-bot)))
  (print "loading")
  (when (probe-file (seen-db bot))
    (setf (seen bot)
	  (cl-store:restore (seen-db bot)))))

(defreply save-seen ((bot (proto 'seen-bot)))
  (cl-store:store (seen bot) (seen-db bot)))




(defmessage have-seen (bot channel nick))
(defreply have-seen ((bot (proto 'seen-bot)) channel nick)
  (when (alref channel (seen bot))
    (setf (gethash nick (alref channel (seen bot))) (get-universal-time))))
(defreply have-seen :after ((bot (proto 'seen-bot)) channel nick)
	  (declare (ignore channel nick))
	  (print "saving")
	  (save-seen bot))

(defmessage last-seen (bot channel nick))
(defreply last-seen ((bot (proto 'seen-bot)) channel nick)
  (let ((seen-table (alref channel (seen bot))))
    (when seen-table
      (gethash nick seen-table))))
  

(deflistener seen-listener
  (print "OMG")
  (have-seen *bot* *channel* *sender*))

(defcommand seen ("(.+)" nick)
  "seen <nick> - reports the last time nick was seen in this channel"
  (let ((seen-time (last-seen *bot* *channel* nick)))
    (if seen-time
	(build-string "I last saw ~A on ~A" nick
		      (build-string (get-datestamp seen-time)
				    " at "
				    (funcall *default-timestamp-function* 
					     seen-time)))
	(build-string "I haven't seen ~A" nick))))

(defun forget (bot channel nick)
  (let ((channel-table (alref channel (seen bot))))
    (when channel-table
      (setf (gethash nick channel-table) nil))))

(defcommand forget ("(.+)" nick)
  "forget <nick> - make the bot forget having seen nick"
  (forget *bot* *channel* nick)
  (build-string "~A? Who? I have no idea who that is ;)" nick))