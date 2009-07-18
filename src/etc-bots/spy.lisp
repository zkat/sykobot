
(in-package :sykobot)

(defproto spy-bot ((proto 'command-bot))
  ((channels-to-spy nil)))

(defreply init-sheep :after ((bot (proto 'spy-bot)) &key)
	  (setf (channels-to-spy bot) nil))

(defcommand spy ("on (\\S+)" channel)
  (push *channel* (alref channel (channels-to-spy *bot*)))
  "I'll be sneaky")

(defcommand nospy ("(\\S+)" channel)
  (setf (alref channel (channels-to-spy *bot*)) (remove *channel* (channels-to-spy *bot*)))
  "I'll withdraw right away")

(deflistener spy-listener
  (let ((report-to (alref *channel* (channels-to-spy *bot*))))
    (when report-to
      (loop for channel in report-to
	 do (send-msg *bot* channel (build-string "~A <~A> ~A"
						  *channel*
						  *sender*
						  *message*))))))