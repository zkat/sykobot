
(in-package :sykobot)

(defproto spy-bot ((proto 'command-bot))
  ((channels-to-spy nil)))

(defreply init-sheep :after ((bot (proto 'spy-bot)) &key)
	  (setf (channels-to-spy bot) nil))

(defcommand spy ("on (\\S+)" channel)
  "Syntax: 'spy on <channel>' - Echoes all messages in <channel> to the current ~
   channel. The bot can only spy on channels which it has already joined."
  (with-accessors ((channels channels-to-spy)) *bot*
    (if (or (find *channel* (alref channel channels) :test #'equalp)
            (string-equal channel *channel*)
            (not (find channel (channels *bot*) :test #'equalp)))
        "Have you nothing better to do than torture a bot?"
        (progn
          (push *channel* (alref channel channels))
          "I'll be sneaky"))))

(defcommand nospy ("(\\S+)" channel)
  "Syntax: 'nospy <channel>' - Stops echoing messages from <channel> to the ~
   current channel."
  (with-accessors ((channels channels-to-spy)) *bot*
    (setf (alref channel channels)
          (remove *channel* (alref channel channels) :test #'equalp)))
  "I'll withdraw right away")

(deflistener spy-listener
  (let ((report-to (alref *channel* (channels-to-spy *bot*))))
    (when report-to
      (loop for channel in report-to
	 do (send-msg *bot* channel
                      (build-string "~A <~A> ~A"
                                    *channel* *sender* *message*))))))
