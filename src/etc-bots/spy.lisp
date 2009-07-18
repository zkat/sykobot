
(in-package :sykobot)

(defproto spy-bot ((proto 'command-bot))
  ((channels-to-spy nil)))

(defreply init-sheep :after ((bot (proto 'spy-bot)) &key)
	  (setf (channels-to-spy bot) nil))

(defcommand spy ("on (\\S+)" channel)
  (with-accessors ((channels channels-to-spy)) *bot*
    (if (find *channel* (alref channel channels) :test #'equalp)
        "Have you nothing better to do than torture a bot?"
        (progn
          (push *channel* (alref channel channels))
          "I'll be sneaky"))))

(defcommand nospy ("(\\S+)" channel)
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
