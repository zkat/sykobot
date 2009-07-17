(in-package :sykobot)

(defproto interpreter-bot ((proto 'command-bot))
  ((people-to-interpret nil)))

(defreply init-sheep :after ((bot (proto 'interpreter-bot)) &key)
  (setf (people-to-interpret bot) nil))

(defcommand interpret ("(\\S+) (in|into|to)\\s*([a-z]{2})*" nick foo output-lang)
  (declare (ignore foo))
  "Syntax: interpret <output-lang> <nick> - Turns on automatic translation of everything <nick> says into <output-lang>. <output-lang> should be a two letter language specifier"
  (let ((ol (if output-lang output-lang "en")))
    (setf (alref nick (alref *channel* (people-to-interpret *bot*))) ol)
    "Tada!"))

(defcommand nointerpret ("(.+)" nick)
  "Syntax: nointerpret <nick> - Stop automatically translating whatever <nick> says."
  (setf (alref nick (alref *channel* (people-to-interpret *bot*))) nil)
  "Tada!")

(deflistener interpreter-listener
  (let ((output-lang (alref *sender* (alref *channel* (people-to-interpret *bot*)))))
    (when output-lang
      (send-msg *bot* *channel* 
		(build-string "<~A> ~A" *sender*
			      (translate "*" output-lang *message*))))))

