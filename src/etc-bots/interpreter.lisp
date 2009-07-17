(in-package :sykobot)

(defproto interpreter-bot ((proto 'command-bot))
  ((people-to-interpret nil)))

(defreply init-sheep :after ((bot (proto 'interpreter-bot)) &key)
  (setf (people-to-interpret bot) nil))

(defcommand interpret ("(\\S+)\\s*into\\s*(\\S+)" nick output-lang)
  "Syntax: 'interpret <output-lang> into <lang>' - Turns on automatic translation of ~
everything <nick> says into <output-lang>. <output-lang> should be a two letter language ~
specifier. Use 'nointerpret <nick>' to stop it."
  (let ((lang (or output-lang "en")))
    (setf (alref nick (alref *channel* (people-to-interpret *bot*))) lang)
    (build-string "Very well, I'll translate everything ~A says into ~A" nick lang)))

(defcommand nointerpret ("(.+)" nick)
  "Syntax: 'nointerpret <nick>' - Stop automatically translating whatever <nick> says."
  (setf (alref nick (alref *channel* (people-to-interpret *bot*))) nil)
  (build-string "Rgr. No longer interpreting for ~A" nick))

(deflistener interpreter-listener
  (let ((output-lang (alref *sender* (alref *channel* (people-to-interpret *bot*)))))
    (when output-lang
      (send-msg *bot* *channel* 
		(build-string "<~A> ~A" *sender*
			      (translate "*" output-lang *message*))))))

