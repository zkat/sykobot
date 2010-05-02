;;;; Copyright 2009 Kat Marchan
;;;;
;;;; This file is part of sykobot.
;;;;
;;;; For licensing and warranty information, refer to COPYING
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package :sykobot)

(defproto =sykobot= ()
  ((connection nil)
   (msg-loop-thread nil)
   (nickname "sykobot")
   (username "sykobot")
   (realname "sykobot")
   (server "irc.freenode.net")
   (port 6667)
   (dir "default-bot/")
   (password nil)
   (channels nil)))

(defvar *active-bot* nil)

;;; Stdout logging of raw IRC.
(defmethod cl-irc:irc-message-event :before (connection message)
  (declare (ignore connection))
  (format t "-> ~a~%"
          (let ((message (irc:raw-message-string message)))
            (subseq message 0 (1- (length message))))))

(defmethod cl-irc::send-irc-message :before (connection command &rest arguments)
  (declare (ignore connection))
  ;; make-irc-message includes a newline
  (format t "<- ~a"  (apply #'cl-irc::make-irc-message command arguments))
  (finish-output))

;;;
;;; IRC connection
;;;
(defmessage init-bot (bot))
(defmessage connect (bot))
(defmessage disconnect (bot &optional message))
(defmessage join (bot channel))
(defmessage part (bot channel))
(defmessage identify (bot password))

(defreply init-bot ((bot =sykobot=))
  (when *active-bot*
    (error "There is already a bot running. Disconnect the current *active-bot* and try again."))
  (connect bot)
  (setf *active-bot* bot))

(defreply connect ((bot =sykobot=))
  (setf (connection bot) (irc:connect :nickname (nickname bot)
                                      :server (server bot)
                                      :port (port bot)
                                      :password (password bot)
                                      :username (username bot)
                                      :realname (realname bot)))
  (setf (irc:client-stream (connection bot)) (make-broadcast-stream))
  (irc:add-hook (connection bot) 'irc:irc-privmsg-message
                (lambda (msg)
                  (handler-bind ((cl-irc:no-such-reply (lambda (c)
                                                         (let ((r (find-restart 'continue c)))
                                                           (when r (invoke-restart r))))))
                    (msg-hook bot msg))))
  (setf (msg-loop-thread bot)
        (bt:make-thread
         (lambda ()
           (handler-bind ((cl-irc:no-such-reply
                           (lambda (c)
                             (let ((r (find-restart 'continue c)))
                               (when r (invoke-restart r))))))
             (irc:read-message-loop (connection bot)))))))

(defreply disconnect ((bot =sykobot=) &optional message)
  (bt:destroy-thread (msg-loop-thread bot))
  (setf *active-bot* nil)
  (irc:quit (connection bot) (or message (values))))

(defreply join ((bot =sykobot=) channel)
  (irc:join (connection bot) channel)
  (str-pushnew channel (channels bot)))

(defreply part ((bot =sykobot=) channel)
  (irc:part (connection bot) channel)
  (with-accessors ((channels channels)) bot
    (setf channels (str-delete channel channels))))

(defreply identify ((bot =sykobot=) password)
  (send-msg bot "nickserv" (build-string "identify ~A" password)))

;;;
;;; irc functions
;;;
(defmessage nick (bot new-nick))
(defmessage send-msg (bot target message))
(defmessage send-reply (bot target user message))
(defmessage send-action (bot channel action))
(defmessage topic (bot channel &optional new-topic))
(defmessage whois (bot mask))

(defreply nick ((bot =sykobot=) new-nick)
  (setf (nickname bot) new-nick)
  (irc:nick (connection bot) new-nick))

(defreply send-msg ((bot =sykobot=)
                    (target =string=)
                    (message =string=))
  (with-properties (connection) bot
    (do-lines (line message collected-lines)
      do (irc:privmsg connection
		      target
		      ;; The following prevents the injection of arbitrary raw IRC via messages containing \r and other possibly meaningful non-printable characters in cases where raw message content originates from a third party source, e.g. raw URL title echoing.
		      (remove-if
		       (lambda (c) (< (char-code c) 32))
		       line))
      collect line into collected-lines)))

(defreply send-reply ((bot =sykobot=)
                      (target =string=)
                      (user =string=)
                      (message =string=))
  (send-msg bot target
            (if (string-equal target user) message
                (apply #'merge-strings #\Newline
                       (do-lines (line (build-string message) message)
                         collect (build-string "~A: ~A"
					       user line)
			 into message)))))

(defreply send-action ((bot =sykobot=) channel action)
  (irc:privmsg (connection bot) channel
               (build-string "~AACTION ~A~2:*" #\^A action)))

(defreply topic ((bot =sykobot=) channel &optional new-topic)
  (if new-topic
      (irc:topic- (connection bot) channel new-topic)
      (irc:topic (irc:find-channel (connection bot) channel))))

(defreply whois ((bot =sykobot=) mask)
  (irc:whois (connection bot) mask))

;;; Message processing doesn't happen in =sykobot=!!!
(defmessage msg-hook (bot msg))
(defreply msg-hook ((bot =sykobot=) msg)
  (declare (ignore bot msg))
  (format t "I don't know how to handle messages! You might want ~
             to look into using =sykobot-listeners=, who ~
             is able to respond to messages."))