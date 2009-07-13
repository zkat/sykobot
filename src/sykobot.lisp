;;;; Copyright 2009 Kat Marchan
;;;;
;;;; This file is part of sykobot.
;;;;
;;;; For licensing and warranty information, refer to COPYING
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package :sykobot)

(defproto sykobot ()
  ((connection nil)
   (msg-loop-thread nil)
   (nickname "sykobot")
   (username "sykobot")
   (realname "sykobot")
   (server "irc.freenode.net")
   (port 6667)
   (dir "default-bot/")
   (password nil)
   (aliases nil)
   (silentp nil)
   (memos (make-hash-table :test #'equalp))
   (facts (make-hash-table :test #'equalp))
   (quotes (make-hash-table :test #'equalp))
   (last-said (make-hash-table :test #'equalp))
   (active-listeners nil)))

(defreply init-sheep :after ((sheep (proto 'sykobot)) &key)
  (setf (memos sheep) (make-hash-table :test #'equalp))
  (setf (facts sheep) (make-hash-table :test #'equalp))
  (setf (quotes sheep) (make-hash-table :test #'equalp))
  (setf (last-said sheep) (make-hash-table :test #'equalp)))

(defvar *active-bot* nil)

;;; Good Medicine
(setf drakma:*drakma-default-external-format* :utf-8
      flexi-streams:*substitution-char* #\?)

;;; Stdout logging of raw IRC.
(defmethod irc-message-event :before (connection message)
  (declare (ignore connection))
  (format t "-> ~a~%" (irc:raw-message-string message)))

(defmethod cl-irc::send-irc-message :after (connection command &rest arguments)
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

(defreply init-bot ((bot (proto 'sykobot)))
  (when *active-bot*
    (error "There is already a bot running. Disconnect the current *active-bot* and try again."))
  (connect bot)
  (setf *active-bot* bot))

(defreply connect ((bot (proto 'sykobot)))
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

(defreply disconnect ((bot (proto 'sykobot)) &optional message)
  (bt:destroy-thread (msg-loop-thread bot))
  (setf *active-bot* nil)
  (irc:quit (connection bot) (or message (values))))

(defreply join ((bot (proto 'sykobot)) channel)
  (irc:join (connection bot) channel))
(defreply join :after ((bot (proto 'sykobot)) channel)
  (setf (gethash channel (last-said bot))
        (make-hash-table :test #'equalp)))

(defreply part ((bot (proto 'sykobot)) channel)
  (irc:part (connection bot) channel))

(defreply identify ((bot (proto 'sykobot)) password)
  (send-msg bot "nickserv" (format nil "identify ~A" password)))

;;;
;;; irc functions
;;;
(defmessage nick (bot new-nick))
(defmessage send-notice (bot target message))
(defmessage send-msg (bot channel message))
(defmessage send-reply (bot target channel message))
(defmessage send-action (bot channel action))
(defmessage topic (bot channel &optional new-topic))

(defreply nick ((bot (proto 'sykobot)) new-nick)
  (setf (nickname bot) new-nick)
  (irc:nick (connection bot) new-nick))

(defreply send-notice ((bot (proto 'sykobot)) target message)
  (irc:notice (connection bot) target message))

(defreply send-msg ((bot (proto 'sykobot)) channel message)
  (unless (silentp bot)
   (irc:privmsg (connection bot) channel (or message ""))))

(defreply send-reply ((bot (proto 'sykobot)) target channel message)
  (send-msg bot channel (format nil "~A: ~A" target message)))

(defreply send-action ((bot (proto 'sykobot)) channel action)
  (send-msg bot channel (format nil "~AACTION ~A~A"
                                (code-char 1) action (code-char 1))))

(defreply topic ((bot (proto 'sykobot)) channel &optional new-topic)
  (if new-topic
      (irc:topic- (connection bot) channel new-topic)
      (irc:topic (irc:find-channel (connection bot) channel))))

;;;
;;; Message processing
;;;
(defmessage msg-hook (bot msg))
(defmessage process-message (bot sender channel message))
(defmessage shut-up (bot))
(defmessage un-shut-up (bot))

(defreply msg-hook ((bot (proto 'sykobot)) msg)
  (let ((sender (irc:source msg))
        (channel (let ((target (car (irc:arguments msg))))
		   (if (equal target
			      (irc:nickname (irc:user (irc:connection msg))))
		       (irc:source msg)
		       target)))
        (message (second (irc:arguments msg))))
    (process-message bot sender channel message)))

(defreply process-message ((bot (proto 'sykobot))
                           sender channel message)
  (call-all-listeners bot sender channel
                      (expand-aliases bot message)))

(defreply shut-up ((bot (proto 'sykobot)))
  (setf (silentp bot) t))

(defreply un-shut-up ((bot (proto 'sykobot)))
  (setf (silentp bot) nil))

(defreply respond-to-message ((bot (proto 'sykobot)) sender channel message)
  (let* ((string (scan-string-for-direct-message bot channel message))
         (command+args (split "\\s+" string :limit 2)))
    (handler-case
        (answer-command bot (car command+args) (cadr command+args) sender channel)
      (error (e)
        (send-reply bot sender channel (format nil "An error occurred: ~A" e))))))

(defreply answer-command ((bot (proto 'sykobot)) (cmd (proto 'string)) args sender channel)
  (answer-command bot (intern (string-upcase cmd) :sykobot) args sender channel))

(defreply answer-command ((bot (proto 'sykobot)) (cmd (proto 'symbol)) args sender channel)
  (let ((fn (command-function cmd)))
    (funcall fn bot args sender channel)))

(defun sent-to-me-p (bot channel message)
  (when (scan-string-for-direct-message bot channel message)
    t))

(defparameter *cmd-prefix* "@")
(defmessage scan-string-for-direct-message (bot channel message))
(defreply scan-string-for-direct-message ((bot (proto 'sykobot)) channel message)
  (cond ((equal channel (nickname bot))
         message)
        ((scan (format nil "^~A: " (nickname bot)) message)
         (regex-replace (format nil "^~A: " (nickname bot)) message ""))
        ((scan (format nil "^~A, " (nickname bot)) message)
         (regex-replace (format nil "^~A, " (nickname bot)) message ""))
        ((scan (format nil "^~A+" *cmd-prefix*) message)
         (regex-replace (format nil "^~A+" *cmd-prefix*) message ""))))

;;;
;;; Aliases
;;;
(defmessage add-alias (bot alias expansion))
(defmessage remove-alias (bot alias))
(defmessage expand-aliases (bot message))

(defreply add-alias ((bot (proto 'sykobot)) alias expansion)
  (setf (aliases bot) (acons alias expansion (aliases bot))))

(defreply remove-alias ((bot (proto 'sykobot)) alias)
  (setf (aliases bot) (delete alias (aliases bot) :count 1
                              :test #'string-equal :key #'car)))

(defreply expand-aliases ((bot (proto 'sykobot)) message)
  (loop while
       (loop for (alias . expansion) in (aliases bot)
          for (new changedp) =
            (multiple-value-list (regex-replace-all alias message
                                                    expansion
                                                    :preserve-case t))
          when changedp do
            (setf message new)
            (return t))
       finally (return message)))
