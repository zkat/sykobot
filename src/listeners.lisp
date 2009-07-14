;;;; Copyright 2009 Kat Marchan
;;;;
;;;; This file is part of sykobot.
;;;;
;;;; For licensing and warranty information, refer to COPYING
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package :sykobot)

;;; These are only bound within the body of listeners.
(defvar *bot*)
(defvar *message*)
(defvar *sender*)
(defvar *channel*)

;;; Must be bound before the bot runs
(defvar *default-listeners*)

;;; Attempt at modularization
(defproto listener-bot ((proto 'sykobot))
  ((listeners (make-hash-table :test #'eq))
   (active-listeners nil)
   (deafp nil)))

(defreply msg-hook ((bot (proto 'listener-bot)) msg)
  (let ((sender (irc:source msg))
        (channel (let ((target (car (irc:arguments msg))))
		   (if (equal target (nickname bot))
		       (irc:source msg)
		       target)))
        (message (cadr (irc:arguments msg))))
    (call-active-listeners bot channel sender
                           (escape-format-string message))))

(defmessage add-listener (bot name function))
(defmessage remove-listener (bot name))
(defmessage listener-function (bot name))
(defmessage call-listener (bot name sender channel message))

(defreply set-listener ((bot (proto 'listener-bot)) (name (proto 'symbol)) function)
  (setf (gethash name (listeners bot)) function))

(defreply remove-listener ((bot (proto 'listener-bot)) (name (proto 'symbol)))
  (remhash name (listeners bot)))

(defreply listener-function ((bot (proto 'listener-bot)) (name (proto 'symbol)))
  (with-properties (listeners) bot
    (gethash name listeners
             (lambda (bot sender channel message)
               (declare (ignore bot sender channel message))
               (cerror "Continue" "Nonexistant listener ~S" name)))))

(defreply call-listener ((bot (proto 'listener-bot)) (name (proto 'symbol)) sender channel message)
  (funcall (listener-function bot name) bot sender channel message))

(defmacro deflistener (name &body body)
  `(set-listener (proto 'listener-bot) ',name
                 (lambda (*bot* *sender* *channel* *message*)
                   (declare (ignorable *bot* *sender* *channel* *message*))
                   ,@body)))

;;; Customization of listeners
(defmessage listener-on (bot channel name))
(defmessage listener-off (bot channel name))
(defmessage call-active-listeners (bot channel sender message))
(defmessage listener-active-p (bot channel name))

(defreply listener-on ((bot (proto 'listener-bot)) channel name)
  (pushnew name (alref channel (active-listeners bot))))

(defreply listener-off ((bot (proto 'listener-bot)) channel name)
  (with-properties (active-listeners) bot
    (setf (alref channel active-listeners)
          (delete name (alref channel active-listeners)))))

(defreply call-active-listeners ((bot (proto 'listener-bot)) channel sender message)
  (let ((deafp (alref channel (deafp bot))))
    (if deafp
        (call-listener bot deafp sender channel message)
        (dolist (name (alref channel (active-listeners bot)))
          (call-listener bot name sender channel message)))))

(defreply listener-active-p ((bot (proto 'listener-bot)) channel name)
  (member name (alref channel (active-listeners bot))))

(defun activate-listeners (bot channel &rest names)
  (dolist (name names)
    (listener-on bot channel name)))

(defreply join :after ((bot (proto 'listener-bot)) channel)
  (apply #'activate-listeners bot channel *default-listeners*))

;;; Deafness (aka silence)

(defmessage toggle-deafness (bot channel))

(defreply toggle-deafness ((bot (proto 'listener-bot)) channel)
  (setf (alref channel (deafp bot))
        (and (not (deafp bot))
             'undeafen)))

(deflistener undeafen
  (unless (zerop (length *message*))
    (toggle-deafness *bot* *channel*)))