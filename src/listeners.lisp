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

;;; Attempt at modularization
(defproto sykobot-listener ((proto 'sykobot))
  ((listeners (make-hash-table :test #'eq))
   (active-listeners nil)))

(defmessage add-listener (bot name function))
(defmessage remove-listener (bot name))
(defmessage listener-function (bot name))
(defmessage call-listener (bot name sender channel message))
(defmessage call-listeners (bot sender channel message))
(defmessage listener-active-p (bot name))

(defreply add-listener ((bot (proto 'sykobot-listener)) (name (proto 'symbol)) function)
  (setf (gethash name (listeners bot)) function))

(defreply remove-listener ((bot (proto 'sykobot-listener)) (name (proto 'symbol)))
  (remhash name (listeners bot)))

(defreply listener-function ((bot (proto 'sykobot-listener)) (name (proto 'symbol)))
  (gethash name (listeners bot)
           (lambda (bot sender channel message)
             (declare (ignore bot sender channel message))
             (cerror "Continue" "Nonexistant listener ~S" name))))

(defreply call-listener ((bot (proto 'sykobot-listener)) (name (proto 'symbol)) sender channel message)
  (funcall (listener-function bot name) bot sender channel message))

(defreply call-listeners ((bot (proto 'sykobot-listener)) sender channel message)
  (dolist (name (active-listeners bot))
    (call-listener bot name sender channel message)))

;;; Where is this used? -Adlai
(defreply listener-active-p ((bot (proto 'sykobot-listener)) name)
  (member name (active-listeners bot)))

(defmacro deflistener (name &body body)
  `(add-listener (proto 'sykobot-listener) ',name
                (lambda (*bot* *sender* *channel* *message*)
                  (declare (ignorable *bot* *sender* *channel* *message*))
                  ,@body)))

;;; Customization of listeners
(defmessage listener-on (bot name))
(defmessage listener-off (bot name))

(defreply listener-on ((bot (proto 'sykobot-listener)) name)
  (pushnew name (active-listeners bot)))

(defreply listener-off ((bot (proto 'sykobot-listener)) name)
  (with-properties (active-listeners) bot
    (setf active-listeners (delete name active-listeners))))

(defun activate-listeners (bot &rest names)
  (dolist (name names)
    (listener-on bot name)))
