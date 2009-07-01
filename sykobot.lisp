(defpackage #:sykobot
  (:use :cl :cl-ppcre))
(in-package :sykobot)

(defparameter *conn* nil)
(defparameter *msg-loop-thread* nil)
(defparameter *nick* "sykobot")

;;;
;;; IRC connection
;;;
(defun connect (server-name &rest channels)
  (setf *conn* (irc:connect :nickname *nick* :server server-name))
  (mapc #'join channels)
  (irc:add-hook *conn* 'irc:irc-privmsg-message #'msg-hook)
  (setf *msg-loop-thread* (bt:make-thread
                           (lambda ()
                             (irc:read-message-loop *conn*)))))

(defun disconnect (&optional message)
  (bt:destroy-thread *msg-loop-thread*)
  (irc:quit *conn* (or message (values))))

(defun join (channel-name)
  (irc:join *conn* channel-name))

(defun part (channel-name)
  (irc:part *conn* channel-name))

(defun identify (password)
  (send-msg "nickserv" (format nil "identify ~A" password)))

;;;
;;; irc functions
;;;
(defun change-nickname (new-name)
  (setf *nick* new-name)
  (irc:nick *conn* new-name))

(defun send-notice (target message)
  (irc:notice *conn* target message))

(defun send-msg (channel message)
  (irc:privmsg *conn* channel (or message "")))

(defun topic (channel-name)
  (let ((channel (irc:find-channel *conn* channel-name)))
    (irc:topic channel)))

;;;
;;; Message processing
;;;
(defun msg-hook (msg)
  (let ((sender (irc:source msg))
        (channel (car (irc:arguments msg)))
        (message (second (irc:arguments msg))))
    (process-message sender channel message)))

(defun process-message (sender channel message)
  (when (sent-to-me-p channel message)
    (respond-to-message sender channel message))
  (when (and (has-url-p message)
             (not (string-equal sender *nick*)))
    (handler-case
        (multiple-value-bind (title url)
            (url-info (grab-url message))
          (send-msg channel (format nil "Title: ~A (at ~A)" title (puri:uri-host (puri:uri url)))))
      (error ()
        (values)))))

(defun respond-to-message (sender channel message)
  (let* ((string (scan-string-for-direct-message channel message))
         (command+args (split "\\s+" string :limit 2)))
    (handler-case
        (answer-command (car command+args) (cadr command+args) sender channel)
      (error (e)
        (send-msg channel (format nil "~A: An error occurred: ~A" sender e))))))

(defun answer-command (cmd args sender channel)
  (let ((fn (command-function cmd)))
    (funcall fn args sender channel)))

(defun sent-to-me-p (channel message)
  (when (scan-string-for-direct-message channel message)
    t))

(defun scan-string-for-direct-message (channel message)
  (cond ((equal channel *nick*)
         message)
        ((scan (format nil "^~A: " *nick*) message)
         (regex-replace (format nil "^~A: " *nick*) message ""))
        ((scan (format nil "^~A, " *nick*) message)
         (regex-replace (format nil "^~A, " *nick*) message ""))
        ((scan "^!+" message)
         (regex-replace "^!+" message ""))))

