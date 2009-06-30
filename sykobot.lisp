(defpackage #:sykobot
  (:use :cl :cl-ppcre))
(in-package :sykobot)

(defparameter *conn* nil)
(defparameter *msg-loop-thread* nil)
(defparameter *nick* "sykobot")

(defun join-channel (name)
  (irc:join *conn* name))

(defun connect-to-server (server-name &rest channels)
  (setf *conn* (irc:connect :nickname *nick* :server server-name))
  (mapc #'join-channel channels)
  (irc:add-hook *conn* 'irc:irc-privmsg-message #'msg-hook)
  (setf *msg-loop-thread* (bt:make-thread
                           (lambda ()
                             (irc:read-message-loop *conn*)))))

(defun disconnect (&optional message)
  (bt:destroy-thread *msg-loop-thread*)
  (irc:quit *conn* (or message (values))))

(defun msg-hook (msg)
  (let ((sender (irc:source msg))
        (channel (car (irc:arguments msg)))
        (message (second (irc:arguments msg))))
    (process-message sender channel message)))

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

(defun respond-to-message (sender channel message)
  (let* ((string (scan-string-for-direct-message channel message))
         (command+args (split "\\s+" string :limit 2)))
    (answer-command (car command+args) (cadr command+args) sender channel)))

(defun answer-command (cmd args sender channel)
  (cond ((string-equal cmd "echo")
         (send-msg channel args))
        ((string-equal cmd "ping")
         (send-msg channel "pong"))
        ((string-equal cmd "google")
         (google-search args sender channel))
        ((string-equal cmd "help")
         (send-msg channel (format nil "~A: I'm not a psychiatrist. Go away." sender)))
        (t (send-notice sender (format nil "Unknown command: ~A" cmd)))))

(defun google-search (query sender channel)
  (let ((search-string (regex-replace-all "\\s+" query "+")))
    (multiple-value-bind (title url)
        (url-info (format nil "http://google.com/search?btnI&q=~A" search-string))
      (send-msg channel (format nil "~A: ~A <~A>" sender title url)))))

(defun url-info (url)
  (handler-case
      (multiple-value-bind (body status-code headers uri)
          (drakma:http-request url)
        (declare (ignore status-code headers))
        (values (multiple-value-bind (match vec)
                    (scan-to-strings (create-scanner "<title>(.+)</title>" 
                                                     :case-insensitive-mode t) body)
                  (declare (ignore match))
                  (if (< 0 (length vec))
                      (elt vec 0)
                      nil))
                (with-output-to-string (s)
                  (puri:render-uri uri s))))
    (usocket:ns-host-not-found-error () (error "Host not found"))))

(defun send-notice (target message)
  (irc:notice *conn* target message))
(defun send-msg (channel message)
  (irc:privmsg *conn* channel (or message "")))

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

(defun has-url-p (string)
  (when (scan "https?://.*[.$| |>]" string) t))

(defun grab-url (string)
  (find-if #'has-url-p (split "\\s+|>|<|," string)))

(defun topic (channel-name)
  (let ((channel (irc:find-channel *conn* channel-name)))
    (irc:topic channel)))
