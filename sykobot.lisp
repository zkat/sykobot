(defpackage #:sykobot
  (:use :cl :cl-ppcre))
(in-package :sykobot)

(defparameter *conn* nil)
(defparameter *msg-loop-thread* nil)
(defparameter *nick* "sykobot")
(defvar *commands* (make-hash-table :test #'eq))

(defun shut-up ()
  (irc:remove-hook *conn* 'irc:irc-privmsg-message #'msg-hook)
  (irc:add-hook *conn* 'irc:irc-privmsg-message #'silent-mode))

(defun un-shut-up ()
  (irc:remove-hook *conn* 'irc:irc-privmsg-message #'silent-mode)
  (irc:add-hook *conn* 'irc:irc-privmsg-message #'msg-hook))

(defun silent-mode (msg)
  (let* ((sender (irc:source msg))
         (channel (car (irc:arguments msg)))
         (message (second (irc:arguments msg)))
         (string (scan-string-for-direct-message channel message))
         (command (car (split "\\s+" string :limit 2))))
    (when (let ((x (string-not-equal command "talk")))
            (or (not x)
                (= x 4)))
      (send-msg channel
                (format nil
                        "~A: bla bla bla bla. There, happy?"
                        sender))
      (un-shut-up))))

(defun join-channel (name)
  (irc:join *conn* name))

(defun change-nickname (new-name)
  (setf *nick* new-name)
  (irc:nick *conn* new-name))

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
    (handler-case
        (answer-command (car command+args) (cadr command+args) sender channel)
      (error (e)
        (send-msg channel (format nil "~A: An error occurred: ~A" sender e))))))

(defun add-command (cmd-symbol fn)
  (setf (gethash cmd-symbol *commands*) fn))
(defun remove-command (cmd-symbol)
  (remhash cmd-symbol *commands*))
(defun command-function (cmd-symbol)
  (multiple-value-bind (fn hasp)
      (gethash cmd-symbol *commands*)
    (or fn (lambda (args sender channel) 
             (send-notice sender (format nil "I don't know how to ~A." cmd))))))
(defun answer-command (cmd args sender channel)
  (funcall (command-function (read-from-string cmd)) args sender channel))

(add-command 'echo (lambda (args sender channel)
                     (send-msg channel args)))
(add-command 'ping (lambda (args sender channel)
                     (send-msg channel "pong")))
(add-command 'google (lambda (args sender channel)
                       (google-search args sender channel)))
(add-command 'shut (lambda (args sender channel) 
                     (shut-up)))
(add-command 'chant (lambda (args sender channel) 
                      (send-msg channel "FUCK REGEX")))
(add-command 'help (lambda (args sender channel)
                     (send-msg channel (format nil "~A: I'm not a psychiatrist. Go away." sender))))

(defun google-search (query sender channel)
  (let ((search-string (regex-replace-all "\\s+" query "+")))
    (multiple-value-bind (title url)
        (url-info (format nil "http://google.com/search?filter=1&safe=on&q=~A&btnI" search-string))
      (send-msg channel (format nil "~A: ~A <~A>" sender title url)))))

(defun url-info (url)
  (handler-case
      (multiple-value-bind (body status-code headers uri)
          (drakma:http-request url)
        (declare (ignore status-code headers))
        (values (multiple-value-bind (match vec)
                    (scan-to-strings (create-scanner "<title[ \\t\\r\\n]*>(.+)</title[ \\t\\r\\n]*>"
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
  (find-if #'has-url-p (split "[\\s+><,]" string)))

(defun topic (channel-name)
  (let ((channel (irc:find-channel *conn* channel-name)))
    (irc:topic channel)))
