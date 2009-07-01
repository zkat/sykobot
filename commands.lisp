(in-package :sykobot)

(let ((command-table (make-hash-table :test #'equalp)))
  (defun add-command (cmd fn)
    (setf (gethash cmd command-table) fn))

  (defun remove-command (cmd)
    (remhash cmd command-table))

  (defun command-function (cmd)
    (multiple-value-bind (fn hasp)
        (gethash cmd command-table)
      (if hasp fn 
          (lambda (args sender channel)
            (send-notice sender (format nil "I don't know how to ~A." cmd))))))
  
  (defun erase-all-commands ()
    (clrhash command-table))
  )

(add-command "echo" (lambda (args sender channel)
                     (send-msg channel args)))
(add-command "ping" (lambda (args sender channel)
                     (send-msg channel "pong")))
(add-command "google" (lambda (args sender channel)
                       (google-search args sender channel)))
(add-command "shut" (lambda (args sender channel) 
                     (shut-up)))
(add-command "chant" (lambda (args sender channel) 
                      (send-msg channel "FUCK REGEX")))
(add-command "help" (lambda (args sender channel)
                     (send-msg channel (format nil "~A: I'm not a psychiatrist. Go away." sender))))

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
                    (scan-to-strings
                     (create-scanner 
                      "<title\\s*>\\s*(.+)\\s*</title\\s*>"
                      :case-insensitive-mode t) body)
                  (declare (ignore match))
                  (if (< 0 (length vec))
                      (decode-html-string (elt vec 0))
                      nil))
                (with-output-to-string (s)
                  (puri:render-uri uri s))))
    (usocket:ns-host-not-found-error () (error "Host not found"))))

(defun decode-html-string (string)
  (html-entities:decode-entities string))

(defun has-url-p (string)
  (when (scan "https?://.*[.$| |>]" string) t))

(defun grab-url (string)
  (find-if #'has-url-p (split "[\\s+><,]" string)))
