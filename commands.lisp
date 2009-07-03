(in-package :sykobot)

;;;
;;; Command definition
;;;
(let ((command-table (make-hash-table :test #'equalp)))
  (defun add-command (cmd fn)
    (setf (gethash cmd command-table) fn))

  (defun remove-command (cmd)
    (remhash cmd command-table))

  (defun command-function (cmd)
    (multiple-value-bind (fn hasp)
        (gethash cmd command-table)
      (if hasp fn
          (lambda (bot args sender channel)
            (declare (ignore channel args))
            (send-notice bot sender (format nil "I don't know how to ~A." cmd))))))

  (defun erase-all-commands ()
    (clrhash command-table)))

(defmacro defcommand (name &body body)
  `(add-command ,name
                (lambda (*bot* *args* *sender* *channel*)
                  (declare (special *bot* *args* *sender* *channel*))
                  (declare (ignorable *args* *bot* *sender* *channel*))
                  ,@body)))

;;; base commands
(defcommand "echo"
  (send-msg *bot* *channel* *args*))
(defcommand "ping"
  (send-reply *bot* *sender* *channel* "pong"))
(defcommand "shut"
  (when (scan "up" *args*)
    (send-reply *bot* *sender* *channel*
                (format nil "Fine. Be that way. Tell me to talk when you realize ~
                           just how lonely and pathetic you really are."))
    (shut-up *bot*)))
(defcommand "hi"
  (send-reply *bot* *sender* *channel* "Go away."))
(defcommand "give"
  (register-groups-bind (new-target new-command new-args)
      ("(\\S+) (\\S+) (.*)$" *args*
    (answer-command *bot* new-command new-args new-target *channel*))))

;;; Slightly buggy
(defcommand "code->char"
  (let ((char (code-char (read-from-string (car (split "\\s+" *args*))))))
    (send-msg *bot* *channel* (format nil "~A" char))))
(defcommand "char->code"
  (let ((code (char-code (elt *args* 0))))
    (send-msg *bot* *channel* (format nil "~A" code))))

;;; General web functionality

(defun url-info (url)
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
              (puri:render-uri uri s)))))

(defun decode-html-string (string)
  (html-entities:decode-entities string))

(defun search-url (engine query)
  (format nil engine (regex-replace-all "\\s+" query "+")))

;;; CLiki search

(defcommand "cliki"
  (send-reply *bot* *sender* *channel*
              (multiple-value-bind (links numlinks)
                  (cliki-urls *args*)
                (format nil "I found ~D result~:P.~@[ Check out <~A>.~]"
                        numlinks (car links)))))

(defun cliki-urls (query)
  (let ((links NIL)
        (page (drakma:http-request
               (search-url "http://www.cliki.net/admin/search?words=~A"
                           query))))
    (ppcre:do-register-groups (url)
        ("\\d <b><a href=\"(.*?)\">(.*?)<" page)
      (push url links))
    (values (nreverse links)
            (or (parse-integer
                 (or (ppcre:scan-to-strings "(\\d*) results? found" page)
                     "")
                 :junk-allowed T)
                0))))

;;; Google search

(defcommand "google"
  (multiple-value-bind (title url)
      (google-search *args*)
    (send-reply *bot* *sender* *channel*
                (format nil "~A <~A>" title url))))

(defun google-search (query)
  (url-info (search-url
             "http://google.com/search?filter=1&safe=on&q=~A&btnI"
             query)))

;;; kiloseconds
(defcommand "kiloseconds"
  (send-reply *bot* *sender* *channel*
              (format nil "the time is GMT ~3$ ks." (get-ks-time))))

(defun get-ks-time ()
  (multiple-value-bind
        (seconds minutes hours date month year day light zone)
      (get-decoded-time)
    (declare (ignore date month year day light))
    (/ (+ seconds
          (* 60 (+ minutes
                   (* 60 (mod (+ hours zone) 24)))))
       1000)))

;;; memos
(defcommand "memo"
  (destructuring-bind (recipient memo)
      (split "\\s+" *args* :limit 2)
    (progn
      (add-memo recipient memo *sender*)
      (send-msg *bot* *channel*
                (format nil "Tada! Added memo for ~A. ~
                             I'll let them know next time they speak"
                        recipient)))))

(let ((memo-table (make-hash-table :test #'equalp)))
  (defun add-memo (recipient memo sender)
    (setf (gethash recipient memo-table) (list memo sender)))

  (defun remove-memo (recipient)
    (remhash recipient memo-table))

  (defun get-memo (recipient)
    (multiple-value-bind (memo hasp)
        (gethash recipient memo-table)
      (if hasp
          memo
          nil)))

  (defun get-and-remove-memo (recipient)
    (let ((memo (get-memo recipient)))
      (remove-memo recipient)
      memo))

  (defun erase-all-memos ()
    (clrhash memo-table)))

(defun send-memos-for-recipient (bot channel recipient)
  (let ((memo (get-and-remove-memo recipient)))
    (when memo
      (destructuring-bind (text sender) memo
        (send-reply bot recipient channel (format nil "\"~A\" - ~A" text sender))))))
