(in-package :sykobot)

;;; Command definition
(let ((command-table (make-hash-table :test #'eq)))
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

(defmacro defcommand (name vars regex &body body)
  `(add-command ',name
                (lambda (*bot* *message* *sender* *channel*)
                  (declare (special *bot* *message* *sender* *channel*))
                  (declare (ignorable *message* *bot* *sender* *channel*))
                  ,@(if vars
                        `((register-groups-bind ,vars (,regex *message*)
                            ,@body))
                       `(,@body)))))

;;; base commands
(defcommand echo (string) "(.*)"
  (send-reply *bot* *sender* *channel* string))
(defcommand ping () ""
  (send-reply *bot* *sender* *channel* "pong"))
(defcommand shut (arg1) "(\\S+)*"
  (when (equalp arg1 "up")
    (send-reply *bot* *sender* *channel*
                (format nil "Fine. Be that way. Tell me to talk when you realize ~
                           just how lonely and pathetic you really are."))
    (shut-up *bot*)))
(defcommand hi () ""
  (send-reply *bot* *sender* *channel* "Go away."))
(defcommand give (new-target new-command new-args) "(\\S+) (\\S+) (.*)$"
  (answer-command *bot* new-command new-args new-target *channel*))

;;; Character Decoding
(defcommand code->char (code-string) "(\\S+)*"
  (send-msg *bot* *channel*
            (let ((code (if code-string (parse-integer code-string :junk-allowed T) 0)))
              (format nil "~:[Invalid code~;~:*~A~]"
                      (and (integerp code) (/= code 127) (>= code 32)
                           (code-char code))))))

(defcommand char->code (char-string) "(\\S+)*"
  (send-msg *bot* *channel*
            (let ((code (and char-string (char-code (elt char-string 0)))))
              (format nil "~:[Invalid character~;~A~]"
                      (and (integerp code) (/= code 127) (>= code 32))
                      code))))

;;; General web functionality
(defun url-info (url)
  (multiple-value-bind (body status-code headers uri)
      (drakma:http-request url)
    (declare (ignore status-code headers))
    (values (multiple-value-bind (match vec)
                (scan-to-strings
                 (create-scanner
                  "<title[.\\s]*>\\s*(.+)\\s*</title[\\s.]*>"
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

;;; Google
(defcommand google (query) "(.*)"
  (multiple-value-bind (title url)
      (google-search query)
    (send-reply *bot* *sender* *channel*
                (format nil "~:[~;~A ~]<~A>" title url))))

(defun google-search (query)
  (url-info (search-url
             "http://google.com/search?filter=1&safe=on&q=~A&btnI"
             query)))

;;; CLiki search
(defcommand cliki (query) "(.*)"
  (send-reply *bot* *sender* *channel*
              (multiple-value-bind (links numlinks)
                  (cliki-urls query)
                (format nil "I found ~D result~:P.~@[ Check out <~A>.~]"
                        numlinks (car links)))))

(defun cliki-urls (query)
  (let ((links NIL)
        (page (drakma:http-request
               (search-url "http://www.cliki.net/admin/search?words=~A"
                           query))))
    (do-register-groups (url)
        ("\\d <b><a href=\"(.*?)\">(.*?)<" page)
      (push url links))
    (values (nreverse links)
            (or (parse-integer
                 (or (scan-to-strings "(\\d*) results? found" page)
                     "")
                 :junk-allowed T)
                0))))

;;; kiloseconds
(defcommand kiloseconds (zone) "(.*)"
  (let* ((parsed-zone (if (= 0 (length zone))
                          0
                          (parse-integer zone :junk-allowed t)))
         (ks-time (get-ks-time parsed-zone)))
    (send-reply *bot* *sender* *channel*
                (format nil "The time in GMT~A is ~3$ ks." 
                        (if (or (= parsed-zone 0) (plusp parsed-zone))
                            (format nil "+~A" parsed-zone)
                            (format nil "~A" parsed-zone))
                        ks-time))))

(defun get-ks-time (&optional (gmt-diff 0))
  (multiple-value-bind
        (seconds minutes hours date month year day light zone)
      (get-decoded-time)
    (declare (ignore date month year day light))
    (/ (+ seconds
          (* 60 (+ minutes
                   (* 60 (mod (+ hours zone gmt-diff) 24)))))
       1000)))

;;; Memos
(defcommand memo (recipient memo) "for (\\S+): (.*)"
  (add-memo recipient memo *sender*)
  (send-msg *bot* *channel*
            (format nil "Tada! Added memo for ~A. ~
                             I'll let them know next time they speak"
                    recipient)))

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

(deflistener send-memos
  (let* ((recipient *sender*)
         (memo (get-and-remove-memo recipient)))
    (when memo
      (destructuring-bind (text who-from) memo
        (send-reply *bot* recipient *channel* (format nil "~A - ~A" text who-from))))))

;;; Parrot
(deflistener parrot
  (send-msg *bot* *channel* *message*))
(defcommand parrot () ""
  (activate-listener 'parrot))
(defcommand noparrot () ""
  (deactivate-listener 'parrot))

;;; Facts
(let ((fact-table (make-hash-table :test #'equalp)))
  (defun set-fact (noun info)
    (setf (gethash noun fact-table) info))

  (defun get-fact (noun)
    (multiple-value-bind (info hasp)
        (gethash noun fact-table)
      (if hasp
          info
          (format nil "I know nothing about ~A" noun))))

  (defun erase-all-facts ()
    (clrhash fact-table)))

(defun split-into-sub-statements (statement)
  (split "\\s*(,|but|however|whereas|although|\\;|\\.)\\s*" statement))

(deflistener scan-for-fact
  (loop for statement in (split-into-sub-statements *message*)
     do (do-register-groups (article noun verb info)
            (".*?([a|an|the|this|that]*)\\s*(\\w+)\\s+(is|are|isn't|ain't)\\s+(.+)"
             statement)
          (set-fact noun (format nil "~A ~A ~A ~A" article noun verb info)))))

(defcommand fact (topic) "(\\S+)*"
  (send-msg *bot* *channel* (get-fact topic)))

;;; URLs
(deflistener scan-for-url
  (when (and (has-url-p *message*)
             (not (string-equal *sender* (nickname *bot*))))
    (handler-case
        (multiple-value-bind (title url)
            (url-info (grab-url *message*))
          (send-msg *bot* *channel* (format nil "Title: ~A (at ~A)" (or title "<unknown title>") (puri:uri-host (puri:uri url)))))
      (error ()
        (values)))))

(defun has-url-p (string)
  (when (scan "https?://.*[.$| |>]" string) t))

(defun grab-url (string)
  (find-if #'has-url-p (split "[\\s+><,]" string)))
