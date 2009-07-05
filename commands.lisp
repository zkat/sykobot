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

(let ((listener-table (make-hash-table :test #'equalp))
      (active-listeners ()))
      (defun add-listener (lr fn)
	(setf (gethash lr listener-table) fn))

      (defun remove-listener (lr)
	(remhash lr listener-table))

      (defun listener-function (lr)
	(multiple-value-bind (fn hasp)
	    (gethash lr listener-table)
	  (if hasp
	      fn
	      (lambda (bot sender channel message)
		     (print "listener doesn't exist")))))

      (defun activate-listener (lr)
	(pushnew lr active-listeners :test #'equalp))

      (defun deactivate-listener (lr)
	(setf active-listeners (remove lr active-listeners :test #'equalp)))

      (defun call-listener (lr bot sender channel message)
	(funcall (listener-function lr) bot sender channel message))

      (defun call-listeners (bot sender channel message)
	(loop for lr in active-listeners
	     do (call-listener lr bot sender channel message)))

      (defun get-l-t ()
	listener-table)

      (defun get-a-l ()
	active-listeners))


(defmacro deflistener (name &body body)
  `(add-listener ,name
		(lambda (bot sender channel message)
		  ,@body)))

;;; general utils
(defun think (bot channel &optional (minimum-delay 0))
  (send-action bot channel "thinks")
  (sleep (+ 8 minimum-delay))
  (send-msg bot channel "Hmm.... Indeed."))

(defun pause-in-thought (bot channel &key (max-time 5) (action-probability 2))
  (if (zerop (random action-probability))
      (think bot channel))
  (sleep (1+ (random max-time))))

;;; base commands
(defcommand "think"
  (think *bot* *channel* 2))
(defcommand "echo"
  (send-msg *bot* *channel* *args*))
(defcommand "ping"
  (send-reply *bot* *sender* *channel* "pong"))
(defcommand "<3"
  (send-reply *bot* *sender* *channel* "<3 <3 <3 OMG <3 <3 <3"))
(defcommand "shut" 
  (when (scan "up" *args*)
    (send-reply *bot* *sender* *channel*
                (format nil "Fine. Be that way. Tell me to talk when you realize ~
                           just how lonely and pathetic you really are."))
    (shut-up *bot*)))
(defcommand "help" 
  (send-reply *bot* *sender* *channel* "No."))
(defcommand "hi" 
  (send-reply *bot* *sender* *channel* "Go away."))
(defcommand "code->char" 
  (let ((char (code-char (read-from-string (car (split "\\s+" *args*))))))
    (send-msg *bot* *channel* (format nil "~A" char))))
(defcommand "char->code" 
  (let ((code (char-code (elt *args* 0))))
    (send-msg *bot* *channel* (format nil "~A" code))))
(defcommand "give"
  (register-groups-bind (new-target new-command new-args)
      ("(\\S+) (\\S+) (.*)$" *args*)
    (answer-command *bot* new-command new-args new-target *channel*)))
(defcommand "exit" 
  (send-reply *bot* *sender* *channel* "1 ON 1 FAGGOT"))

;;; Google
(defcommand "google"
  (multiple-value-bind (title url)
      (google-search *args*)
    (send-reply *bot* *sender* *channel*
                (format nil "~A <~A>" title url))))

(defun google-search (query)
  (url-info (search-url
             "http://google.com/search?filter=1&safe=on&q=~A&btnI"
             query)))

(defun search-url (engine query)
  (format nil engine (regex-replace-all "\\s+" query "+")))

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

;;; More
(defcommand "chant" 
  (send-msg *bot* *channel* (format nil "MORE ~:@(~A~)" *more*)))
(defvar *more* "lulz")
(defvar *prepositions*
  '("aboard"  "about"  "above"  "across"  "after"  "against"  "along"  "among"  "around"  "as"   "at"
    "before"  "behind"   "below" "beneath" "beside"  "between"  "beyond"  "but" "except"  "by"
    "concerning"  "despite"  "down"  "during"  "except" "for"  "from"  "in"  "into"  "like" "near"
    "of"  "off"  "on"  "onto"  "out"  "outside"  "over"  "past"  "per"  "regarding"  "since"  "through"
    "throughout"  "till"  "to"  "toward"  "under" "underneath"  "until"  "up"   "upon"  "with"
    "within" "without"))
(defvar *conjunctions*
  '("for" "and" "nor" "but" "or" "yet" "so"))
(defvar *articles*
  '("an" "a" "the"))


(deflistener "scan-for-more"
  (let ((s message))
    (let ((str (nth-value
		1 (scan-to-strings "[MORE|MOAR]\\W+((\\W|[A-Z0-9])+)([A-Z0-9])($|[^A-Z0-9])" s))))
      (or
       (and str
	    (setf *more* (concatenate 'string (elt str 0) (elt str 2))))
       (let ((str (nth-value 1 (scan-to-strings "(?i)[more|moar]\\W+(\\w+)\\W+(\\w+)\\W+(\\w+)" s))))
	 (or
	  (and str
	       (or (member (elt str 0) *prepositions* :test #'string-equal)
		   (member (elt str 0) *conjunctions* :test #'string-equal)
		   (member (elt str 0) *articles* :test #'string-equal))
	       (or (member (elt str 1) *prepositions* :test #'string-equal)
		   (member (elt str 1) *conjunctions* :test #'string-equal)
		   (member (elt str 1) *articles* :test #'string-equal))
	       (setf *more* (string-upcase
			     (concatenate 'string (elt str 0) " " (elt str 1)
					  " " (elt str 2)))))
	  (let ((str (nth-value 1 (scan-to-strings "(?i)[more|moar]\\W+(\\w+)\\W+(\\w+)" s))))
	    (or
	     (and str
		  (or (member (elt str 0) *prepositions* :test #'string-equal)
		      (member (elt str 0) *conjunctions* :test #'string-equal)
		      (member (elt str 0) *articles* :test #'string-equal))
		  (setf *more* (string-upcase
				(concatenate 'string (elt str 0) " " (elt str 1)))))
	     (let ((str (nth-value 1 (scan-to-strings "(?i)[more|moar]\\W+(\\w+)" s))))
	       (or
		(and str (setf *more* (string-upcase (elt str 0))))))))))))))
(activate-listener "scan-for-more")


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
    (clrhash memo-table))
  )

(deflistener "send-memos"
  (let* ((recipient sender)
	 (memo (get-and-remove-memo recipient)))
    (when memo
      (destructuring-bind (text who-from) memo
	(send-reply bot recipient channel (format nil "You have a memo!"))
	(send-reply bot recipient channel (format nil "~A - ~A" text who-from))))))
(activate-listener "send-memos")

;;; Cliki search
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



(deflistener "parrot"
  (send-msg bot channel message))
(defcommand "parrot"
  (activate-listener "parrot"))
(defcommand "noparrot"
  (deactivate-listener "parrot"))


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

(deflistener "scan-for-fact"
  (loop for statement in (split-into-sub-statements message)
       do (do-register-groups (article noun verb info)
	    (".*?([a|an|the|this|that]*)\\s*(\\w+)\\s+(is|are|isn't|ain't)\\s+(.+)"
	     statement)
	    (set-fact noun (format nil "~A ~A ~A ~A" article noun verb info)))))
(activate-listener "scan-for-fact")

(defcommand "fact"
  (send-msg *bot* *channel* (get-fact *args*)))

(deflistener "scan-for-url"
  (when (and (has-url-p message)
             (not (string-equal sender (nickname bot))))
    (handler-case
        (multiple-value-bind (title url)
            (url-info (grab-url message))
          (send-msg bot channel (format nil "Title: ~A (at ~A)" title (puri:uri-host (puri:uri url)))))
      (error ()
        (values)))))
(activate-listener "scan-for-url")

(defun has-url-p (string)
  (when (scan "https?://.*[.$| |>]" string) t))

(defun grab-url (string)
  (find-if #'has-url-p (split "[\\s+><,]" string)))
