;;;; Copyright 2009 Kat Marchan
;;;;
;;;; This file is part of sykobot.
;;;;
;;;; For licensing and warranty information, refer to COPYING
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package :sykobot)

;; ;;; Response stack
;; ;;; Used to assist piping and multiple return values
;; (defvar *responses*)

;;; Modularization of commands
(defproto command-bot ((proto 'listener-bot))
  ((commands (make-hash-table :test #'equal))
   (detection-regex nil)))

(defreply init-sheep :after ((proto 'command-bot) &key)
  (setf (commands proto) (make-hash-table :test #'equal)))

;;; Detection regex handling
(defmessage update-detection-regex (bot))
(defreply update-detection-regex ((bot (proto 'command-bot)))
  (setf (detection-regex bot)
        (create-scanner (build-string "^~A[:,] " (nickname bot))
                        :case-insensitive-mode T)))

(defreply init-bot :after ((bot (proto 'command-bot)))
  (update-detection-regex bot))

(defreply nick :after ((bot (proto 'command-bot)) new-nick)
  (declare (ignore new-nick))
  (update-detection-regex bot))

;;; Command handling stuff
(defmessage add-command (bot command function))
(defmessage remove-command (bot command))
(defmessage command-function (bot command))
(defmessage erase-all-commands (bot))
(defmessage list-all-commands (bot))

(defreply add-command ((bot (proto 'command-bot)) command function)
  (setf (gethash command (commands bot)) function))

(defreply remove-command ((bot (proto 'command-bot)) command)
  (remhash command (commands bot)))

(defreply command-function ((bot (proto 'command-bot)) command)
  (with-properties (commands) bot
    (gethash (string-upcase command) commands
             (lambda (bot args sender channel)
               (declare (ignore bot args sender channel))
               (error (build-string "I don't know how to ~A"
                                    command))))))

(defreply erase-all-commands ((bot (proto 'command-bot)))
  (clrhash (commands bot)))

(defreply list-all-commands ((bot (proto 'command-bot)))
  (with-properties (commands) bot
    (hash-table-keys commands)))

;; We declare these variables here, but do not bind them unless we're actually inside
;; the body of a command.
(defvar *bot*)
(defvar *message*)
(defvar *sender*)
(defvar *channel*)

;; A very convenient macro...
(defmacro defcommand (name (&optional (regex "") &rest vars) &body body)
  `(add-command (proto 'command-bot) (symbol-name ',name)
                (lambda (*bot* *message* *sender* *channel*)
                  (declare (ignorable *message* *bot* *sender* *channel*))
                  ,@(if vars
                        `((register-groups-bind ,vars (,regex *message*)
                            ,@body))
                        `(,@body)))))

;;; Command processing

;;; Checks if a message is applicable for the bot.
;;; If so, returns the command section.
;;; CALLED BY: command-listener, undeafen-listener
(defmessage get-message-index (bot message))
(defreply get-message-index ((bot (proto 'sykobot)) message)
  (nth-value 1 (scan (detection-regex bot) message)))

;;; When a message is applicable for the bot, responds to it.
;;; CALLED BY: call-listeners
;;; CALLS: respond-to-message
;;;  - Adlai
(deflistener command-listener
  (let ((index (get-message-index *bot* *message*)))
    (when index
      (restartable (respond-to-message *bot* *sender* *channel*
				       (subseq *message* index))))))

(defmessage respond-to-message (bot sender channel message))
(defmessage get-responses (bot cmd args sender channel))
(defmessage process-command-string (bot string sender channel &optional pipe-input))

;;; Removes the direct message indicator from a message,
;;;   and then splits it into a command and arguments.
;;; CALLED BY: command-listener
;;; CALLS: process-command-string
;;;  - Adlai
(defreply respond-to-message ((bot (proto 'sykobot))
                              (sender (proto 'string))
                              (channel (proto 'string))
                              (message (proto 'string)))
  (destructuring-bind (command &optional args)
      (split "\\s+" message :limit 2)
    (send-reply bot channel sender
                (funcall (command-function bot command)
			 bot args sender channel)))
  #+nil (let* ((results (process-command-string bot message sender channel)))
          (loop for result in results
             do (send-reply bot channel sender (build-string result)))))

;;; Pipes don't work atm.
;; ;;; Parses a string into piped commands, and manages piping their
;; ;;;   inputs and outputs together, collecting their results.
;; ;;; RECURSIVE!!!
;; ;;; CALLED BY: respond-to-message, process-command-string
;; ;;; CALLS: get-responses, process-command-string
;; ;;;  - Adlai
;; (defreply process-command-string ((bot (proto 'sykobot)) string sender channel &optional pipe-input)
;;   (let* ((head+tail (split "\\s*\\|\\s*" string :limit 2))
;;          (command (if pipe-input (concatenate 'string (car head+tail) " " pipe-input) (car head+tail)))
;;          (cmd+args (split "\\s+" command :limit 2))
;;          (responses (get-responses bot (car cmd+args) (cadr cmd+args) sender channel))
;;          (results (if (cadr head+tail)
;;                       (loop for response in responses
;;                          collect (process-command-string bot (cadr head+tail)
;;                                                          sender channel response))
;;                       responses)))
;;     (flatten results)))

;; ;;; Calls the function representing a command.
;; ;;; CALLED BY: process-command-string
;; ;;; CALLS: command-function
;; ;;;  - Adlai
;; (defreply get-responses ((bot (proto 'sykobot)) cmd args sender channel)
;;   (let ((fn (command-function bot cmd)))
;;     (funcall fn bot args sender channel)))

;;; (defparameter *cmd-prefix* "@")

;; ;;; Puts message responses on the response stack
;; (defun cmd-msg (message &rest format-args)
;;   (push (apply #'build-string message format-args)
;;         *responses*))

;;; Deafness
(defcommand shut ("(\\S+)*" arg1)
  (when (equalp arg1 "up")
    (toggle-deafness *bot* *channel*)
    "Fine. Be that way. Tell me to talk when you realize ~
       just how lonely and pathetic you really are."))
(deflistener undeafen
  (let ((index (get-message-index *bot* *message*)))
    (when (and index
               (let ((diff (mismatch "talk" *message*
                                     :test #'char=
                                     :start2 index)))
                 (or (not diff)
                     (> diff 3))))
      (toggle-deafness *bot* *channel*)
      (send-reply *bot* *channel* *sender*
                  "bla bla blah. Happy?"))))

;;; base commands
(defcommand echo ("(.*)" string)
  string)
(defcommand source ()
  "I'm licensed under the AGPL, you can find my source code at: http://github.com/zkat/sykobot")
(defcommand version ()
  "Pfft. I have no versions. I'm 100% git")
(defcommand help ()
  "No.")
(defcommand commands ()
  (build-string "Available commands are ~{~A~^ ~}"
                (list-all-commands *bot*)))
(defcommand topic ("(.*)" new-topic)
  (if (< 0 (length new-topic))
      (topic *bot* *channel* new-topic) ; Slightly broken
      (topic *bot* *channel*)))
(defcommand ping () "pong")
(defcommand hi () "Go away.")
(defcommand language () "I'm \"Lost In Stupid Parentheses\"")

;;; Give is currently broken
#+nil (defcommand give ("(\\S+) (\\S+) (.*)$" new-target new-command new-args)
        (setf *sender* new-target)
        (setf *responses* (get-responses *bot* new-command new-args new-target *channel*)))

;;; These are broken until encoding issues can be finalized.
;; ;;; Character Decoding
;; (defcommand code->char ("(\\S+)*" code-string)
;;   (let ((code (if code-string (parse-integer code-string :junk-allowed T) 0)))
;;     (build-string "~:[Invalid code~;~:*~A~]"
;;                   (and (integerp code) (/= code 127) (>= code 32)
;;                        (code-char code)))))

;; (defcommand char->code ("(\\S+)*" char-string)
;;   (let ((code (and char-string (char-code (elt char-string 0)))))
;;     (build-string  "~:[Invalid character~;~A~]"
;;                    (and (integerp code) (/= code 127) (>= code 32)
;;                         code))))

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

(defun search-url (engine-regex query)
  (build-string engine-regex (regex-replace-all "\\s+" query "+")))

;;; Google
(defcommand google ("(.*)" query)
  (multiple-value-bind (title url)
      (google-search query)
    (build-string "~:[~;~A ~]<~A>" title title url)))

(defun google-search (query)
  (url-info (search-url
             "http://google.com/search?filter=1&safe=on&q=~A&btnI"
             query)))

;;; ArchWiki 
(defcommand wiki ("(.*)" query)
  (multiple-value-bind (title url)
      (wiki-search query)
    (build-string "~:[~;~A ~]<~A>" title title url)))

(defun wiki-search (query)
  (url-info (search-url
             "http://wiki.archlinux.org/index.php/Special:Search?search=~A"
             query)))

;;; AUR 
(defcommand aursearch ("(.*)" query)
  (multiple-value-bind (title url)
      (aur-search query)
    (build-string "~:[~;~A ~]<~A>" title title url)))

(defun aur-search (query)
  (url-info (search-url
             "http://aur.archlinux.org/rpc.php?type=search&arg=~A"
             query)))

;;; BBS 
(defcommand bbs ("(.*)" query)
  (multiple-value-bind (title url)
      (bbs-search query)
    (build-string "~:[~;~A ~]<~A>" title title url)))


(defun bbs-search (query)
  (url-info (search-url
             "http://bbs.archlinux.org/search.php?action=search&keywords=~A"
             query)))

;;; CLiki search
(defcommand cliki ("(.*)" query)
  (multiple-value-bind (links numlinks)
      (cliki-urls query)
    (build-string "I found ~D result~:P.~@[ Check out <~A>.~]"
                  numlinks (car links))))

;;; CLIKI
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
(defcommand kiloseconds ("(.*)" zone)
  (when (zerop (length zone)) (setf zone "0"))
  (let ((parsed-zone (parse-integer zone :junk-allowed t)))
    (if parsed-zone
        (let ((ks-time (get-ks-time parsed-zone)))
          (build-string "The time in GMT~@D is ~3$ ks."
                        (- (mod (+ 11 parsed-zone) 24) 11)
                        ks-time))
        "Invalid timezone.")))

(defun get-ks-time (&optional (gmt-diff 0))
  (multiple-value-bind
        (seconds minutes hours date month year day light zone)
      (get-decoded-time)
    (declare (ignore date month year day light))
    (/ (+ seconds
          (* 60 (+ minutes
                   (* 60 (mod (+ hours zone gmt-diff) 24)))))
       1000)))

;;; Parrot
(deflistener parrot
  (send-msg *bot* *channel* *message*))
(defcommand parrot ()
  (if (listener-active-p *bot* *channel* 'parrot)
      (progn
        (listener-off *bot* *channel* 'parrot)
        "NODOUCHE")
      (progn
        (listener-on *bot* *channel* 'parrot)
        "TIME TO BE A DOUCHEBAG")))
(defcommand noparrot ()
  (listener-off *bot* *channel* 'parrot)
  "NODOUCHE")

;;; URLs
(deflistener scan-for-url
  (do-register-groups (link) ("(https?://.*?)(?:>|[.,]? |$)" *message*)
    (multiple-value-bind (title url) (url-info link)
      (send-msg *bot* *channel*
                (build-string "Title: ~A (at ~A)"
                              (or title "<unknown title>")
                              (puri:uri-host (puri:uri url)))))))

;; ;;; Aliasing commands
;; ;;; Don't stress this with crazy regexp aliases. It only works
;; ;;;   for text-to-text aliases, without any regex stuff.
;; (defcommand alias ("(\\S+) (.*)$" alias expansion)
;;   (add-alias *bot*
;;              (build-string "(?i)(~A[:,])~A(?: |$)"
;;                            (nickname *bot*) alias)
;;              (build-string "\\1~A " expansion))
;;   (cmd-msg "Alright, alias added."))

;; (defcommand remove-alias ("(\\S+)" alias)
;;   (remove-alias *bot*
;;                 (print (build-string "(?i)(~A[:,])~A(?: |$)"
;;                                      (nickname *bot*) alias)))
;;   (cmd-msg "Done. Alias removed."))

;; ;;;'Filters'
;; (defparameter *english->l33t* '(("a" . "4") ("b" . "|3") ("c" . "<") ("d" . "|)") ("e" . "3")
;;                                 ("f" . "|=") ("g" . "9") ("h" . "|-|") ("i" . "1") ("j" . "_|")
;;                                 ("k" . "|<") ("l" . "|_") ("m" . "/\\/\\") ("n" . "|\\|")
;;                                 ("o" . "0") ("p" . "p") ("q" . "q") ("r" . "|2") ("s" . "5")
;;                                 ("t" . "7") ("u" . "|_|") ("v" . "\\/") ("w" . "\\/\\/") ("x" . "><")
;;                                 ("y" . "y") ("z" . "z")))

;; (defcommand leet ("(.*)" input)
;;   (let ((translated-string input))
;;     (loop for translation in *english->l33t*
;;        when translation
;;        do (setf translated-string (regex-replace-all (ppcre:create-scanner (car translation)
;;                                                                            :case-insensitive-mode t)
;;                                                      translated-string
;;                                                      (cdr translation))))
;;     (cmd-msg translated-string)))

;; (defcommand capitalise ("(.*)" input)
;;   (cmd-msg (string-upcase input)))

(defcommand singa ()
  (build-string
   "I love to singa~@
    about the moon-a and a june-a and a spring-a~@
    I love to singa"))

(defcommand translate ("(\\S+) (\\S+) (.*)" input-lang output-lang text)
  (if (and (= (length output-lang) 2)
           (or (= (length input-lang) 2)
               (string= input-lang "*")))
      (let* ((lang-pair (merge-strings "|" (if (string= input-lang "*") ""
                                               input-lang)
                                       output-lang))
             (json-result
              (drakma:http-request "http://ajax.googleapis.com/ajax/services/language/translate"
                                   :parameters `(("v" . "1.0") ("q" . ,text) ("langpair" . ,lang-pair))))
             (response (json:decode-json-from-string json-result)))
        (case (alref :response-status response)
          (200 (decode-html-string
                (alref :translated-text
                       (alref :response-data response))))
          (T (build-string "Error: ~A"
                           (alref :response-details response)))))
      "Language specifications need to be 2 letters long."))

(defcommand weather ("(.+)" location)
  (let* ((location-data
	  (json:decode-json-from-string 
	   (map 'string #'code-char 
		(drakma:http-request 
		 "http://ws.geonames.org/searchJSON"
		 :parameters `(("q" . ,location) ("maxRows" . "1"))))))
	 (lat (format nil "~A" (alref :lat (car (alref :geonames location-data)))))
	 (lng (format nil "~A" (alref :lng (car (alref :geonames location-data)))))
	 (weather-data
	  (alref :weather-observation
		 (json:decode-json-from-string
		  (map 'string #'code-char
		       (drakma:http-request
			"http://ws.geonames.org/findNearByWeatherJSON"
			:parameters `(("lat" . ,lat) ("lng" . ,lng)))))))
	 (cloudyness (alref :clouds weather-data))
	 (temp (alref :temperature weather-data))
	 (station-name (alref :station-name weather-data)))
    (if weather-data
      (build-string 
       "there are ~A and the temperature is ~A°C at ~A"
       cloudyness temp station-name)
      (build-string "I couldn't find the weather for ~A" location))))

;; (defcommand reverse ("(.*)" input)
;;   (cmd-msg (reverse input)))
