;;;; Copyright 2009 Kat Marchan
;;;;
;;;; This file is part of sykobot.
;;;;
;;;; For licensing and warranty information, refer to COPYING
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package :sykobot)

;;; Modularization of commands
(defproto command-bot ((proto 'listener-bot))
  ((commands (make-hash-table :test #'equal))
   (detection-regex nil)
   (command-prefix "@")))

(defproto command ()
  ((cmd-function (constantly "OOPS. Don't go here."))
   (dox "No documentation available.")))

;;; Detection regex handling
(defmessage update-detection-regex (bot))
(defreply update-detection-regex ((bot (proto 'command-bot)))
  (setf (detection-regex bot)
        (create-scanner (build-string "^(?:~A[:,] |~A)" (nickname bot) (command-prefix bot))
                        :case-insensitive-mode T)))

(defreply (setf command-prefix) :after (new-value (bot (proto 'command-bot)))
  "Keeps the detection-regex up-to-date."
  (declare (ignore new-value))
  (update-detection-regex bot))

(defreply (setf nickname) :after (new-value (bot (proto 'command-bot)))
  "Keeps the detection-regex up-to-date."
  (declare (ignore new-value))
  (update-detection-regex bot))

;;; Command handling stuff
(define-condition unknown-command (error)
  ((command-name :reader command-name
                 :initarg :command-name))
  (:report (lambda (condition stream)
             (format stream "Unknown command: ~A"
                     (command-name condition)))))

(defmessage add-command (bot name command))
(defmessage remove-command (bot command))
(defmessage find-command (bot name))
(defmessage command-function (bot command))
(defmessage erase-all-commands (bot))
(defmessage list-all-commands (bot))

(defreply add-command ((bot (proto 'command-bot)) name command)
  (setf (gethash name (commands bot)) command))

(defreply remove-command ((bot (proto 'command-bot)) name)
  (remhash name (commands bot)))

(defreply find-command ((bot (proto 'command-bot)) name)
  (with-properties (commands) bot
    (gethash (string-upcase name) commands)))

(defreply command-function ((bot (proto 'command-bot)) name)
  (or (let ((cmd (find-command bot name)))
	(when cmd (cmd-function cmd)))
      (error 'unknown-command :command-name name)))

(defreply erase-all-commands ((bot (proto 'command-bot)))
  (clrhash (commands bot)))

(defreply list-all-commands ((bot (proto 'command-bot)))
  (with-properties (commands) bot
    (hash-table-keys commands)))

;; A very convenient macro...
(defmacro defcommand (name (&optional (regex "") &rest vars) &body body)
  (let ((documentation nil)
	(real-body nil))
    (if (and (stringp (car body))
	     (cdr body))
	(progn
	  (setf documentation (car body))
	  (setf real-body (cdr body)))
	(setf real-body body))
    `(add-command (proto 'command-bot) (symbol-name ',name)
		  (defclone ((proto 'command))
		      ((cmd-function
                        (lambda ()
                          ,@(if vars
                                `((or (register-groups-bind ,vars (,regex *message*)
                                        ,@real-body)
                                      (error ,(build-string "Not enough arguments to ~A. Try 'help ~:*~A'"
                                                            name))))
                                `(,@real-body))))
		       ,@(when documentation
			       `((dox ,documentation))))))))

;;; Command processing
(defmessage get-message-index (bot message)
  (:documentation "Checks if a message is applicable for the bot. If so,
it returns the command section of the message."))
(defreply get-message-index ((bot (proto 'sykobot)) message)
  (nth-value 1 (scan (detection-regex bot) message)))

(deflistener command-listener
  "When a message is applicable for the bot, respond to it."
  (let ((index (get-message-index *bot* *message*)))
    (when index
      (restartable (respond-to-message *bot* *sender* *channel*
				       (subseq *message* index))))))

(defmessage respond-to-message (bot sender channel message))
(defmessage get-responses (bot cmd args sender channel))
(defmessage process-command-string (bot string sender channel &optional pipe-input))

(defreply respond-to-message ((bot (proto 'sykobot))
                              (sender (proto 'string))
                              (channel (proto 'string))
                              (message (proto 'string)))
  "Removes the direct message indicator from a message, and then
splits it into a command and arguments"
  (destructuring-bind (command &optional *message*)
      (split "\\s+" message :limit 2)
    (send-reply bot channel sender
                (funcall (command-function bot command))))


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
  "Syntax: 'echo <string>' - Echoes back STRING."
  string)
(defcommand reverse ("(.*)" input)
  "Syntax: 'reverse <string>' - Reverses the input."
  (reverse input))
(defcommand help ("(.*)" cmd-name)
  "Syntax: 'help [<cmd-name>]' - If cmd-name is provided, dumps the docstring for that command. ~
Otherwise, it dumps a generic help string."
  (if (<= 1 (length cmd-name))
      (let ((cmd (find-command *active-bot* cmd-name)))
	(if cmd 
	    (dox cmd)
	    (build-string "I don't know any command called ~A" cmd-name)))
      "Tell me 'help <cmd-name>' for more information on a particular command."))
(defcommand source ()
  "Syntax: 'source' - Dumps information about the bot's source code."
  "I'm licensed under the AGPL, you can find my source code at: http://github.com/zkat/sykobot")
(defcommand version ()
  "Syntax: 'version' - Current version of sykobot."
  "Pfft. I have no versions. I'm 100% git")
(defcommand commands ()
  "Syntax: 'commands' - Lists all available commands"
  (build-string "Available commands are ~{~A~^ ~}"
                (list-all-commands *bot*)))
(defcommand topic ("(.*)" new-topic)
  "Syntax: 'topic [<new-topic>]' - If new-topic is provided, and the bot has topic-setting~
privileges, it sets the channel's topic. Otherwise, it dumps the current topic."
  (if (< 0 (length new-topic))
      (topic *bot* *channel* new-topic) ; Slightly broken
      (topic *bot* *channel*)))
(defcommand ping () 
  "Syntax: 'ping' - Pongs you."
  "pong")
(defcommand hi ()
  "This is undocumented. Fuck you. Don't care."
  "Go away.")
(defcommand language () 
  "Syntax: 'language' - Returns information about the bot's implementation language."
  "I'm \"Lost In Stupid Parentheses\"")
;;; Give is currently broken
#+nil (defcommand give ("(\\S+) (\\S+) (.*)$" new-target new-command new-args)
        (setf *sender* new-target)
        (setf *responses* (get-responses *bot* new-command new-args new-target *channel*)))

;;; Character Decoding
;; These are broken until encoding issues can be finalized. -Adlai
;; I'll worry about it later. Worksfornow -syko
(defcommand code->char ("(\\S+)*" code-string)
  "Syntax: 'code->char <number>' - Converts <number> into its utf-8 representation."
  (let ((code (if code-string (parse-integer code-string :junk-allowed T) 0)))
    (build-string "~:[Invalid code~;~A~]"
                  (and (integerp code) (/= code 127) (>= code 32))
		  (code-char code))))

(defcommand char->code ("(\\S+)*" char-string)
  "Syntax: 'char->code <arg>' - Takes the first character seen in <arg> and converts it to its ~
utf-8 code."
  (let ((code (and char-string (char-code (elt char-string 0)))))
    (build-string  "~:[Invalid character~;~A~]"
                   (and (integerp code) (/= code 127) (>= code 32))
		   code)))

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
  "Syntax: 'google <query> - Does a google search and returns the top-ranking result."
  (multiple-value-bind (title url)
      (google-search query)
    (build-string "~:[~;~A ~]<~A>" title title url)))

(defun google-search (query)
  (url-info (search-url
             "http://google.com/search?filter=1&safe=on&q=~A&btnI"
             query)))

;;; ArchWiki 
(defcommand wiki ("(.*)" query)
  "Syntax: 'wiki <query> - Searches the Arch Linux wiki."
  (multiple-value-bind (title url)
      (wiki-search query)
    (build-string "~:[~;~A ~]<~A>" title title url)))

(defun wiki-search (query)
  (url-info (search-url
             "http://wiki.archlinux.org/index.php/Special:Search?search=~A"
             query)))

;;; AUR 
(defcommand aursearch ("(.*)" query)
  "Syntax: 'aursearch <query> - Searches the Arch User Repository."
  (multiple-value-bind (title url)
      (aur-search query)
    (build-string "~:[~;~A ~]<~A>" title title url)))

(defun aur-search (query)
  (url-info (search-url
             "http://aur.archlinux.org/rpc.php?type=search&arg=~A"
             query)))

;;; BBS 
(defcommand bbs ("(.*)" query)
  "Syntax: 'bbs <query> - Searches the Arch Linux BBS."
  (multiple-value-bind (title url)
      (bbs-search query)
    (build-string "~:[~;~A ~]<~A>" title title url)))


(defun bbs-search (query)
  (url-info (search-url
             "http://bbs.archlinux.org/search.php?action=search&keywords=~A"
             query)))

;;; CLiki search
(defcommand cliki ("(.*)" query)
  "Syntax: 'bbs <query>' - Searches CLiki."
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
(defcommand kiloseconds ("(\\S+)?" zone-string)
  "Syntax: 'kiloseconds [<zone>]' - The current time in kiloseconds. Optionally, a time zone~
 can be provided in the format [+-]NUM. Example: 'kiloseconds -5'."
  (when (null zone-string) (setf zone-string "0"))
  (let ((zone (parse-integer zone-string :junk-allowed t)))
    (if zone
        (build-string "The time is ~A." (get-kilosecond-timestamp (get-universal-time) zone))
        "Invalid timezone.")))

;;; Parrot
(deflistener parrot
  (send-msg *bot* *channel* *message*))
(defcommand parrot ()
  "Syntax: 'parrot' - Turns the bot into an auto-echoing douchebag."
  (if (listener-active-p *bot* *channel* 'parrot)
      (progn
        (listener-off *bot* *channel* 'parrot)
        "NODOUCHE")
      (progn
        (listener-on *bot* *channel* 'parrot)
        "TIME TO BE A DOUCHEBAG")))
(defcommand noparrot ()
  "Syntax: 'noparrot' - stops the madness."
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
  "I love to singa
about the moon-a and a june-a and a spring-a
I love to singa"
  (build-string
   "I love to singa~@
    about the moon-a and a june-a and a spring-a~@
    I love to singa"))

(defcommand error ()
  "This accepts no arguments. It makes lisp signal an error, to make sure they're handled properly."
  (error "This is a test error.")
  "Uh oh")

(defcommand translate ("(\\S+) (\\S+) (.*)" input-lang output-lang text)
  "Syntax: 'translate <input-lang> <output-lang> <text>' - translates TEXT from input-lang into~
 output-lang. Providing '*' as the input-lang will make it auto-detect the from-language."
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
  "Syntax: 'weather <location>' - Tells you the current weather in <location>"
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
