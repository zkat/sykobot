;;;; Copyright 2009 Kat Marchan
;;;;
;;;; This file is part of sykobot.
;;;;
;;;; For licensing and warranty information, refer to COPYING
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package :sykobot)

;;; Response stack
;;; Used to assist piping and multiple return values
(defvar *responses*)

;;; Modularization of commands
(defproto sykobot-commands ((proto 'sykobot-listeners))
  ((commands (make-hash-table :test #'equal))
   (detection-regex nil)))

;;; Detection regex handling
(defmessage update-detection-regex (bot))
(defreply update-detection-regex ((bot (proto 'sykobot-commands)))
  (setf (detection-regex bot)
        (create-scanner (build-string "^~A[:,] " (nickname bot))
                        :case-insensitive-mode T)))

(defreply init-bot :after ((bot (proto 'sykobot-commands)))
  (update-detection-regex bot))

(defreply nick :after ((bot (proto 'sykobot-commands)) new-nick)
  (declare (ignore new-nick))
  (update-detection-regex bot))

;;; Command handling stuff
(defmessage add-command (bot command function))
(defmessage remove-command (bot command))
(defmessage command-function (bot command))
(defmessage erase-all-commands (bot))
(defmessage list-all-commands (bot))

(defreply add-command ((bot (proto 'sykobot-commands)) command function)
  (setf (gethash command (commands bot)) function))

(defreply remove-command ((bot (proto 'sykobot-commands)) command)
  (remhash command (commands bot)))

(defreply command-function ((bot (proto 'sykobot-commands)) command)
  (gethash (string-upcase command) (commands bot)
           (lambda (bot args sender channel)
             (declare (ignore channel args))
             (send-notice bot sender
                          (build-string "I don't know how to ~A." command))
             (values))))

(defreply erase-all-commands ((bot (proto 'sykobot-commands)))
  (clrhash (commands bot)))

(defreply list-all-commands ((bot (proto 'sykobot-commands)))
  (hash-table-keys (commands bot)))

(defmacro defcommand (name (&optional (regex "") &rest vars) &body body)
  `(add-command (proto 'sykobot-commands) (symbol-name ',name)
                (lambda (*bot* *message* *sender* *channel*)
                  (declare (ignorable *message* *bot* *sender* *channel*))
                  (let ((*responses* nil))
                    ,@(if vars
                          `((register-groups-bind ,vars (,regex *message*)
                              ,@body))
                          `(,@body))
                    *responses*))))

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
      (handler-case
          (respond-to-message *bot* *sender* *channel*
                              (subseq *message* index))
        (error (e) (send-msg *bot* *channel*
                             (build-string "An error occured: ~A" e)))))))

(defmessage respond-to-message (bot sender channel message))
(defmessage get-responses (bot cmd args sender channel))
(defmessage process-command-string (bot string sender channel &optional pipe-input))

;;; Removes the direct message indicator from a message,
;;;   and then splits it into a command and arguments.
;;; CALLED BY: command-listener
;;; CALLS: process-command-string
;;;  - Adlai
(defreply respond-to-message ((bot (proto 'sykobot)) sender channel message)
  (let* ((results (process-command-string bot message sender channel)))
    (loop for result in results
       do (send-reply bot sender channel (build-string "~A" result)))))

;;; Parses a string into piped commands, and manages piping their
;;;   inputs and outputs together, collecting their results.
;;; RECURSIVE!!!
;;; CALLED BY: respond-to-message, process-command-string
;;; CALLS: get-responses, process-command-string
;;;  - Adlai
(defreply process-command-string ((bot (proto 'sykobot)) string sender channel &optional pipe-input)
  (let* ((head+tail (split "\\s*\\|\\s*" string :limit 2))
         (command (if pipe-input (concatenate 'string (car head+tail) " " pipe-input) (car head+tail)))
         (cmd+args (split "\\s+" command :limit 2))
         (responses (get-responses bot (car cmd+args) (cadr cmd+args) sender channel))
         (results (if (cadr head+tail)
                      (loop for response in responses
                         collect (process-command-string bot (cadr head+tail)
                                                         sender channel response))
                      responses)))
    (flatten results)))

;;; Calls the function representing a command.
;;; CALLED BY: process-command-string
;;; CALLS: command-function
;;;  - Adlai
(defreply get-responses ((bot (proto 'sykobot)) cmd args sender channel)
  (let ((fn (command-function bot cmd)))
    (funcall fn bot args sender channel)))

;;; (defparameter *cmd-prefix* "@")

;;; Puts message responses on the response stack
(defun cmd-msg (message &rest format-args)
  (push (apply #'build-string message format-args)
        *responses*))

;;; Deafness
(defcommand shut ("(\\S+)*" arg1)
  (when (equalp arg1 "up")
    (cmd-msg "Fine. Be that way. Tell me to talk when you realize ~
                just how lonely and pathetic you really are.")
    (toggle-deafness *bot* *channel*)))
(deflistener undeafen
  (let ((index (get-message-index *bot* *message*)))
    (when (and index
               (let ((diff (mismatch "talk" *message*
                                      :test #'char=
                                      :start2 index)))
                 (or (not diff)
                     (> diff 3))))
      (toggle-deafness *bot* *channel*)
      (send-reply *bot* *sender* *channel*
                  "bla bla blah. Happy?"))))

;;; base commands
(defcommand echo ("(.*)" string)
  (cmd-msg string))
(defcommand source ()
  (cmd-msg "I'm licensed under the AGPL, you can find my source code at: http://github.com/zkat/sykobot"))
(defcommand version ()
  (cmd-msg "Pfft. I have no versions. I'm 100% git"))
(defcommand help ()
  (cmd-msg "No."))
(defcommand commands ()
  (cmd-msg "Available commands are ~{~A~^ ~}"
           (list-all-commands *bot*)))
(defcommand topic ("(.*)" new-topic)
  (if (< 0 (length new-topic))
      (topic *bot* *channel* new-topic)
      (cmd-msg (topic *bot* *channel*))))
(defcommand ping ()
  (cmd-msg "pong"))
(defcommand hi ()
  (cmd-msg "Go away."))
(defcommand language ()
  (cmd-msg "(((((()())))(((()()(OMFG)))()))(((()))))"))

;;; Give is currently broken
;; (defcommand give ("(\\S+) (\\S+) (.*)$" new-target new-command new-args)
;;   (setf *sender* new-target)
;;   (setf *responses* (get-responses *bot* new-command new-args new-target *channel*)))

;;; Character Decoding
(defcommand code->char ("(\\S+)*" code-string)
  (let ((code (if code-string (parse-integer code-string :junk-allowed T) 0)))
    (cmd-msg "~:[Invalid code~;~:*~A~]" (and (integerp code) (/= code 127) (>= code 32)
                                             (code-char code)))))

(defcommand char->code ("(\\S+)*" char-string)
  (let ((code (and char-string (char-code (elt char-string 0)))))
    (cmd-msg  "~:[Invalid character~;~A~]"
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
  (multiple-value-bind (title url)
      (google-search query)
    (cmd-msg "~:[~;~A ~]<~A>" title title url)))

(defun google-search (query)
  (url-info (search-url
             "http://google.com/search?filter=1&safe=on&q=~A&btnI"
             query)))

;;; CLiki search
(defcommand cliki ("(.*)" query)
  (multiple-value-bind (links numlinks)
      (cliki-urls query)
    (cmd-msg "I found ~D result~:P.~@[ Check out <~A>.~]" numlinks (car links))))

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
          (cmd-msg "The time in GMT~A is ~3$ ks."
                   (if (or (= parsed-zone 0) (plusp parsed-zone))
                       (build-string "+~A" (mod parsed-zone 24))
                       (build-string "~A" (- (mod parsed-zone 24) 24)))
                   ks-time))
        (cmd-msg "Invalid timezone."))))

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
        (cmd-msg "NODOUCHE"))
      (progn
        (listener-on *bot* *channel* 'parrot)
        (cmd-msg "TIME TO BE A DOUCHEBAG"))))
(defcommand noparrot ()
  (listener-off *bot* *channel* 'parrot)
  (cmd-msg "NODOUCHE"))

;;; URLs
(deflistener scan-for-url
  (when (and (has-url-p *message*)
             (not (string-equal *sender* (nickname *bot*))))
    (handler-case
        (multiple-value-bind (title url)
            (url-info (grab-url *message*))
          (send-msg *bot* *channel*
                    (build-string "Title: ~A (at ~A)"
				  (or title "<unknown title>")
				  (puri:uri-host (puri:uri url)))))
      (error ()
        (values)))))

(defun has-url-p (string)
  (when (scan "https?://.*[.$| |>]" string) t))

(defun grab-url (string)
  (find-if #'has-url-p (split "[\\s+><,]" string)))

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

;; (defcommand singa ()
;;   (cmd-msg "I love to singa")
;;   (cmd-msg "about the moon-a and a june-a and a spring-a")
;;   (cmd-msg "I love to singa"))

;; (defcommand translate ("(\\S+) (\\S+) (.*)" input-lang output-lang text)
;;   (if (and (= (length output-lang) 2)
;;            (or (= (length input-lang) 2)
;;                (string= input-lang "*")))
;;       (let* ((lang-pair (merge-strings "|" (if (string= input-lang "*") ""
;;                                                input-lang)
;;                                        output-lang))
;;              (json-result
;;               (drakma:http-request "http://ajax.googleapis.com/ajax/services/language/translate"
;;                                    :parameters `(("v" . "1.0") ("q" . ,text) ("langpair" . ,lang-pair))))
;;              (response (json:decode-json-from-string json-result)))
;;         (case (alref :response-status response)
;;           (200 (cmd-msg (decode-html-string
;;                          (alref :translated-text
;;                                 (alref :response-data response)))))
;;           (T (cmd-msg "Error: ~A"
;;                       (alref :response-details response)))))
;;       (cmd-msg "Language specifications need to be 2 letters long.")))


;; (defcommand reverse ("(.*)" input)
;;   (cmd-msg (reverse input)))

(defcommand error NIL
  (command-function "garbage" 'frobnobdication #'oops))
