(in-package :sykobot)

(defproto interpreter-bot ((proto 'command-bot))
  ((people-to-interpret nil)))

(defreply init-sheep :after ((bot (proto 'interpreter-bot)) &key)
  (setf (people-to-interpret bot) nil))

(defparameter *language-codes*
  '(("Albanian" . "Sq") ("Arabic" . "Ar") ("Bulgarian" . "Bg") ("Catalan" . "Ca")
    ("Chinese" . "Zh") ("Croatian" . "Hr") ("Czech" . "Cs") ("Danish" . "Da")
    ("Dutch" . "Nl") ("English" . "En") ("Estonian" . "Et") ("Filipino" . "Tl")
    ("Finnish" . "Fi") ("French" . "Fr") ("Galician" . "Gl") ("German" . "De")
    ("Greek" . "El") ("Hebrew" . "Iw") ("Hindi" . "Hi") ("Hungarian" . "Hu")
    ("Indonesian" . "Id") ("Italian" . "It") ("Japanese" . "Ja") ("Korean" . "Ko") 
    ("Latvian" . "Lv") ("Lithuanian" . "Lt") ("Maltese" . "Mt") ("Norwegian" . "No")
    ("Persian" . "Fa") ("Polish" . "Pl") ("Portuguese" . "Pt-Pt") ("Romanian" . "Ro")
    ("Russian" . "Ru") ("Serbian" . "Sr") ("Slovak" . "Sk") ("Slovenian" . "Sl")
    ("Spanish" . "Es") ("Swedish" . "Sv") ("Thai" . "Th") ("Turkish" . "Tr") ("Ukrainian" . "Uk")
    ("Vietnamese" . "Vi")))

(defcommand interpret ("(\\S+)\\s*into\\s*(\\S+)" nick output-lang)
  "Syntax: 'interpret <output-lang> into <lang>' - Turns on automatic translation of ~
everything <nick> says into <output-lang>. <output-lang> should be one of the bot's ~
available languages. Use 'nointerpret <nick>' to stop it."
  (let ((lang (cond ((lang->code output-lang)
		     output-lang)
		    ((null output-lang)
		     "english")
		    (t (error "Unknown language: ~A" output-lang)))))
    (setf (alref nick (alref *channel* (people-to-interpret *bot*))) lang)
    (build-string "Very well, I'll translate everything ~A says into ~A" nick output-lang)))

(defcommand available-languages ()
  (build-string "~{~A~^, ~}"
                (str-remove "Unknown"
                            (loop for (lang . code) in *language-codes*
                               collect lang))))

(defcommand nointerpret ("(.+)" nick)
  "Syntax: 'nointerpret <nick>' - Stop automatically translating whatever <nick> says."
  (setf (alref nick (alref *channel* (people-to-interpret *bot*))) nil)
  (build-string "Rgr. No longer interpreting for ~A" nick))

(deflistener interpreter-listener
  (let ((output-lang (alref *sender* (alref *channel* (people-to-interpret *bot*)))))
    (when output-lang
      (send-msg *bot* *channel* 
		(build-string "<~A> ~A" *sender*
			      (translate "*" output-lang *message*))))))

(defcommand translate ("(\\S+) (\\S+) (.*)" input-lang output-lang text)
  "Syntax: 'translate <input-lang> <output-lang> <text>' - translates TEXT from input-lang into~
 output-lang. Providing '*' as the input-lang will make it auto-detect the from-language."
  (translate input-lang output-lang text))
 
(defun lang->code (language)
  (cdr (str-assoc language *language-codes*)))

(defun translate (input-lang output-lang text)
  (let* ((lang-pair (merge-strings "|" (if (string= input-lang "*") ""
					   (lang->code input-lang))
				   (lang->code output-lang)))
	 (json-result
	  (drakma:http-request "http://ajax.googleapis.com/ajax/services/language/translate"
			       :parameters `(("v" . "1.0") ("q" . ,text) ("langpair" . ,lang-pair))))
	 (response (json:decode-json-from-string json-result)))
    (case (alref :response-status response)
      (200 (decode-html-string
	    (alref :translated-text
		   (alref :response-data response))))
      (T (build-string "Error: ~A"
		       (alref :response-details response))))))
