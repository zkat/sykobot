(in-package :sykobot)

(defproto interpreter-bot ((proto 'command-bot))
  ((people-to-interpret nil)))

(defreply init-sheep :after ((bot (proto 'interpreter-bot)) &key)
  (setf (people-to-interpret bot) nil))

(defparameter *language-codes*
  '(("Afrikaans" . "Af") ("Albanian" . "Sq") ("Amharic" . "Am") ("Arabic" . "Ar") 
    ("Armenian" . "Hy") ("Azerbaijani" . "Az") ("Basque" . "Eu") ("Belarusian" . "Be")
    ("Bengali" . "Bn") ("Bihari" . "Bh") ("Bulgarian" . "Bg") ("Burmese" . "My") ("Catalan" . "Ca")
    ("Cherokee" . "Chr") ("Chinese" . "Zh") ("Chinese-Simplified" . "Zh-Cn")
    ("Chinese-Traditional" . "Zh-Tw") ("Croatian" . "Hr") ("Czech" . "Cs") ("Danish" . "Da")
    ("Dhivehi" . "Dv") ("Dutch" . "Nl") ("English" . "En") ("Esperanto" . "Eo") ("Estonian" . "Et")
    ("Filipino" . "Tl") ("Finnish" . "Fi") ("French" . "Fr") ("Galician" . "Gl") ("Georgian" . "Ka")
    ("German" . "De") ("Greek" . "El") ("Guarani" . "Gn") ("Gujarati" . "Gu") ("Hebrew" . "Iw")
    ("Hindi" . "Hi") ("Hungarian" . "Hu") ("Icelandic" . "Is") ("Indonesian" . "Id")
    ("Inuktitut" . "Iu") ("Italian" . "It") ("Japanese" . "Ja") ("Kannada" . "Kn") ("Kazakh" . "Kk")
    ("Khmer" . "Km") ("Korean" . "Ko") ("Kurdish" . "Ku") ("Kyrgyz" . "Ky") ("Laothian" . "Lo")
    ("Latvian" . "Lv") ("Lithuanian" . "Lt") ("Macedonian" . "Mk") ("Malay" . "Ms")
    ("Malayalam" . "Ml") ("Maltese" . "Mt") ("Marathi" . "Mr") ("Mongolian" . "Mn")
    ("Nepali" . "Ne") ("Norwegian" . "No") ("Oriya" . "Or") ("Pashto" . "Ps") ("Persian" . "Fa")
    ("Polish" . "Pl") ("Portuguese" . "Pt-Pt") ("Punjabi" . "Pa") ("Romanian" . "Ro")
    ("Russian" . "Ru") ("Sanskrit" . "Sa") ("Serbian" . "Sr") ("Sindhi" . "Sd") ("Sinhalese" . "Si")
    ("Slovak" . "Sk") ("Slovenian" . "Sl") ("Spanish" . "Es") ("Swahili" . "Sw") ("Swedish" . "Sv")
    ("Tajik" . "Tg") ("Tamil" . "Ta") ("Tagalog" . "Tl") ("Telugu" . "Te") ("Thai" . "Th")
    ("Tibetan" . "Bo") ("Turkish" . "Tr") ("Ukrainian" . "Uk") ("Urdu" . "Ur") ("Uzbek" . "Uz")
    ("Uighur" . "Ug") ("Vietnamese" . "Vi") ("Unknown" . "")))

(defcommand interpret ("(\\S+)\\s*into\\s*(\\S+)" nick output-lang)
  "Syntax: 'interpret <output-lang> into <lang>' - Turns on automatic translation of ~
everything <nick> says into <output-lang>. <output-lang> should be one of the bot's ~
available languages. Use 'nointerpret <nick>' to stop it."
  (let ((lang (cond ((< 2 (length output-lang))
		     (cdr (assoc output-lang *language-codes* :test #'string-equal)))
		    ((null output-lang)
		     "en")
		    (t (error "Unknown language: ~A" output-lang)))))
    (setf (alref nick (alref *channel* (people-to-interpret *bot*))) lang)
    (build-string "Very well, I'll translate everything ~A says into ~A" nick output-lang)))

(defcommand available-languages ()
  (build-string "~{~A~^, ~}" (remove "Unknown"
				    (loop for (lang . code) in *language-codes*
				       collect lang)
				    :test #'string-equal)))

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

