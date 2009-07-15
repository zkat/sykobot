;; translation of http://www.strout.net/python/ai/therapist.py

(in-package :sykobot)

(defun translate (str alist)
  "take a string and replace any (key, value) in alist"
  (let ((words (split "\\s+" str)))
    (loop for word in words
          for y from 0
       when (alref word alist)
	 do (setf (elt words y) (alref word alist)))
    (format nil "~{~A~^ ~}" words)))

(defun substitute-groups (str groups)
  "take a string containing $1 $2 etc and substitute with the coresponding element"
  (let ((words (split "\\s+" str)))
    (loop for word in words
       for y from 0
       when (equalp (char word 0) #\$)
       do (let ((ele (parse-integer word :start 1 :junk-allowed t)))
	    (when (and ele (< ele (length groups)))
	      (setf (elt words y) (reflect (elt groups ele))))))
    (format nil "~{~A~^ ~}" words)))

(defvar *reflections* nil)
(setf *reflections* '(("am" . "are") ("are" . "am")
		      ("i" . "you") 
		      ("i'd" . "you would") ("you would" . "i'd")
		      ("i've" . "you have") ("you have" . "i'd")
		      ("i'll" . "you will") ("you will" . "i'll")
		      ("my" . "your") ("your" . "my")
		      ("mines" . "yours") ("yours" . "mine")
		      ("me" . "you") ("you" . "me")))

(defun reflect (str)
  (translate str *reflections*))

(defun respond (string regexen)
  "takes the string, checks through the regexes until a match is made
   and chooses a random response from the response list"
  (or
   (loop for regex+responses in regexen
      do (destructuring-bind (regex responses) regex+responses
	   (multiple-value-bind (match groups) (scan-to-strings regex string)
	     (when match
	       (return (substitute-groups (random-elt responses) groups))))))
   "I have nothing to say to you"))

(defparameter *eliza-responses* 
  '(("hello" ("go away"))
    ("my name is (\\w+)" ("hello $0"
			  "greetings $0"))

;;psychoanalysis
    ("I need (.+)" ("Why do you need $0?"
		    "Would it really help to get $0?"))
    ("I dreamt (that|about|)(.+)" ("You dream $0 $1 often then?"
				   "I dream $0 $1 too!"))
    ("sorry" ("please don't apologise" "yeah, I'm sorry too"
	      "no worries"))
    ("do you remember (.+)" ("How could I forget!"
			     "How could I forget $0"
			     "Is $0 worth remembering"))
    ("if (.+) then (.+)" ("Do you really think it's likely that $0"
			  "Well, it sure would be awesome if $1"))

    ("my mother (.+)" ("Who else in your family $0 ?"
		       "Tell me more about your family"))
    ("I want (.+)" ("Why would anyone want $0"
		    "Yes, that would be nice"))
    ("are you (.+)" ("And if I were $0 - what then?"
		     "Why does it matter?"
		     "Why does it matter if $0"
		     "Would you prefer it if I were $0"
		     "wait, are YOU $0"))
    ("perhaps (.+)" ("Why the uncertainty"
		     "Of course $0"
		     "Well, duh?!"))

    ("I have (.+)" ("Why did you tell me that you've $0"
		    "Now that you have $0 what will you do next?"))
    ("I would (.+)" ("Why would you $0"
		     "Why would you do that?"
		     "Who else did you tell that you'd $0"))
    ("I suspect (.+)" ("I suspect *someone* is hiding something"
		       "I more than suspect so"
		       "I KNOW $0"))


    ("yes" ("orly?"
	    "yup kk, sure"
	    "well I'm glad we sorted that out"))
    ("orly" ("yarly"
	     "btard"))
	  
;; sarcastic stuff
    ("\\w+ (is|are) (fat|big|hot|awesome|win|sexy|ugly)" ("your mum is $1"))
    ("(\\w+) (is|are) here" ("oh great. I love $0"
			     "$0 is annoying"
			     "well since $0 $1 here, maybe you could leave"))
    ("(tard|ass)" ("no, you're the $0"))
    ("die" ("no, you die"))
    ("you suck" ("your mum sucks"))
    ("phrik" ("screw phrik"))))

				   
(defun respond-to (string)
  (respond string *eliza-responses*))


(defcommand - ("(.+)" string)
  (respond-to string))