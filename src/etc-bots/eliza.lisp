;;;; Copyright 2009 Kat Marchan
;;;;
;;;; This file is part of sykobot.
;;;;
;;;; For licensing and warranty information, refer to COPYING
;;;;
;;;; translation of http://www.strout.net/python/ai/therapist.py
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package :sykobot)

(defun translate (string alist)
  "take a string and replace any (key, value) in alist"
  (let ((words (split "\\s+" string)))
    (loop for word in words
          for y from 0
       when (alref word alist)
	 do (setf (elt words y) (alref word alist)))
    (format nil "~{~A~^ ~}" words)))

(defun substitute-groups (string groups)
  "take a string containing $1 $2 etc and substitute with the coresponding element"
  (let ((words (split "\\s+" string)))
    (loop for word in words
       for y from 0
       when (equal (char word 0) #\$)
       do (let ((following (cadr (split "\\d+" word)))
		(ele (parse-integer (scan-to-strings "\\d+" word))))
	    (when (and ele (< ele (length groups)))
	      (setf (elt words y) (if (case-specifier-p (char word 1))
				      (reflect (elt groups ele)
					       (char word 1))
				      (reflect (elt groups ele))))
	      (if following (setf (elt words y) (merge-strings ""
							       (elt words y)
							       following))))))
	    
    (format nil "~{~A~^ ~}" words)))

(defparameter *reflections* '(("n" . (("am" . "are") ("are" . "am")
				     ("i" . "you") ("you" . "I")
;;				     ("i'd" . "you would") ("you would" . "i'd")
;;				     ("i've" . "you have") ("you have" . "i'd")
;;				     ("i'll" . "you will") ("you will" . "i'll")
				     ("my" . "your") ("your" . "my")
				     ("mine" . "yours") ("yours" . "mine")
				     ("me" . "you") ("you" . "me")))
			      ("a" . (("you" . "me")))))

(defun case-specifier-p (char)
  (member char '(#\n #\a)))

(defun reflect (string &optional (in-case #\n))
  (format nil "~{~A~^ ~}" (mapcar (lambda (w) (reflect-word w in-case)) (split "\\s+" string))))

(defun reflect-word (word &optional (in-case #\n))
  (unless (case-specifier-p in-case)
    (error "that isn't a grammatical case"))
  (cond
    ((alref word (alref in-case *reflections*)))
    ((equal in-case #\a) (reflect-word word))
    (t word)))
      

(defparameter *preprocessings* '(("i'll" . "I will") ("i'd" . "I would")
				 ("you'd" . "you would") ("you'll" . "you will")
				 ("he'd" . "he would") ("he'll" . "he will")
				 ("she'd" . "she would") ("she'll" . "she will")
				 ("it'd" . "it would") ("it'll" . "it will")
				 ("we'd" . "we would") ("we'll" . "we will")
				 ("they'd" . "they would") ("they'll" . "they will")
				 ("i've" . "I have") ("i'm" . "I am")
				 ("ima" . "I am going to")
				 ("he's" . "he is") ("she's" . "she is") 
				 ("it's" . "it is")))
(defun preprocess (string)
  (translate string *preprocessings*))

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
    ("I need (.+)" ("Why do you need $a0?"
		    "Would it really help to get $a0?"))
    ("I dreamt (that|about|)(.+)" ("You dream $0 $1 often then?"
				   "I dream $0 $1 too!"))
    ("sorry" ("please don't apologise" "yeah, I'm sorry too"
	      "no worries"))
    ("do you remember (.+)" ("How could I forget $0?"
			     "$0?!"
			     "How could I forget $0"
			     "Is $0 worth remembering"))

    ("(\\w+) (thinks*) (that )*(.+)" ("Why do I care what $0 $1?"
				      "Why should I care what $0 $1?"
				      "$0 $1 these kinda things often?"))

    ("if (.+) then (.+)" ("Do you really think it's likely that $0"
			  "Well, it sure would be awesome if $1"))
    ("my mother (.+)" ("Who else in your family $0 ?"
		       "Tell me more about your family"))
    ("I want (.+)" ("Why would anyone want $a0"
		    "Yes, that would be nice"))
    ("I like (.+)" ("I like $a0 too!"))
    ("I demand (.+)" ("oh, you *demand* do you?"))
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
    ;; questions
    ("how (was|is|are|were) ([\\w ]+) (\\w+ing)" ("$1 $0 $2 great"
						  "$1 $0 $2 meh"
						  "$1 $0 $2 ok I guess"))

    ("\\w+ (is|are) (fat|big|hot|awesome|win|sexy|ugly)" ("your mum is $1"))
    ("(\\w+) (is|are) here" ("oh great. I love $0"
			     "$0 is annoying"
			     "well since $0 $1 here, maybe you could leave"))

    ("how (was|is|are|were) ([\\w ]+)" ("$1 $0 great"))
    ("(how|what|where|when|why|who) (\\w+) you (.+)"
     ("well, $0 $1 YOU $2?"))
    ("(how|what|where|when|why|who) (.+)" ("$0 indeed"
					   "I ask myself the same question"))
    ("can (\\w+) (.+)" ("I dunno. Can $0 $a1?"))
    ("\\w+ (is|are|were|was) (.+)" ("correct"))



    ("(tard|ass)" ("no, you're the $0"))
    ("die" ("no, you die"))
    ("you suck" ("your mum sucks"))
    ("phrik" ("screw phrik"))
    ("fag" ("you're the fag"))
    ("bot" ("hmm? bot? No, I am no bot"
	    "I don't see any bots"))

    ;; clutch at straws

    ("interesting" ("well, YOU are not interesting"))
    ("hate" ("...and I hate you"))
    ("^\\s*because (.+)" ("oh, well now it makes sense"))

    ;; fine, give up, DEFLECT IT
    ("(.*)" ("Why do you say that $0\?" "What do you mean when you say that $0\?"
	     "What are your motives behind saying such a thing\?"
	     "I think it's clear that you're full of shit when you say that $0."))))

(defun respond-to (string)
  (respond (remove #\? string) *eliza-responses*))

(defcommand - ("(.+)" string)
  (respond-to (preprocess string)))

(defcommand reflect ("(.+)" string)
  "swaps 1st and 2nd person in the accusative case"
  (reflect string))

(defcommand preprocess ("(.+)" string)
  "preprocesses the string - i.e. normalise abbreviations"
  (preprocess string))