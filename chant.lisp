;;; The code in this file is based on the chant routines
;;;   of 'minon' on #lisp.
(in-package :sykobot)

;;; These variables assist in parsing messages
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

;;; This variable stores the current 'fad'.
;;; It is an alist, because each channel that the bot connects to
;;;  could have a different fad.
(defvar *more* NIL)

;;; These customizations help with the use of :ALREF
(setf *default-alref-test* #'string-equal
      *default-alref-value* "MONEY")

;;; This command is used to chant.
(defcommand "chant"
  (send-msg *bot* *channel*
            (format nil "MORE ~:@(~A~)"
                    (alref *channel* *more*))))

;;; This is where the cookie crumbles.
;;;
;;; More detailed documentation coming soon; essentially, each
;;;   regexp scans an increasingly general search, trying to
;;;   find the current 'fad'. The boolean functions essentially
;;;   ensure that once a match is found, the function will exit.
;;;
;;; Because the scans are arranged in decreasing specificity, this
;;;   means that the most specific 'fad' will be cached.
;;;
;;; This scan function is fucked. I've started going over the regexps, and
;;;   I can tell that they're supposed to do SOMETHING, but they don't.
(defun scan-for-more (s channel)
  (let ((str (nth-value
              1 (scan-to-strings "[MORE|MOAR]\\W+((\\W|[A-Z0-9])+)([A-Z0-9])($|[^A-Z0-9])" s))))
    (or
     (and str
          (setf (alref channel *more*)
                (concatenate 'string (elt str 0) (elt str 2))))
     (let ((str (nth-value 1 (scan-to-strings "(?i)[more|moar]\\W+(\\w+)\\W+(\\w+)\\W+(\\w+)" s))))
       (or
        (and str
             (or (member (elt str 0) *prepositions* :test #'string-equal)
                 (member (elt str 0) *conjunctions* :test #'string-equal)
                 (member (elt str 0) *articles* :test #'string-equal))
             (or (member (elt str 1) *prepositions* :test #'string-equal)
                 (member (elt str 1) *conjunctions* :test #'string-equal)
                 (member (elt str 1) *articles* :test #'string-equal))
             (setf (alref channel *more*)
                   (string-upcase (concatenate 'string (elt str 0)
                                               " " (elt str 1)
                                               " " (elt str 2)))))
        (let ((str (nth-value 1 (scan-to-strings "(?i)[more|moar]\\W+(\\w+)\\W+(\\w+)" s))))
          (or
           (and str
                (or (member (elt str 0) *prepositions* :test #'string-equal)
                    (member (elt str 0) *conjunctions* :test #'string-equal)
                    (member (elt str 0) *articles* :test #'string-equal))
                (setf (alref channel *more*)
                      (string-upcase (concatenate 'string (elt str 0)
                                                  " " (elt str 1)))))
           (let ((str (nth-value 1 (scan-to-strings "(?i)[more|moar]\\W+(\\w+)" s))))
             (and str
                  (setf (alref channel *more*)
                        (string-upcase (elt str 0))))))))))))

