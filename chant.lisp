(in-package :sykobot)

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

(defvar *more* "lulz")

(add-command "chant" (lambda (bot args sender channel)
                      (send-msg bot channel (format nil "MORE ~:@(~A~)" *more*))))

(defun scan-for-more (s)
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
              (and str (setf *more* (string-upcase (elt str 0)))))))))))))

