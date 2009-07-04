;;;; This is a stripped-down version of Adlai's ALREF Utility
;;;; If you want to use the code in this file, please consider
;;;;   using the full utility, available via git from:
;;;;     <git://github.com/adlai/ALREF.git>
;;;; This version, because it's just for use within this one
;;;;   project, lacks some of the features (both convenience
;;;;   and bug-safety) that the full utility has.

(in-package :sykobot)

(defvar *default-alref-test* #'eql)
(defvar *default-alref-value* NIL)
(defmacro with-gensyms (vars &body body)
  `(let ,(loop for x in vars collect `(,x (gensym)))
     ,@body))
(defun alref (item alist &key
              (test *default-alref-test*)
              (key #'identity)
              (default *default-alref-value*))
  "Retreive the value corresponding to ITEM in ALIST."
  (or (cdr (assoc item alist :test test :key key))
      default))
(define-setf-expander alref (item alist
                             &key (test *default-alref-test*)
                                  (key #'identity)
                             &environment env)
  "Set the value corresponding to ITEM in ALIST."
  (multiple-value-bind (foo bar stores setter)
      (get-setf-expansion alist env)
    (declare (ignore foo bar))
    (with-gensyms (it g-item g-alist g-test g-key new)
      (values (list g-item g-alist g-test g-key it)
              (list item alist test key
                    `(assoc ,item ,alist :test ,test :key ,key))
              `(,new)
              `(cond ((eq ,new NIL)
                      (let ((,(car stores)
                             (delete ,g-item ,g-alist :test ,g-test
                                     :count 1 :key
                                     #'(lambda (pair)
                                         (funcall ,g-key
                                                  (car pair))))))
                        ,setter))
                     (,it (setf (cdr ,it) ,new))
                     (T (let ((,(car stores)
                               (acons ,g-item ,new ,g-alist)))
                          ,setter)))
              `(cdr ,it)))))
