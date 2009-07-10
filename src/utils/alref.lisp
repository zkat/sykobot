;;;; Copyright 2009 Adlai Chandrasekhar
;;;;
;;;; This file, while part of sykobot, is derived from Adlai's ALREF
;;;;   project, available via git from:
;;;      <git://github.com/adlai/ALREF.git>
;;;;
;;;; For licensing and warranty information, please refer to the
;;;;   project site, at <http://www.github.com/adlai/ALREF/>.
;;;;
;;;; This version, because it's just for use within this one
;;;;   project, lacks some of the features (both convenience
;;;;   and bug-safety) that the full utility has.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :sykobot)

(defmacro with-gensyms (vars &body body)
  `(let ,(loop for x in vars collect `(,x (gensym)))
     ,@body))
(defun alref (item alist &key
              (test #'string-equal)
              (key #'identity) default)
  "Retreive the value corresponding to ITEM in ALIST."
  (or (cdr (assoc item alist :test test :key key))
      default))
(define-setf-expander alref (item alist
                             &key (test '#'string-equal)
                                  (key '#'identity)
                             &environment env)
  "Set the value corresponding to ITEM in ALIST."
  (multiple-value-bind (orig-temps orig-vals stores setter)
      (get-setf-expansion alist env)
    (with-gensyms (it g-item g-alist g-test g-key new)
      (values (append (list g-item g-alist g-test g-key it)
                      orig-temps)
              (append (list item alist test key
                            `(assoc ,item ,alist :test ,test :key ,key))
                      orig-vals)
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
