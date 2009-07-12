;;;; Copyright 2009 Kat Marchan
;;;;
;;;; This file is part of sykobot.
;;;;
;;;; For licensing and warranty information, refer to COPYING
;;;;
;;;; code in this file is largely based on clikibot's chant routine.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package :sykobot)

;; general-purpose util functions/macros/etc go here

(defun build-string (&rest strings)
  (when (and (stringp (car strings))
	     (string-equal (car strings) "~A"))
    (error "BUILD-STRING is being called like FORMAT.~@
            Please rewrite this call and join the sensation."))
  (if (cadr strings)
      (apply #'format nil strings)
      (funcall #'format nil "~A"
	       (or (car strings)
		   ""))))

(defmacro do-lines ((var stream &optional result) &body body)
  `(loop for ,var = (read-line ,stream nil)
      while ,var do ,@body
      finally (return ,result)))

(defun random-elt (sequence)
  (let ((l (length sequence)))
    (if (zerop l)
        nil
        (elt sequence (random (length sequence))))))

(defun hash-table-keys (table)
  (loop for key being the hash-keys of table collect key))

;;; NOTE: Don't use '\' in the separator, it fails.
(defun merge-strings (separator &rest strings)
  (build-string (build-string "~~{~~A~~^~A~~}"
			      (regex-replace-all "~" separator "~~"))
		strings))

(defun flatten (x)
  "Flattens a list."
  (labels ((rec (x acc)
             (cond ((null x) acc)
                   ((atom x) (cons x acc))
                   (t (rec (car x) (rec (cdr x) acc))))))
    (rec x nil)))

