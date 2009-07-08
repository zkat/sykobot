(in-package :sykobot)

;; general-purpose util functions/macros/etc go here

(defun random-elt (sequence) 
  (let ((l (length sequence)))
    (if (zerop l)
	nil
	(elt sequence (random (length sequence))))))

(defun hash-table-keys (table)
  (loop for key being the hash-keys of table collect key))

;;; NOTE: Don't use '\' in the separator, it fails.
(defun merge-strings (separator &rest strings)
  (format nil (format nil "~~{~~A~~^~A~~}"
                      (regex-replace-all "~" separator "~~"))
          strings))

(defun flatten (x)
  "Flattens a list."
  (labels ((rec (x acc)
	     (cond ((null x) acc)
		   ((atom x) (cons x acc))
		   (t (rec (car x) (rec (cdr x) acc))))))
    (rec x nil)))

