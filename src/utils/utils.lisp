(in-package :sykobot)

;; general-purpose util functions/macros/etc go here

(defun hash-table-keys (table) (loop for key being the hash-keys of table collect key))
(defun random-elt (sequence) (elt sequence (random (length sequence))))