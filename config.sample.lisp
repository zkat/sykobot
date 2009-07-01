;;; Any code in file "config.lisp" will be loaded after all
;;; the other files have loaded.
;;;
;;; Sample code:

(in-package :sykobot)
(defparameter *my-bot* (clone #@sykobot))
(setf (nickname *my-bot*) "mybotname")