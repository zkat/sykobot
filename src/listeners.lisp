;;;; Copyright 2009 Kat Marchan
;;;;
;;;; This file is part of sykobot.
;;;;
;;;; For licensing and warranty information, refer to COPYING
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package :sykobot)

(defmessage activate-listener (bot name))
(defmessage deactivate-listener (bot name))
(defmessage call-all-listeners (bot sender channel message))
(defmessage listener-active-p (bot name))

(defreply activate-listener ((bot (proto 'sykobot)) name)
  (pushnew name (active-listeners bot)))
(defreply deactivate-listener ((bot (proto 'sykobot)) name)
  (with-properties (active-listeners) bot
    (setf active-listeners (delete name active-listeners))))

(defreply call-all-listeners ((bot (proto 'sykobot)) sender channel message)
  (loop for name in (active-listeners bot)
     do (call-listener bot name sender channel message)))

(defreply listener-active-p ((bot (proto 'sykobot)) name)
  (if (member name (active-listeners bot))
      t nil))

(defun activate-listeners (bot &rest listener-names)
  (loop for listener in listener-names
       do (activate-listener bot listener)))

;;; Listeners
(let ((listener-table (make-hash-table :test #'eq)))

  (defun add-listener (name function)
    (assert (symbolp name))
    (setf (gethash name listener-table) function))

  (defun remove-listener (name)
    (remhash name listener-table))

  (defun listener-function (name)
    (multiple-value-bind (fn hasp)
        (gethash name listener-table)
      (if hasp
          fn
          (lambda (bot sender channel message)
            (declare (ignore bot sender channel message))
            (print "listener doesn't exist")))))

  (defun call-listener (bot name sender channel message)
    (funcall (listener-function name) bot sender channel message))

  (defun get-listener-table ()
    listener-table)

  ) ;end listener table

(defmacro deflistener (name &body body)
  `(add-listener ',name
                (lambda (*bot* *sender* *channel* *message*)
                  (declare (ignorable *bot* *sender* *channel* *message*))
                  ,@body)))

