(in-package :sykobot)

;;; Listeners
(let ((listener-table (make-hash-table :test #'eq))
      (active-listeners ()))
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

  (defun activate-listener (name)
    (pushnew name active-listeners :test #'equalp))

  (defun deactivate-listener (name)
    (setf active-listeners (remove name active-listeners :test #'equalp)))

  (defun call-listener (name bot sender channel message)
    (funcall (listener-function name) bot sender channel message))

  (defun call-listeners (bot sender channel message)
    (loop for name in active-listeners
       do (call-listener name bot sender channel message)))

  (defun get-listener-table ()
    listener-table)

  (defun get-active-listeners ()
    active-listeners))

(defmacro deflistener (name &body body)
  `(add-listener ',name
                (lambda (*bot* *sender* *channel* *message*)
                  (declare (ignorable *bot* *sender* *channel* *message*))
                  ,@body)))

