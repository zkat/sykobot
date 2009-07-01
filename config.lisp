(in-package :sykobot)

(defun config-exists-p ()
  (if (probe-file (merge-pathnames ".sykobotrc" (user-homedir-pathname)))
      t nil))

(defun run-sykobot (&optional (server "irc.freenode.net"))
  (handler-bind ((cl-irc:no-such-reply (lambda (c)
                                         (let ((r (find-restart 'continue c)))
                                           (when r (invoke-restart r))))))
    (if (config-exists-p)
        (load (merge-pathnames ".sykobotrc" (user-homedir-pathname)))
        (progn (setf (server #@sykobot) server)
               (run-bot #@sykobot)))))

