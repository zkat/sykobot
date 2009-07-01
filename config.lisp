(in-package :sykobot)

(defun config-exists-p ()
  (if (probe-file (merge-pathnames ".sykobotrc" (user-homedir-pathname)))
      t nil))

(defun run-sykobot ()
  (handler-bind ((cl-irc:no-such-reply (lambda (c)
                                         (let ((r (find-restart 'continue c)))
                                           (when r (invoke-restart r))))))
    (if (config-exists-p)
        (load (merge-pathnames ".sykobotrc" (user-homedir-pathname)))
        (run-bot #@sykobot "irc.freenode.net"))))

