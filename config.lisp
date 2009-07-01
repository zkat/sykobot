(in-package :sykobot)

(export '(*default-channels* *server* *identify-with-nickserv?* *nickserv-password* *nickname*))
(defvar *default-channels* nil)
(defvar *server* nil)
(defvar *identify-with-nickserv?* nil)
(defvar *nickserv-password* nil)
(defvar *nickname* nil)

(defun config-exists-p ()
  (if (probe-file (merge-pathnames ".sykobotrc" (user-homedir-pathname)))
      t nil))

(defun load-rc-file ()
  (load (merge-pathnames ".sykobotrc" (user-homedir-pathname))))

(defun run-sykobot ()
  (handler-bind ((cl-irc:no-such-reply (lambda (c)
                                         (let ((r (find-restart 'continue c)))
                                           (when r (invoke-restart r))))))
    (when (config-exists-p)
      (load (merge-pathnames ".sykobotrc" (user-homedir-pathname))))
    (when *nickname*
      (setf (nickname (proto 'sykobot)) *nickname*))
    (when *server*
      (setf (server (proto 'sykobot)) *server*))
    (run-bot (proto 'sykobot))
    (when *identify-with-nickserv?*
      (identify (proto 'sykobot) *nickserv-password*))
    (loop for channel in *default-channels*
       do (join (proto 'sykobot) channel))))



