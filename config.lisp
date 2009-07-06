(in-package :sykobot)

(defvar *default-channels* nil)
(defvar *server* nil)
(defvar *identify-with-nickserv?* nil)
(defvar *nickserv-password* nil)
(defvar *nickname* nil)
(defvar *init-file*
  (merge-pathnames ".sykobotrc" (user-homedir-pathname)))

(defun run-bot (&optional (bot-prototype (proto 'sykobot)))
  (let ((bot (clone bot-prototype)))
    (handler-bind ((cl-irc:no-such-reply (lambda (c)
                                           (let ((r (find-restart 'continue c)))
                                             (when r (invoke-restart r))))))
      (apply #'activate-listeners bot '(command-listener send-memos scan-for-fact
                                        scan-for-url scan-for-more))
      (when (probe-file *init-file*)
        (handler-case (load *init-file*)
          (end-of-file () (error "You missed a paren somewhere"))))
      (when *nickname* (setf (nickname bot) *nickname*))
      (when *server*   (setf (server   bot) *server*))
      (load-memos bot)
      (load-facts bot)
      (init-bot bot)
      (when *identify-with-nickserv?*
        (identify bot *nickserv-password*))
      (dolist (channel *default-channels* bot)
        (join bot channel)))))
