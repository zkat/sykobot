(in-package :sykobot)

(defvar *default-channels* nil)
(defvar *server* nil)
(defvar *port* nil)
(defvar *identify-with-nickserv?* nil)
(defvar *nickserv-password* nil)
(defvar *nickname* nil)
(defvar *username* nil)
(defvar *realname* nil)
(defvar *bot-dir* nil)
(defvar *default-listeners* '(command-listener send-memos scan-for-fact scan-for-more scan-for-url))

(defvar *home* (merge-pathnames ".sykobot/" (user-homedir-pathname)))

(defmessage bot-dir (bot))
(defreply bot-dir ((bot (proto 'sykobot)))
  (ensure-directories-exist (merge-pathnames (dir bot) *home*)))

(defun run-bot (&optional (bot-prototype (proto 'sykobot)))
  (let ((bot (clone bot-prototype)))
    (handler-bind ((cl-irc:no-such-reply (lambda (c)
                                           (let ((r (find-restart 'continue c)))
                                             (when r (invoke-restart r))))))
      (when *bot-dir*
        (setf (dir bot) *bot-dir*))
      (let ((init-file (merge-pathnames "init" (bot-dir bot))))
        (when (probe-file init-file)
          (handler-case (load init-file)
            (end-of-file () (error "You missed a paren somewhere")))))
      (apply #'activate-listeners bot *default-listeners*)
      (when *nickname*
        (setf (nickname bot) *nickname*))
      (when *username*
        (setf (username bot) *username*))
      (when *realname*
        (setf (realname bot) *realname*))
      (when *server*
        (setf (server bot) *server*))
      (when *port*
        (setf (port bot) *port*))
      (load-memos bot)
      (load-facts bot)
      (init-bot bot)
      (when *identify-with-nickserv?*
        (identify bot *nickserv-password*))
      (dolist (channel *default-channels* bot)
        (join bot channel)))))
